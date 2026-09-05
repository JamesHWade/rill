# Reading never waits for acquisition. The current session holds this immutable
# copy until the Reader explicitly loads a newer one or opens another story.
reading_document <- function(store, reader_id, entry) {
  span <- telemetry_local_span("article.reading_copy")
  current <- telemetry_span(
    "store.reading_copy.lookup",
    store_get_document(store, reader_id, entry$entry_id)
  )
  telemetry_attributes(span, list("copy.cached" = !is.null(current)))
  if (!is.null(current)) {
    return(current)
  }
  fallback <- telemetry_span("article.feed_copy", document_fallback(entry))
  telemetry_span(
    "store.reading_copy.save",
    store_save_document_if_missing_head(store, reader_id, fallback)$document
  )
}

public_reading_document <- function(store, entry_id) {
  if (identical(store$mode, "postgres")) {
    rows <- DBI::dbGetQuery(
      store$pool,
      "SELECT document_id FROM public_document_heads WHERE entry_id = $1",
      params = list(entry_id)
    )
    id <- if (nrow(rows)) rows$document_id[[1]] else NULL
  } else {
    id <- unname(store$memory$document_heads[entry_id])
    if (!length(id) || is.na(id)) id <- NULL
  }
  if (is.null(id)) NULL else store_get_document_record(store, id)
}

full_reading_document <- function(document) {
  !is.null(document) && !identical(document$acquisition_method, "feed_fallback")
}

# Only public subscription entries are eligible; a worker receives no Reader
# state, private captures, authentication configuration, or database credentials
# in its serialized request.
preparation_entry <- function(store, entry_id, reader_id = NULL) {
  if (identical(store$mode, "postgres")) {
    rows <- DBI::dbGetQuery(
      store$pool,
      paste(
        "SELECT e.*, f.title AS feed_title FROM entries e",
        "JOIN feeds f ON f.feed_id = e.feed_id",
        "WHERE e.entry_id = $1 AND f.source_kind = 'subscription'",
        "AND EXISTS (SELECT 1 FROM subscriptions s",
        "JOIN readers r ON r.reader_id = s.reader_id",
        "WHERE s.feed_id = e.feed_id AND s.status = 'active'",
        "AND r.status = 'active' AND ($2::text IS NULL OR s.reader_id = $2))"
      ),
      params = list(entry_id, reader_id %||% NA_character_)
    )
  } else {
    subscriptions <- store$memory$subscriptions
    readers <- store$memory$readers
    eligible <- subscriptions$status == "active" &
      subscriptions$reader_id %in% readers$reader_id[readers$status == "active"]
    if (!is.null(reader_id)) {
      eligible <- eligible & subscriptions$reader_id == reader_id
    }
    feeds <- store$memory$feeds
    ids <- intersect(
      subscriptions$feed_id[eligible],
      feeds$feed_id[feeds$source_kind == "subscription"]
    )
    entries <- store$memory$entries
    rows <- entries[
      entries$entry_id == entry_id & entries$feed_id %in% ids,
      ,
      drop = FALSE
    ]
    if (nrow(rows)) {
      rows$feed_title <- feeds$title[match(rows$feed_id, feeds$feed_id)]
    }
  }
  if (!nrow(rows)) NULL else as.list(rows[1, , drop = FALSE])
}

preparation_candidates <- function(
  store,
  reader_id = NULL,
  limit = 100L,
  now = Sys.time()
) {
  since <- now - 7 * 86400
  if (identical(store$mode, "postgres")) {
    rows <- DBI::dbGetQuery(
      store$pool,
      paste(
        "SELECT e.entry_id FROM entries e JOIN feeds f ON f.feed_id = e.feed_id",
        "LEFT JOIN public_document_heads h ON h.entry_id = e.entry_id",
        "LEFT JOIN documents d ON d.document_id = h.document_id",
        "LEFT JOIN article_preparations p ON p.entry_id = e.entry_id",
        "WHERE f.source_kind = 'subscription'",
        "AND COALESCE(e.published_at, e.inserted_at) >= $1::timestamptz",
        "AND (d.document_id IS NULL OR d.acquisition_method = 'feed_fallback')",
        "AND (p.entry_id IS NULL OR (p.next_attempt_at <= $2::timestamptz AND p.attempts < 5))",
        "AND EXISTS (SELECT 1 FROM subscriptions s JOIN readers r ON r.reader_id = s.reader_id",
        "WHERE s.feed_id = e.feed_id AND s.status = 'active' AND r.status = 'active'",
        "AND ($3::text IS NULL OR s.reader_id = $3))",
        "ORDER BY COALESCE(e.published_at, e.inserted_at) DESC, e.entry_id LIMIT $4"
      ),
      params = list(since, now, reader_id %||% NA_character_, limit)
    )
    return(rows$entry_id)
  }
  entries <- store$memory$entries
  dates <- as.POSIXct(
    ifelse(
      is.na(entries$published_at),
      entries$inserted_at,
      entries$published_at
    ),
    tz = "UTC"
  )
  ids <- entries$entry_id[order(dates, decreasing = TRUE)]
  ids <- ids[
    ids %in% entries$entry_id[!is.na(dates) & dates >= now - 7 * 86400]
  ]
  utils::head(
    Filter(
      function(id) {
        attempt <- preparation_attempt(store, id)
        !is.null(preparation_entry(store, id, reader_id)) &&
          !full_reading_document(public_reading_document(store, id)) &&
          (is.null(attempt) ||
            (as.POSIXct(attempt$next_attempt_at, tz = "UTC") <= now &&
              attempt$attempts < 5L))
      },
      ids
    ),
    limit
  )
}

preparation_attempt <- function(store, entry_id) {
  if (identical(store$mode, "postgres")) {
    rows <- DBI::dbGetQuery(
      store$pool,
      "SELECT * FROM article_preparations WHERE entry_id = $1",
      params = list(entry_id)
    )
    if (!nrow(rows)) {
      return(NULL)
    }
    result <- as.list(rows[1, , drop = FALSE])
    result$failure <- if (is.na(result$failure)) {
      NULL
    } else {
      jsonlite::fromJSON(result$failure, simplifyVector = FALSE)
    }
    return(result)
  }
  store$memory$article_preparations[[entry_id]]
}

claim_preparation <- function(
  store,
  entry_id,
  retry = FALSE,
  now = Sys.time()
) {
  token <- rill_id("preparation", entry_id, utc_now(), stats::runif(1))
  until <- now + 300
  if (identical(store$mode, "postgres")) {
    rows <- DBI::dbGetQuery(
      store$pool,
      paste(
        "INSERT INTO article_preparations (entry_id, token, attempts, status, next_attempt_at)",
        "VALUES ($1, $2, 1, 'running', $3)",
        "ON CONFLICT (entry_id) DO UPDATE SET token = EXCLUDED.token,",
        "attempts = CASE WHEN $5 AND article_preparations.attempts >= 5 THEN 1 ELSE article_preparations.attempts + 1 END,",
        "status = 'running', next_attempt_at = EXCLUDED.next_attempt_at, failure = NULL",
        "WHERE article_preparations.next_attempt_at <= $4",
        "AND (article_preparations.attempts < 5 OR $5) RETURNING token"
      ),
      params = list(entry_id, token, until, now, retry)
    )
    return(if (nrow(rows)) token else NULL)
  }
  old <- preparation_attempt(store, entry_id)
  if (
    !is.null(old) &&
      (as.POSIXct(old$next_attempt_at, tz = "UTC") > now ||
        (old$attempts >= 5L && !retry))
  ) {
    return(NULL)
  }
  store$memory$article_preparations[[entry_id]] <- list(
    entry_id = entry_id,
    token = token,
    attempts = if (is.null(old) || (retry && old$attempts >= 5L)) {
      1L
    } else {
      old$attempts + 1L
    },
    status = "running",
    next_attempt_at = until,
    failure = NULL
  )
  token
}

finish_preparation <- function(
  store,
  entry_id,
  token,
  document = NULL,
  failure = NULL,
  now = Sys.time()
) {
  finish <- function(transaction_store) {
    if (identical(transaction_store$mode, "postgres")) {
      DBI::dbGetQuery(
        transaction_store$pool,
        "SELECT entry_id FROM entries WHERE entry_id = $1 FOR UPDATE",
        params = list(entry_id)
      )
      DBI::dbGetQuery(
        transaction_store$pool,
        "SELECT entry_id FROM article_preparations WHERE entry_id = $1 FOR UPDATE",
        params = list(entry_id)
      )
    }
    attempt <- preparation_attempt(transaction_store, entry_id)
    if (
      is.null(attempt) ||
        !identical(attempt$token, token) ||
        !identical(attempt$status, "running")
    ) {
      return(FALSE)
    }
    # Recheck eligibility after the fetch, and never regress a concurrently
    # prepared head or modify a Reader's explicit copy selection.
    if (
      !is.null(document) &&
        !is.null(preparation_entry(transaction_store, entry_id)) &&
        !full_reading_document(public_reading_document(
          transaction_store,
          entry_id
        ))
    ) {
      if (identical(transaction_store$mode, "postgres")) {
        store_insert_document(transaction_store$pool, document)
        DBI::dbExecute(
          transaction_store$pool,
          paste(
            "INSERT INTO public_document_heads (entry_id, document_id, ownership_key, selected_at)",
            "VALUES ($1, $2, 'public', $3) ON CONFLICT (entry_id) DO UPDATE SET",
            "document_id = EXCLUDED.document_id, selected_at = EXCLUDED.selected_at"
          ),
          params = list(entry_id, document$document_id, document$received_at)
        )
      } else {
        store_save_document(transaction_store, document)
        transaction_store$memory$document_heads[[
          entry_id
        ]] <- document$document_id
      }
    }
    status <- if (is.null(failure)) "succeeded" else "failed"
    next_at <- now + min(86400, 300 * 2^(attempt$attempts - 1L))
    if (identical(transaction_store$mode, "postgres")) {
      DBI::dbExecute(
        transaction_store$pool,
        "UPDATE article_preparations SET status = $2, next_attempt_at = $3, failure = $4::jsonb WHERE entry_id = $1",
        params = list(
          entry_id,
          status,
          next_at,
          if (is.null(failure)) {
            NA_character_
          } else {
            jsonlite::toJSON(failure, auto_unbox = TRUE)
          }
        )
      )
    } else {
      attempt$status <- status
      attempt$next_attempt_at <- next_at
      attempt$failure <- failure
      transaction_store$memory$article_preparations[[entry_id]] <- attempt
    }
    TRUE
  }
  if (identical(store$mode, "postgres")) {
    return(pool::poolWithTransaction(store$pool, function(connection) {
      finish(structure(
        list(mode = "postgres", pool = connection),
        class = "rill_store"
      ))
    }))
  }
  finish(store)
}

extract_preparation <- function(entry, config) {
  tryCatch(
    list(document = document_from_defuddle(entry, config)),
    error = function(error) {
      failure <- preparation_failure(error, "extraction", config, emit = FALSE)
      failure$title <- entry$title
      list(failure = failure)
    }
  )
}

start_article_preparation <- function(
  store,
  config,
  reader_id,
  entry_id,
  retry = FALSE
) {
  span <- telemetry_start("article.prepare")
  telemetry_activate(span)
  handed_off <- FALSE
  status <- "unset"
  ending_attributes <- list()
  on.exit(
    if (!handed_off) telemetry_end(span, status, ending_attributes),
    add = TRUE
  )
  entry <- telemetry_span(
    "store.preparation.entry",
    preparation_entry(store, entry_id, reader_id)
  )
  if (
    is.null(entry) ||
      full_reading_document(public_reading_document(store, entry_id))
  ) {
    return(NULL)
  }
  token <- telemetry_span(
    "store.preparation.claim",
    claim_preparation(store, entry_id, retry)
  )
  if (is.null(token)) {
    return(NULL)
  }
  directory <- tempfile("rill-article-preparation-")
  dir.create(directory, mode = "0700")
  package_path <- getNamespaceInfo(asNamespace("rill"), "path")
  # Whitelist only extraction settings, not the enclosing app configuration.
  settings <- config[intersect(
    names(config),
    c(
      "defuddle_backend",
      "defuddle_base_url",
      "defuddle_api_key",
      "defuddle_command"
    )
  )]
  process <- tryCatch(
    telemetry_span(
      "article.worker.launch",
      launch_article_preparation(entry, settings, directory, package_path)
    ),
    error = \(error) error
  )
  if (inherits(process, "error")) {
    unlink(directory, recursive = TRUE)
    failure <- preparation_failure(process, "extraction", config)
    failure$title <- entry$title
    finish_preparation(store, entry_id, token, failure = failure)
    status <- "error"
    ending_attributes <- list(
      "failure.reference" = failure$reference,
      "failure.code" = failure$code
    )
    return(list(entry_id = entry_id, failure = failure))
  }
  handed_off <- TRUE
  list(
    span = span,
    end_span = telemetry_finalizer(span),
    process = process,
    directory = directory,
    entry_id = entry_id,
    token = token,
    started_at = Sys.time()
  )
}

launch_article_preparation <- function(entry, config, directory, package_path) {
  callr::r_bg(
    function(entry, config, package_path, trace_context, launched_at) {
      if (file.exists(file.path(package_path, "R", "article-preparation.R"))) {
        pkgload::load_all(
          package_path,
          export_all = FALSE,
          helpers = FALSE,
          quiet = TRUE
        )
      } else {
        loadNamespace("rill", lib.loc = dirname(package_path))
      }
      ns <- asNamespace("rill")
      on.exit(get("telemetry_flush", ns)(), add = TRUE)
      get("telemetry_span", ns)(
        "article.worker.extract",
        get("extract_preparation", ns)(entry, config),
        attributes = list(
          "worker.bootstrap_ms" = as.numeric(difftime(
            Sys.time(),
            launched_at,
            units = "secs"
          )) *
            1000
        ),
        parent = trace_context
      )
    },
    args = list(entry, config, package_path, telemetry_context(), Sys.time()),
    supervise = TRUE,
    user_profile = FALSE,
    stdout = file.path(directory, "stdout"),
    stderr = file.path(directory, "stderr")
  )
}

close_article_preparation <- function(
  job,
  status = "unset",
  attributes = list()
) {
  if (is.null(job)) {
    return(invisible(NULL))
  }
  if (job$process$is_alive()) {
    job$process$kill()
    if (is.null(attributes$preparation.outcome)) {
      attributes$preparation.outcome <- "cancelled"
    }
  }
  if (is.function(job$end_span)) {
    job$end_span(status, attributes)
  }
  unlink(job$directory, recursive = TRUE)
  invisible(NULL)
}

poll_article_preparation <- function(job, store, config, timeout = 120) {
  if (
    job$process$is_alive() &&
      as.numeric(difftime(Sys.time(), job$started_at, units = "secs")) < timeout
  ) {
    return(NULL)
  }
  telemetry_activate(job$span)
  status <- "unset"
  ending_attributes <- list()
  on.exit(close_article_preparation(job, status, ending_attributes), add = TRUE)
  result <- tryCatch(job$process$get_result(), error = function(error) {
    list(failure = preparation_failure(error, "extraction", config))
  })
  if (!is.null(result$failure)) {
    diagnostic <- result$failure[c(
      "reference",
      "stage",
      "code",
      "error_type",
      "http_status",
      "backend"
    )]
    telemetry_log("warn", "article.prepare_failed", diagnostic)
    message(
      "article.prepare_failed ",
      jsonlite::toJSON(diagnostic, auto_unbox = TRUE, na = "null")
    )
  }
  tryCatch(
    telemetry_span(
      "store.preparation.finish",
      finish_preparation(
        store,
        job$entry_id,
        job$token,
        result$document,
        result$failure
      )
    ),
    error = function(error) {
      result <<- list(failure = preparation_failure(error, "storage", config))
      try(
        finish_preparation(
          store,
          job$entry_id,
          job$token,
          failure = result$failure
        ),
        silent = TRUE
      )
    }
  )
  status <- if (is.null(result$failure)) "ok" else "error"
  ending_attributes <- list(
    "preparation.outcome" = if (is.null(result$failure)) {
      "ready"
    } else {
      "failed"
    },
    "failure.reference" = result$failure$reference,
    "failure.code" = result$failure$code,
    "http.response.status_code" = result$failure$http_status
  )
  result
}

# The scheduled poller is already a background process. Its bounded acquisition
# pass runs after the Feed polling transaction has released its lock.
prepare_recent_articles <- function(store, config, limit = 100L, budget = 600) {
  ids <- preparation_candidates(store, limit = limit)
  started <- Sys.time()
  prepared <- failed <- 0L
  for (id in ids) {
    if (as.numeric(difftime(Sys.time(), started, units = "secs")) >= budget) {
      break
    }
    job <- start_article_preparation(store, config, NULL, id)
    if (is.null(job)) {
      next
    }
    if (!is.null(job$failure)) {
      failed <- failed + 1L
      next
    }
    remaining <- max(
      0,
      budget - as.numeric(difftime(Sys.time(), started, units = "secs"))
    )
    job$process$wait(timeout = min(120, remaining) * 1000)
    result <- poll_article_preparation(job, store, config, timeout = 0)
    if (is.null(result$failure)) {
      prepared <- prepared + 1L
    } else {
      failed <- failed + 1L
    }
  }
  list(prepared = prepared, failed = failed)
}

article_preparation_controller <- function(
  store,
  config,
  reader_id,
  updated,
  idle = \() NULL
) {
  state <- new.env(parent = emptyenv())
  state$job <- NULL
  state$queue <- character()
  state$retry <- character()
  state$scheduled <- FALSE
  state$closed <- FALSE
  state$draining <- FALSE
  state$requested <- list()
  schedule <- function() {
    if (state$scheduled || state$closed) {
      return(invisible(NULL))
    }
    state$scheduled <- TRUE
    shiny::withReactiveDomain(NULL, later::later(poll, 0.25))
    invisible(NULL)
  }
  poll <- function() {
    state$scheduled <- FALSE
    if (state$closed) {
      return(invisible(NULL))
    }
    tryCatch(
      {
        if (!is.null(state$job)) {
          result <- poll_article_preparation(state$job, store, config)
          if (is.null(result)) {
            schedule()
            return(invisible(NULL))
          }
          id <- state$job$entry_id
          state$job <- NULL
          updated(id, result)
        }
        if (length(state$queue)) {
          id <- state$queue[[1]]
          state$queue <- state$queue[-1]
          retry <- id %in% state$retry
          state$retry <- setdiff(state$retry, id)
          request <- state$requested[[id]]
          state$requested[[id]] <- NULL
          started <- telemetry_span(
            "article.prepare.dispatch",
            start_article_preparation(store, config, reader_id, id, retry),
            attributes = list(
              "queue.wait_ms" = (proc.time()[[3L]] - request$at) * 1000
            ),
            parent = request$context
          )
          if (!is.null(started$failure)) {
            updated(id, started)
          } else {
            state$job <- started
            updated(id, NULL)
          }
        }
        if (!is.null(state$job) || length(state$queue)) schedule()
      },
      error = function(error) {
        close_article_preparation(state$job)
        state$job <- NULL
        state$queue <- character()
        state$requested <- list()
        updated(
          NULL,
          list(failure = preparation_failure(error, "storage", config))
        )
      }
    )
    if (state$draining && is.null(state$job) && !length(state$queue)) {
      state$draining <- FALSE
      idle()
    }
    invisible(NULL)
  }
  list(
    request = function(ids, retry = FALSE) {
      if (state$closed) {
        return(invisible(NULL))
      }
      state$queue <- utils::head(unique(c(ids, state$queue)), 500L)
      for (id in ids) {
        state$requested[[id]] <- list(
          at = proc.time()[[3L]],
          context = telemetry_context()
        )
      }
      state$requested <- state$requested[intersect(
        names(state$requested),
        state$queue
      )]
      state$draining <- TRUE
      if (retry) {
        state$retry <- unique(c(ids, state$retry))
      }
      state$retry <- intersect(state$retry, state$queue)
      schedule()
      invisible(NULL)
    },
    busy = \(id) id %in% c(state$queue, state$job$entry_id),
    poll = poll,
    state = state,
    close = function() {
      state$closed <- TRUE
      state$draining <- FALSE
      close_article_preparation(state$job)
      state$job <- NULL
      state$queue <- character()
      state$requested <- list()
      invisible(NULL)
    }
  )
}

article_preparation_status <- function(store, entry_id, busy = FALSE) {
  if (full_reading_document(public_reading_document(store, entry_id))) {
    return("ready")
  }
  if (busy) {
    return("running")
  }
  attempt <- preparation_attempt(store, entry_id)
  if (is.null(attempt)) {
    return("missing")
  }
  if (as.POSIXct(attempt$next_attempt_at, tz = "UTC") > Sys.time()) {
    return(if (identical(attempt$status, "running")) "running" else "waiting")
  }
  "missing"
}
