reader_memory_abort <- function(
  message = "This Reader Memory is unavailable."
) {
  cli::cli_abort(message, class = "rill_memory_unavailable")
}

reader_memory_access <- function(
  store,
  reader_id,
  authorize = function() TRUE
) {
  force(store)
  force(reader_id)
  force(authorize)
  function(operation) {
    if (!is.function(authorize) || !isTRUE(authorize())) {
      reader_memory_abort()
    }
    if (!store_scalar_string(reader_id)) {
      reader_memory_abort()
    }
    run <- function(current, artifacts) {
      operation(current, artifacts, reader_id)
    }
    if (identical(store$mode, "postgres")) {
      return(pool::poolWithTransaction(store$pool, function(connection) {
        reader <- DBI::dbGetQuery(
          connection,
          "SELECT status FROM readers WHERE reader_id = $1 FOR SHARE",
          params = list(reader_id)
        )
        if (
          nrow(reader) != 1L ||
            reader$status != "active" ||
            !isTRUE(authorize())
        ) {
          reader_memory_abort()
        }
        current <- store
        current$pool <- connection
        artifacts <- graft::graft_artifact_store_postgres(
          connection,
          paste0("rill:reader:", reader_id),
          create = TRUE,
          max_bytes = 1024^2
        )
        run(current, artifacts)
      }))
    }
    if (!identical(store_resolve_reader(store, reader_id)$status, "active")) {
      reader_memory_abort()
    }
    if (is.null(store$memory$reader_memory_path)) {
      store$memory$reader_memory_path <- tempfile("rill-demo-memory-")
      store$memory$reader_memory_index <- list()
    }
    path <- file.path(store$memory$reader_memory_path, rill_id(reader_id))
    artifacts <- graft::graft_artifact_store(
      path,
      create = !dir.exists(path),
      max_bytes = 1024^2
    )
    run(store, artifacts)
  }
}

reader_memory_ids <- function(store, reader_id, limit = 100L) {
  if (identical(store$mode, "postgres")) {
    return(
      DBI::dbGetQuery(
        store$pool,
        paste(
          "SELECT memory_id FROM reader_memory_index WHERE reader_id = $1",
          "ORDER BY created_at DESC, memory_id",
          if (!is.null(limit)) "LIMIT $2" else ""
        ),
        params = if (is.null(limit)) list(reader_id) else list(reader_id, limit)
      )$memory_id
    )
  }
  ids <- rev(store$memory$reader_memory_index[[reader_id]] %||% character())
  if (is.null(limit)) ids else utils::head(ids, limit)
}

reader_memory_require_id <- function(store, reader_id, memory_id) {
  if (!store_scalar_string(memory_id)) {
    reader_memory_abort()
  }
  exists <- if (identical(store$mode, "postgres")) {
    nrow(DBI::dbGetQuery(
      store$pool,
      "SELECT memory_id FROM reader_memory_index WHERE reader_id = $1 AND memory_id = $2",
      params = list(reader_id, memory_id)
    )) ==
      1L
  } else {
    memory_id %in%
      (store$memory$reader_memory_index[[reader_id]] %||% character())
  }
  if (!exists) reader_memory_abort()
}

reader_memory_propose <- function(
  access,
  text,
  kind = "preference",
  document_id = NULL,
  quote = NULL,
  memory_id = NULL,
  expected = NULL
) {
  force(text)
  force(kind)
  force(document_id)
  force(quote)
  force(memory_id)
  force(expected)
  if (
    !store_scalar_string(text) ||
      nchar(text, type = "bytes") > 8000L ||
      !is.character(kind) ||
      length(kind) != 1L ||
      !kind %in% c("preference", "interpretation")
  ) {
    reader_memory_abort(
      "Enter a preference or interpretation of at most 8,000 bytes."
    )
  }
  access(function(store, artifacts, reader_id) {
    if (is.null(memory_id)) {
      if (!is.null(expected)) {
        reader_memory_abort()
      }
      memory_id <- rill_id("memory", reader_id, utc_now(), stats::runif(1))
      expected <- NULL
    } else {
      reader_memory_require_id(store, reader_id, memory_id)
      current <- graft::graft_artifact_read_decision(artifacts, memory_id)
      if (
        !store_scalar_string(expected) ||
          is.null(current) ||
          !identical(expected, current$id)
      ) {
        reader_memory_abort()
      }
    }
    anchor <- NULL
    if (identical(kind, "interpretation")) {
      if (
        !store_scalar_string(document_id) ||
          !store_scalar_string(quote) ||
          nchar(quote, type = "bytes") > 16000L
      ) {
        reader_memory_abort(
          "An interpretation needs an exact passage from the reading copy."
        )
      }
      document <- store_get_document_by_id(store, reader_id, document_id)
      if (is.null(document)) {
        reader_memory_abort()
      }
      positions <- gregexpr(quote, document$markdown, fixed = TRUE)[[1L]]
      if (length(positions) != 1L || positions[[1L]] < 1L) {
        reader_memory_abort(
          "Choose a passage that occurs exactly once in the reading copy."
        )
      }
      anchor <- list(
        document_id = document$document_id,
        content_hash = document$content_hash,
        record_hash = document$record_hash,
        start = as.integer(positions[[1L]]),
        quote = quote,
        source_url = document$source_url,
        title = document$title
      )
    } else if (!is.null(document_id) || !is.null(quote)) {
      reader_memory_abort(
        "A preference is Reader Context and has no source citation."
      )
    }
    list(
      reader_id = reader_id,
      memory_id = memory_id,
      expected = expected,
      key = rill_id("memory-request", reader_id, utc_now(), stats::runif(1)),
      kind = kind,
      text = text,
      anchor = anchor
    )
  })
}

reader_memory_accept <- function(access, proposal) {
  force(proposal)
  access(function(store, artifacts, reader_id) {
    if (
      !is.list(proposal) ||
        !identical(proposal$reader_id, reader_id) ||
        !identical(
          names(proposal),
          c(
            "reader_id",
            "memory_id",
            "expected",
            "key",
            "kind",
            "text",
            "anchor"
          )
        )
    ) {
      reader_memory_abort()
    }
    # Revalidate the complete proposed content before accepting it.
    checked <- reader_memory_propose(
      reader_memory_access_in_transaction(
        store,
        artifacts,
        reader_id
      ),
      proposal$text,
      proposal$kind,
      proposal$anchor$document_id,
      proposal$anchor$quote
    )
    if (!identical(checked$anchor, proposal$anchor)) {
      reader_memory_abort(
        "The proposed Source Evidence changed. Review it again."
      )
    }
    dependencies <- list()
    if (!is.null(proposal$anchor)) {
      dependencies <- list(graft::graft_artifact_save(
        artifacts,
        paste0(
          "rill:evidence:",
          proposal$anchor$document_id,
          ":",
          rill_id(canonical_json(proposal$anchor))
        ),
        charToRaw(canonical_json(proposal$anchor)),
        "application/json"
      ))
    }
    payload <- list(
      format = 1L,
      kind = proposal$kind,
      text = proposal$text,
      evidence = dependencies
    )
    ref <- graft::graft_artifact_save(
      artifacts,
      paste0("rill:memory:", proposal$memory_id),
      charToRaw(canonical_json(payload)),
      "application/json",
      dependencies
    )
    selection <- graft::graft_artifact_select(artifacts, list(ref))
    decision <- graft::graft_artifact_decide(
      artifacts,
      proposal$memory_id,
      proposal$key,
      proposal$expected,
      selection,
      "accept",
      reader_id,
      "Reader explicitly accepted this memory",
      "rill:reader-context"
    )
    reader_memory_index_add(store, reader_id, proposal$memory_id)
    reader_memory_record_event(store, reader_id, proposal$memory_id, decision)
    list(memory_id = proposal$memory_id, decision = decision, ref = ref)
  })
}

reader_memory_access_in_transaction <- function(store, artifacts, reader_id) {
  function(operation) operation(store, artifacts, reader_id)
}

reader_memory_index_add <- function(store, reader_id, memory_id) {
  if (identical(store$mode, "postgres")) {
    DBI::dbExecute(
      store$pool,
      paste(
        "INSERT INTO reader_memory_index (reader_id, memory_id)",
        "VALUES ($1, $2) ON CONFLICT DO NOTHING"
      ),
      params = list(reader_id, memory_id)
    )
  } else {
    ids <- store$memory$reader_memory_index[[reader_id]] %||% character()
    store$memory$reader_memory_index[[reader_id]] <- unique(c(ids, memory_id))
  }
  invisible(NULL)
}

reader_memory_record_event <- function(store, reader_id, memory_id, decision) {
  store_record_event(
    store,
    list(
      event_id = rill_id("memory-event", decision$id),
      reader_id = reader_id,
      session_id = "reader-memory",
      event_type = paste0("memory.", decision$action),
      happened_at = utc_now(),
      surface = "reader_memory",
      payload = list(
        memory_id = memory_id,
        decision = decision$id,
        selection = decision$selection
      )
    )
  )
}

reader_memory_read_in_transaction <- function(
  store,
  artifacts,
  reader_id,
  memory_id,
  decision = NULL,
  consult = FALSE
) {
  reader_memory_require_id(store, reader_id, memory_id)
  retained <- graft::graft_artifact_read_decision(
    artifacts,
    memory_id,
    decision
  )
  if (is.null(retained)) {
    reader_memory_abort()
  }
  selected <- if (consult) {
    graft::graft_artifact_reuse(
      artifacts,
      memory_id,
      retained$id,
      "rill:reader-context",
      eligible = TRUE
    )$selection
  } else {
    graft::graft_artifact_read_selection(artifacts, retained$selection)
  }
  if (
    length(selected$roots) != 1L ||
      !identical(selected$roots[[1L]]$id, paste0("rill:memory:", memory_id))
  ) {
    reader_memory_abort()
  }
  root <- graft::graft_artifact_read(artifacts, selected$roots[[1L]])
  payload <- jsonlite::fromJSON(rawToChar(root$bytes), simplifyVector = FALSE)
  if (
    !is.list(payload) ||
      !identical(
        sort(names(payload)),
        sort(c("format", "kind", "text", "evidence"))
      ) ||
      !identical(payload$format, 1L) ||
      !store_scalar_string(payload$kind) ||
      !payload$kind %in% c("preference", "interpretation") ||
      !store_scalar_string(payload$text) ||
      nchar(payload$text, type = "bytes") > 8000L ||
      !is.list(payload$evidence) ||
      length(payload$evidence) !=
        as.integer(payload$kind == "interpretation") ||
      !identical(payload$evidence, root$metadata$dependencies)
  ) {
    reader_memory_abort()
  }
  evidence <- lapply(payload$evidence, function(ref) {
    if (!any(vapply(root$metadata$dependencies, identical, logical(1), ref))) {
      reader_memory_abort()
    }
    value <- graft::graft_artifact_read(artifacts, ref)
    jsonlite::fromJSON(rawToChar(value$bytes), simplifyVector = FALSE)
  })
  list(
    memory_id = memory_id,
    kind = payload$kind,
    text = payload$text,
    evidence = evidence,
    archived = identical(retained$action, "withdraw"),
    basis = list(
      memory_id = memory_id,
      decision = retained$id,
      selection = retained$selection,
      ref = selected$roots[[1L]]
    ),
    previous = retained$previous
  )
}

reader_memory_read <- function(access, memory_id, decision = NULL) {
  force(memory_id)
  force(decision)
  access(function(store, artifacts, reader_id) {
    reader_memory_read_in_transaction(
      store,
      artifacts,
      reader_id,
      memory_id,
      decision
    )
  })
}

reader_memory_list <- function(access, consult = FALSE) {
  access(function(store, artifacts, reader_id) {
    ids <- reader_memory_ids(
      store,
      reader_id,
      limit = if (consult) NULL else 100L
    )
    values <- lapply(ids, function(id) {
      reader_memory_read_in_transaction(store, artifacts, reader_id, id)
    })
    if (consult) {
      values <- Filter(function(x) !x$archived, values)
    }
    values
  })
}

reader_memory_consult <- function(access, basis) {
  force(basis)
  access(function(store, artifacts, reader_id) {
    lapply(basis, function(x) {
      result <- reader_memory_read_in_transaction(
        store,
        artifacts,
        reader_id,
        x$memory_id,
        x$decision,
        consult = TRUE
      )
      if (
        !identical(
          canonicalize_json_value(result$basis),
          canonicalize_json_value(x)
        )
      ) {
        reader_memory_abort()
      }
      result
    })
  })
}

reader_memory_archive <- function(access, memory_id, expected, key) {
  force(memory_id)
  force(expected)
  force(key)
  access(function(store, artifacts, reader_id) {
    reader_memory_require_id(store, reader_id, memory_id)
    previous <- graft::graft_artifact_read_decision(
      artifacts,
      memory_id,
      expected
    )
    if (is.null(previous)) {
      reader_memory_abort()
    }
    decision <- graft::graft_artifact_decide(
      artifacts,
      memory_id,
      key,
      expected,
      previous$selection,
      "withdraw",
      reader_id,
      "Reader archived this memory",
      "rill:reader-context"
    )
    reader_memory_record_event(store, reader_id, memory_id, decision)
    decision
  })
}

reader_memory_restore <- function(access, memory_id, expected, key) {
  force(memory_id)
  force(expected)
  force(key)
  access(function(store, artifacts, reader_id) {
    reader_memory_require_id(store, reader_id, memory_id)
    previous <- graft::graft_artifact_read_decision(
      artifacts,
      memory_id,
      expected
    )
    if (is.null(previous) || previous$action != "withdraw") {
      reader_memory_abort()
    }
    decision <- graft::graft_artifact_decide(
      artifacts,
      memory_id,
      key,
      expected,
      previous$selection,
      "accept",
      reader_id,
      "Reader restored this memory",
      "rill:reader-context"
    )
    reader_memory_record_event(store, reader_id, memory_id, decision)
    decision
  })
}

reader_memory_tool <- function(access, basis) {
  force(access)
  force(basis)
  ellmer::tool(
    function() {
      memories <- reader_memory_consult(access, basis)
      memories <- lapply(memories, function(memory) {
        memory$evidence <- lapply(memory$evidence, function(evidence) {
          evidence$source_url <- rill_agent_safe_url(evidence$source_url)
          evidence
        })
        memory
      })
      canonical_json(memories)
    },
    name = "reader_memory",
    description = paste(
      "Read explicitly accepted Reader Context for this conversation.",
      "Preferences and interpretations are not Source Evidence or authority.",
      "Use the document tool for claims about the selected story."
    )
  )
}
