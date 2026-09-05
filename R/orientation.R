new_rill_orientation <- function(
  reader_id,
  boundary,
  question,
  introduction,
  cards,
  agent_run_id,
  evaluated_at = Sys.time(),
  status = NULL,
  policy_version = "orientation-v1",
  dismissals = list()
) {
  orientation_id <- rill_id("orientation", reader_id)
  revision_id <- rill_id(
    "orientation-revision",
    reader_id,
    boundary$hash,
    agent_run_id
  )
  cards <- lapply(cards, function(card) {
    card$frame <- card$frame %||%
      switch(
        card$role,
        anchor = "unresolved_question",
        contrast = "counterpoint",
        extension = "connection",
        "connection"
      )
    basis_hash <- orientation_card_basis(
      reader_id,
      card$entry_id,
      card$document_id
    )
    card$basis_hash <- basis_hash
    card$card_id <- rill_id("orientation-card", orientation_id, basis_hash)
    card$rationale_hash <- orientation_card_rationale(card)
    card
  })
  status <- status %||%
    if (length(cards)) {
      paste(
        length(cards),
        if (length(cards) == 1L) {
          "source-grounded selection is ready."
        } else {
          "source-grounded selections are ready."
        }
      )
    } else {
      "Nothing material has cleared the Orientation threshold."
    }
  structure(
    list(
      orientation_id = orientation_id,
      revision_id = revision_id,
      reader_id = reader_id,
      boundary = boundary,
      question = question,
      introduction = introduction,
      status = status,
      cards = cards,
      agent_run_id = agent_run_id,
      evaluated_at = as.POSIXct(evaluated_at, tz = "UTC"),
      updated_at = as.POSIXct(evaluated_at, tz = "UTC"),
      policy_version = policy_version,
      dismissals = dismissals
    ),
    class = c("rill_orientation", "list")
  )
}

orientation_card_basis <- function(reader_id, entry_id, document_id) {
  rill_id("orientation-card-basis", reader_id, entry_id, document_id)
}

orientation_card_rationale <- function(card) {
  rill_id(
    "orientation-rationale",
    card$interpretation,
    card$why_now,
    card$evidence
  )
}

prepare_orientation_documents <- function(store, reader_id, limit = 12L) {
  entries <- store_list_entries(
    store,
    reader_id,
    view = "unread",
    limit = 500L,
    sort = "newest"
  )
  documents <- store_list_documents(store, reader_id, entries$entry_id)
  pending <- entries[
    !as.character(entries$entry_id) %in% names(documents),
    ,
    drop = FALSE
  ]
  limit <- as.integer(limit)
  prepared <- character()
  for (index in seq_len(nrow(pending))) {
    if (length(prepared) >= limit) {
      break
    }
    entry <- as.list(pending[index, , drop = FALSE])
    document <- tryCatch(
      document_fallback(entry, reason = "orientation-feed-copy"),
      rill_document_invalid = \(error) NULL
    )
    if (is.null(document)) {
      next
    }
    saved <- store_save_document_if_missing_head(store, reader_id, document)
    if (isTRUE(saved$created)) {
      prepared <- c(prepared, document$document_id)
    }
  }

  list(
    total = nrow(entries),
    ready = length(documents),
    prepared = length(prepared),
    document_ids = unname(prepared)
  )
}

orientation_candidates <- function(store, reader_id, limit = 12L, dismissals) {
  entries <- store_list_entries(
    store,
    reader_id,
    view = "unread",
    limit = max(500L, as.integer(limit)),
    sort = "newest"
  )
  documents <- store_list_documents(store, reader_id, entries$entry_id)
  if (missing(dismissals)) {
    dismissals <- store_get_orientation(store, reader_id)$dismissals %||% list()
  }
  dismissed <- vapply(
    dismissals,
    `[[`,
    character(1),
    "basis_hash"
  )

  candidates <- lapply(seq_len(nrow(entries)), function(index) {
    entry <- as.list(entries[index, , drop = FALSE])
    document <- documents[[as.character(entry$entry_id)]] %||% NULL
    basis_hash <- if (is.null(document)) {
      NULL
    } else {
      orientation_card_basis(reader_id, entry$entry_id, document$document_id)
    }
    list(
      entry = entry,
      document = document,
      basis_hash = basis_hash,
      dismissed = !is.null(basis_hash) && basis_hash %in% dismissed
    )
  })
  candidates <- Filter(
    \(candidate) !is.null(candidate$document) && !isTRUE(candidate$dismissed),
    candidates
  )
  utils::head(candidates, as.integer(limit))
}

orientation_boundary <- function(candidates) {
  identities <- lapply(candidates, function(candidate) {
    document <- candidate$document
    list(
      entry_id = as.character(candidate$entry$entry_id),
      document_id = document$document_id %||% NULL,
      document_content_hash = document$content_hash %||% NULL,
      document_record_hash = document$record_hash %||% NULL,
      basis_hash = candidate$basis_hash %||% NULL,
      dismissed = isTRUE(candidate$dismissed)
    )
  })
  document_ids <- vapply(
    Filter(\(candidate) !is.null(candidate$document), candidates),
    \(candidate) candidate$document$document_id,
    character(1)
  )

  list(
    hash = rill_id("orientation-boundary", canonical_json(identities)),
    document_ids = unname(document_ids),
    candidate_count = length(candidates),
    candidates = identities
  )
}

orientation_state_token <- function(state) {
  cards <- vapply(
    state$orientation$cards %||% list(),
    `[[`,
    character(1),
    "basis_hash"
  )
  rill_id(
    "orientation-state",
    state$orientation$revision_id %||% "",
    state$boundary$hash,
    paste(cards, collapse = "\u241f")
  )
}

store_orientation_polled_state <- function(store, reader_id) {
  state <- if (!identical(store$mode, "postgres")) {
    orientation_status(store, reader_id)
  }
  # Read the hosted token first: a concurrent change must trigger another poll,
  # not be remembered as already reflected in an older state.
  token <- tryCatch(
    store_orientation_poll_token(store, reader_id, state),
    error = \(error) NA_character_
  )
  state <- state %||% orientation_status(store, reader_id)
  list(state = state, token = token)
}

store_orientation_poll_token <- function(store, reader_id, state = NULL) {
  if (!identical(store$mode, "postgres")) {
    state <- state %||% orientation_status(store, reader_id)
    return(orientation_state_token(state))
  }

  # Only the final fingerprint crosses the database connection on each poll.
  rows <- DBI::dbGetQuery(
    store$pool,
    paste(
      "WITH orientation_record AS (",
      paste(
        "SELECT revision_id, updated_at, payload FROM orientations",
        "WHERE reader_id = $1"
      ),
      "), unread_entries AS (",
      paste(
        "SELECT e.entry_id, e.feed_id, f.source_kind,",
        "row_number() OVER (ORDER BY",
        "COALESCE(e.published_at, e.inserted_at) DESC, e.entry_id) AS",
        "queue_position"
      ),
      "FROM entries e",
      "JOIN feeds f ON f.feed_id = e.feed_id",
      paste(
        "JOIN subscriptions sub ON sub.feed_id = e.feed_id",
        "AND sub.reader_id = $1 AND sub.status = 'active'"
      ),
      paste(
        "LEFT JOIN entry_state st ON st.entry_id = e.entry_id",
        "AND st.reader_id = $1"
      ),
      "WHERE COALESCE(st.hidden, false) = false AND st.read_at IS NULL",
      paste("AND", postgres_capture_entry_visible_sql("$1")),
      paste(
        "ORDER BY COALESCE(e.published_at, e.inserted_at) DESC,",
        "e.entry_id LIMIT 500"
      ),
      "), candidate_documents AS (",
      paste(
        "SELECT unread.queue_position, unread.entry_id, d.document_id,",
        "d.content_hash, d.record_hash"
      ),
      "FROM unread_entries unread",
      paste(
        "LEFT JOIN reader_document_selections selected",
        "ON selected.reader_id = $1",
        "AND selected.entry_id = unread.entry_id"
      ),
      paste(
        "LEFT JOIN public_document_heads public",
        "ON public.entry_id = unread.entry_id"
      ),
      paste(
        "JOIN documents d ON d.document_id =",
        "COALESCE(selected.document_id, public.document_id)"
      ),
      "WHERE (unread.source_kind <> 'capture' OR d.reader_id = $1)",
      "AND NOT EXISTS (",
      "SELECT 1 FROM orientation_record current,",
      paste(
        "jsonb_array_elements(COALESCE(",
        "current.payload -> 'dismissals', '[]'::jsonb",
        ")) dismissal"
      ),
      paste(
        "WHERE dismissal ->> 'entry_id' = unread.entry_id",
        "AND dismissal ->> 'document_id' = d.document_id"
      ),
      ") ORDER BY unread.queue_position LIMIT 12",
      "), fingerprint AS (",
      "SELECT concat_ws('|',",
      paste(
        "COALESCE((SELECT revision_id FROM orientation_record), ''),",
        "COALESCE((SELECT extract(epoch FROM updated_at)::text",
        "FROM orientation_record), ''),"
      ),
      "COALESCE((SELECT string_agg(concat_ws(':',",
      paste(
        "queue_position::text, entry_id, document_id, content_hash,",
        "record_hash), '|' ORDER BY queue_position)"
      ),
      "FROM candidate_documents), '')",
      ") AS value)",
      "SELECT md5(value) AS poll_token FROM fingerprint"
    ),
    params = list(reader_id)
  )
  rows$poll_token[[1L]]
}

orientation_status <- function(store, reader_id, limit = 12L) {
  orientation <- store_get_orientation(store, reader_id)
  candidates <- orientation_candidates(
    store,
    reader_id,
    limit = limit,
    dismissals = orientation$dismissals %||% list()
  )
  boundary <- orientation_boundary(candidates)
  if (is.null(orientation)) {
    return(list(
      orientation = NULL,
      candidates = candidates,
      boundary = boundary,
      due = TRUE,
      invalid_document_ids = character()
    ))
  }

  card_document_ids <- vapply(
    orientation$cards,
    `[[`,
    character(1),
    "document_id"
  )
  active_basis <- vapply(
    Filter(
      \(candidate) !is.null(candidate$document) && !isTRUE(candidate$dismissed),
      candidates
    ),
    `[[`,
    character(1),
    "basis_hash"
  )
  card_basis <- vapply(
    orientation$cards,
    `[[`,
    character(1),
    "basis_hash"
  )
  valid <- card_document_ids %in%
    boundary$document_ids &
    card_basis %in% active_basis
  orientation$cards <- orientation$cards[valid]
  if (any(!valid)) {
    count <- length(orientation$cards)
    orientation$status <- if (count) {
      paste(
        count,
        if (count == 1L) {
          "source-grounded selection remains current."
        } else {
          "source-grounded selections remain current."
        }
      )
    } else {
      "No current Orientation selection remains."
    }
    if (count) {
      orientation$question <- "What still deserves attention?"
      orientation$introduction <- paste(
        "These source-grounded selections remain current while Orientation",
        "catches up with your reading."
      )
      orientation$cards <- Map(
        function(card, index) {
          if (index == 1L) {
            card$role <- "anchor"
            card$frame <- "connection"
          }
          card
        },
        orientation$cards,
        seq_along(orientation$cards)
      )
    }
  }

  list(
    orientation = orientation,
    candidates = candidates,
    boundary = boundary,
    due = !identical(orientation$boundary$hash, boundary$hash),
    invalid_document_ids = unname(card_document_ids[!valid])
  )
}

orientation_processing_note <- function(
  store,
  orientation,
  config,
  destination_state = NULL,
  preparing = FALSE
) {
  if (isTRUE(config$demo_mode)) {
    return(
      "Bundled demo Orientation \u00b7 no reading copies were sent to a model."
    )
  }

  destination <- rill_agent_data_destination(
    config$agent_model,
    config$agent_base_url %||% ""
  )
  if (isTRUE(preparing)) {
    return(paste(
      "Evaluating bounded reading copies with",
      paste0(destination, ".")
    ))
  }
  if (!is.null(orientation)) {
    run <- store_get_agent_run(
      store,
      orientation$reader_id,
      orientation$agent_run_id
    )
    produced_by <- run$pinned_inputs$data_destination %||% NULL
    if (!is.null(produced_by)) {
      return(paste(
        "Produced from bounded reading copies sent to",
        paste0(produced_by, ".")
      ))
    }
  }
  destination_state <- destination_state %||%
    orientation_destination_state(store, config$actor_id, config)
  if (isTRUE(destination_state$enabled)) {
    return(paste(
      "Automatic Orientation is enabled for",
      paste0(destination, ".")
    ))
  }
  if (!isTRUE(destination_state$available)) {
    return("Automatic Orientation is unavailable in this installation.")
  }
  if (isTRUE(destination_state$needs_confirmation)) {
    return(paste(
      "Automatic Orientation is off \u00b7 confirm",
      destination,
      "as its external Data Destination."
    ))
  }
  paste(
    "Automatic Orientation is off for",
    paste0(destination, ".")
  )
}

store_save_orientation <- function(store, orientation) {
  validate_orientation(store, orientation)
  if (identical(store$mode, "postgres")) {
    DBI::dbExecute(
      store$pool,
      paste(
        "INSERT INTO orientations (",
        paste(
          "reader_id, orientation_id, revision_id, boundary_hash,",
          "evaluation_run_id, evaluated_at, updated_at, payload"
        ),
        ") VALUES ($1, $2, $3, $4, $5, $6, $7, $8::jsonb)",
        "ON CONFLICT (reader_id) DO UPDATE SET",
        paste(
          "orientation_id = EXCLUDED.orientation_id,",
          "revision_id = EXCLUDED.revision_id,",
          "boundary_hash = EXCLUDED.boundary_hash,",
          "evaluation_run_id = EXCLUDED.evaluation_run_id,",
          "evaluated_at = EXCLUDED.evaluated_at,",
          "updated_at = EXCLUDED.updated_at,",
          "payload = EXCLUDED.payload"
        )
      ),
      params = list(
        orientation$reader_id,
        orientation$orientation_id,
        orientation$revision_id,
        orientation$boundary$hash,
        orientation$agent_run_id,
        orientation$evaluated_at,
        orientation$updated_at,
        orientation_json(orientation_payload(orientation))
      )
    )
    return(orientation)
  }

  store$memory$orientations[[orientation$reader_id]] <- orientation
  orientation
}

store_get_orientation <- function(store, reader_id, lock = FALSE) {
  if (identical(store$mode, "postgres")) {
    suffix <- if (isTRUE(lock)) " FOR UPDATE" else ""
    rows <- DBI::dbGetQuery(
      store$pool,
      paste0("SELECT * FROM orientations WHERE reader_id = $1", suffix),
      params = list(reader_id)
    )
    if (!nrow(rows)) {
      return(NULL)
    }
    return(orientation_from_row(rows[1L, , drop = FALSE]))
  }

  store$memory$orientations[[reader_id]] %||% NULL
}

orientation_payload <- function(orientation) {
  list(
    boundary = orientation$boundary,
    question = orientation$question,
    introduction = orientation$introduction,
    status = orientation$status,
    cards = orientation$cards,
    policy_version = orientation$policy_version,
    dismissals = orientation$dismissals
  )
}

orientation_json <- function(value) {
  as.character(jsonlite::toJSON(
    value,
    auto_unbox = TRUE,
    null = "null",
    digits = NA
  ))
}

orientation_records <- function(records, identity_field) {
  if (is.null(records) || !length(records)) {
    return(list())
  }
  if (!is.list(records)) {
    orientation_abort("Orientation JSON contains an invalid record collection.")
  }
  if (!is.null(names(records)) && identity_field %in% names(records)) {
    return(list(records))
  }
  records
}

orientation_from_row <- function(row) {
  value <- \(name) row[[name]][[1L]]
  payload <- jsonlite::fromJSON(
    as.character(value("payload")),
    simplifyVector = FALSE
  )
  payload$boundary$document_ids <- as.character(unlist(
    payload$boundary$document_ids %||% list(),
    use.names = FALSE
  ))
  payload$boundary$candidate_count <- as.integer(
    payload$boundary$candidate_count %||% 0L
  )
  payload$boundary$candidates <- orientation_records(
    payload$boundary$candidates,
    "entry_id"
  )
  cards <- orientation_records(payload$cards, "document_id")
  dismissals <- orientation_records(payload$dismissals, "basis_hash")

  structure(
    list(
      orientation_id = value("orientation_id"),
      revision_id = value("revision_id"),
      reader_id = value("reader_id"),
      boundary = payload$boundary,
      question = payload$question,
      introduction = payload$introduction,
      status = payload$status,
      cards = cards,
      agent_run_id = value("evaluation_run_id"),
      evaluated_at = value("evaluated_at"),
      updated_at = value("updated_at"),
      policy_version = payload$policy_version,
      dismissals = lapply(dismissals, function(dismissal) {
        dismissal$dismissed_at <- as.POSIXct(
          as.character(dismissal$dismissed_at),
          tz = "UTC"
        )
        dismissal
      })
    ),
    class = c("rill_orientation", "list")
  )
}

store_select_orientation_card <- function(
  store,
  reader_id,
  orientation_id,
  revision_id,
  card_id,
  entry_id,
  document_id,
  basis_hash,
  rationale_hash,
  event_id,
  session_id,
  selected_at = utc_now(),
  lock = FALSE
) {
  if (
    identical(store$mode, "postgres") &&
      inherits(store$pool, "Pool") &&
      !isTRUE(lock)
  ) {
    return(pool::poolWithTransaction(store$pool, function(connection) {
      DBI::dbExecute(
        connection,
        "SET TRANSACTION ISOLATION LEVEL SERIALIZABLE"
      )
      transaction_store <- structure(
        list(mode = "postgres", pool = connection),
        class = "rill_store"
      )
      store_select_orientation_card(
        transaction_store,
        reader_id = reader_id,
        orientation_id = orientation_id,
        revision_id = revision_id,
        card_id = card_id,
        entry_id = entry_id,
        document_id = document_id,
        basis_hash = basis_hash,
        rationale_hash = rationale_hash,
        event_id = event_id,
        session_id = session_id,
        selected_at = selected_at,
        lock = TRUE
      )
    }))
  }

  orientation <- store_get_orientation(store, reader_id, lock = lock)
  if (is.null(orientation)) {
    orientation_abort("There is no current Orientation for this Reader.")
  }
  if (
    !identical(orientation$orientation_id, orientation_id) ||
      !identical(orientation$revision_id, revision_id)
  ) {
    orientation_abort("That Orientation revision is no longer current.")
  }
  stored_cards <- Filter(
    function(card) {
      identical(card$card_id, card_id) &&
        identical(card$entry_id, entry_id) &&
        identical(card$document_id, document_id) &&
        identical(card$basis_hash, basis_hash) &&
        identical(card$rationale_hash, rationale_hash)
    },
    orientation$cards
  )
  if (length(stored_cards) != 1L) {
    orientation_abort("That Orientation card is no longer current.")
  }
  card <- stored_cards[[1L]]

  if (identical(store$mode, "postgres")) {
    entry <- DBI::dbGetQuery(
      store$pool,
      "SELECT entry_id FROM entries WHERE entry_id = $1 FOR UPDATE",
      params = list(card$entry_id)
    )
    if (nrow(entry) != 1L) {
      orientation_abort("That Orientation card is no longer current.")
    }
    head <- DBI::dbGetQuery(
      store$pool,
      paste(
        paste(
          "SELECT COALESCE(selected.document_id, public.document_id)",
          "AS document_id FROM entries e"
        ),
        paste(
          "LEFT JOIN reader_document_selections selected",
          "ON selected.reader_id = $1 AND selected.entry_id = e.entry_id"
        ),
        paste(
          "LEFT JOIN public_document_heads public",
          "ON public.entry_id = e.entry_id"
        ),
        "WHERE e.entry_id = $2"
      ),
      params = list(reader_id, card$entry_id)
    )
    if (
      nrow(head) != 1L ||
        !identical(as.character(head$document_id[[1L]]), card$document_id)
    ) {
      orientation_abort("That Orientation card is no longer current.")
    }
    DBI::dbGetQuery(
      store$pool,
      paste(
        "SELECT entry_id FROM entry_state",
        "WHERE reader_id = $1 AND entry_id = $2 FOR UPDATE"
      ),
      params = list(reader_id, card$entry_id)
    )
  }

  orientation <- orientation_status(
    store,
    reader_id,
    limit = 12L
  )$orientation
  cards <- Filter(
    function(card) {
      identical(card$card_id, card_id) &&
        identical(card$entry_id, entry_id) &&
        identical(card$document_id, document_id) &&
        identical(card$basis_hash, basis_hash) &&
        identical(card$rationale_hash, rationale_hash)
    },
    orientation$cards %||% list()
  )
  if (length(cards) != 1L) {
    orientation_abort("That Orientation card is no longer current.")
  }
  card <- cards[[1L]]

  position <- match(
    card$card_id,
    vapply(orientation$cards, `[[`, character(1), "card_id")
  )
  provenance <- list(
    orientation_id = orientation$orientation_id,
    revision_id = orientation$revision_id,
    card_id = card$card_id,
    entry_id = card$entry_id,
    document_id = card$document_id,
    basis_hash = card$basis_hash,
    rationale_hash = card$rationale_hash,
    agent_run_id = orientation$agent_run_id,
    role = card$role,
    frame = card$frame,
    interpretation = card$interpretation,
    why_now = card$why_now,
    evidence = card$evidence,
    boundary_hash = orientation$boundary$hash,
    evaluated_at = format(
      orientation$evaluated_at,
      tz = "UTC",
      usetz = TRUE
    ),
    policy_version = orientation$policy_version
  )
  event <- list(
    event_id = orientation_string(event_id, "event_id"),
    reader_id = reader_id,
    entry_id = card$entry_id,
    session_id = orientation_string(session_id, "session_id"),
    event_type = "entry_opened",
    happened_at = selected_at,
    surface = "orientation",
    position = as.integer(position),
    payload = provenance
  )
  previous_state <- if (identical(store$mode, "memory")) {
    store$memory$state
  } else {
    NULL
  }
  previous_events <- if (identical(store$mode, "memory")) {
    store$memory$events
  } else {
    NULL
  }
  tryCatch(
    {
      store_mark_opened(
        store,
        reader_id,
        card$entry_id,
        opened_at = selected_at
      )
      store_record_event(store, event, require_new = TRUE)
    },
    error = function(error) {
      if (identical(store$mode, "memory")) {
        store$memory$state <- previous_state
        store$memory$events <- previous_events
      }
      stop(error)
    }
  )

  list(
    entry_id = card$entry_id,
    document_id = card$document_id,
    position = as.integer(position),
    provenance = provenance,
    event = event
  )
}

store_dismiss_orientation_card <- function(
  store,
  reader_id,
  card_id,
  revision_id,
  rationale_hash,
  dismissed_at = Sys.time(),
  event = NULL,
  lock = FALSE
) {
  if (
    identical(store$mode, "postgres") &&
      inherits(store$pool, "Pool") &&
      !isTRUE(lock)
  ) {
    return(pool::poolWithTransaction(store$pool, function(connection) {
      transaction_store <- structure(
        list(mode = "postgres", pool = connection),
        class = "rill_store"
      )
      store_dismiss_orientation_card(
        transaction_store,
        reader_id,
        card_id,
        revision_id,
        rationale_hash,
        dismissed_at = dismissed_at,
        event = event,
        lock = TRUE
      )
    }))
  }

  orientation <- store_get_orientation(store, reader_id, lock = lock)
  if (is.null(orientation)) {
    orientation_abort("There is no current Orientation for this Reader.")
  }
  if (!identical(orientation$revision_id, revision_id)) {
    orientation_abort("That Orientation revision is no longer current.")
  }
  cards <- Filter(\(card) identical(card$card_id, card_id), orientation$cards)
  if (length(cards) != 1L) {
    orientation_abort("That Orientation card is no longer current.")
  }
  card <- cards[[1L]]
  if (!identical(card$rationale_hash, rationale_hash)) {
    orientation_abort("That Orientation rationale is no longer current.")
  }
  existing <- Filter(
    \(dismissal) identical(dismissal$basis_hash, card$basis_hash),
    orientation$dismissals
  )
  if (length(existing)) {
    if (!is.null(event)) {
      store_record_event(
        store,
        orientation_dismissal_event(event, existing[[1L]])
      )
    }
    return(existing[[1L]])
  }

  active_basis <- vapply(
    orientation_candidates(store, reader_id, limit = 12L),
    `[[`,
    character(1),
    "basis_hash"
  )
  if (!card$basis_hash %in% active_basis) {
    orientation_abort("That Orientation card is no longer current.")
  }

  dismissal <- list(
    orientation_id = orientation$orientation_id,
    revision_id = orientation$revision_id,
    card_id = card$card_id,
    basis_hash = card$basis_hash,
    entry_id = card$entry_id,
    document_id = card$document_id,
    why_now = card$why_now,
    rationale_hash = card$rationale_hash,
    dismissed_at = as.POSIXct(dismissed_at, tz = "UTC")
  )
  orientation$dismissals <- c(orientation$dismissals, list(dismissal))
  orientation$updated_at <- as.POSIXct(dismissed_at, tz = "UTC")
  previous <- store_get_orientation(store, reader_id)
  previous_events <- if (identical(store$mode, "memory")) {
    store$memory$events
  } else {
    NULL
  }
  tryCatch(
    {
      store_save_orientation(store, orientation)
      if (!is.null(event)) {
        store_record_event(
          store,
          orientation_dismissal_event(event, dismissal)
        )
      }
    },
    error = function(error) {
      if (identical(store$mode, "memory")) {
        store$memory$orientations[[reader_id]] <- previous
        store$memory$events <- previous_events
      }
      stop(error)
    }
  )
  dismissal
}

orientation_dismissal_event <- function(event, dismissal) {
  event$entry_id <- dismissal$entry_id
  event$happened_at <- dismissal$dismissed_at
  event$payload <- list(
    card_id = dismissal$card_id,
    orientation_id = dismissal$orientation_id,
    revision_id = dismissal$revision_id,
    document_id = dismissal$document_id,
    basis_hash = dismissal$basis_hash,
    rationale_hash = dismissal$rationale_hash,
    why_now = dismissal$why_now
  )
  event
}

store_complete_orientation_run <- function(
  store,
  orientation,
  worker_id,
  usage = list(),
  terminal_reason = "complete",
  deputy_run_id = NULL,
  finished_at = Sys.time(),
  allow_cancelling = FALSE
) {
  publishable_statuses <- if (isTRUE(allow_cancelling)) {
    c("running", "cancelling")
  } else {
    "running"
  }
  run <- store_get_agent_run(
    store,
    orientation$reader_id,
    orientation$agent_run_id
  )
  if (
    is.null(run) ||
      !identical(run$kind, "orientation") ||
      !run$status %in% publishable_statuses ||
      !identical(run$worker_id, worker_id) ||
      !identical(
        run$pinned_inputs$boundary_hash,
        orientation$boundary$hash
      )
  ) {
    return(NULL)
  }

  complete <- function(target_store) {
    owned_run <- store_get_agent_run(
      target_store,
      orientation$reader_id,
      orientation$agent_run_id
    )
    if (
      is.null(owned_run) ||
        !identical(owned_run$kind, "orientation") ||
        !owned_run$status %in% publishable_statuses ||
        !identical(owned_run$worker_id, worker_id) ||
        !identical(
          owned_run$pinned_inputs$boundary_hash,
          orientation$boundary$hash
        )
    ) {
      return(NULL)
    }
    current <- store_get_orientation(
      target_store,
      orientation$reader_id,
      lock = identical(target_store$mode, "postgres")
    )
    if (!is.null(current)) {
      known <- vapply(
        orientation$dismissals,
        `[[`,
        character(1),
        "basis_hash"
      )
      carried <- Filter(
        \(dismissal) !dismissal$basis_hash %in% known,
        current$dismissals
      )
      orientation$dismissals <- c(orientation$dismissals, carried)
    }
    candidate_limit <- as.integer(
      owned_run$pinned_inputs$candidate_limit %||%
        orientation$boundary$candidate_count %||%
        12L
    )
    current_boundary <- orientation_boundary(orientation_candidates(
      target_store,
      orientation$reader_id,
      limit = candidate_limit
    ))
    if (!identical(current_boundary$hash, orientation$boundary$hash)) {
      return(NULL)
    }
    orientation$evaluated_at <- as.POSIXct(finished_at, tz = "UTC")
    orientation$updated_at <- orientation$evaluated_at
    validate_orientation(target_store, orientation)

    completed_run <- store_finish_agent_run(
      target_store,
      reader_id = orientation$reader_id,
      run_id = orientation$agent_run_id,
      worker_id = worker_id,
      status = "completed",
      usage = usage,
      terminal_reason = terminal_reason,
      deputy_run_id = deputy_run_id,
      finished_at = finished_at
    )
    if (is.null(completed_run)) {
      return(NULL)
    }
    saved <- store_save_orientation(target_store, orientation)
    list(run = completed_run, orientation = saved)
  }

  if (identical(store$mode, "postgres")) {
    return(pool::poolWithTransaction(store$pool, function(connection) {
      DBI::dbExecute(
        connection,
        "SET TRANSACTION ISOLATION LEVEL SERIALIZABLE"
      )
      transaction_store <- structure(
        list(mode = "postgres", pool = connection),
        class = "rill_store"
      )
      complete(transaction_store)
    }))
  }

  complete(store)
}

orientation_abort <- function(message) {
  cli::cli_abort(message, class = "rill_orientation_invalid")
}

orientation_string <- function(value, field) {
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(trimws(value))
  ) {
    orientation_abort(
      "Orientation {.field {field}} must be a non-empty string."
    )
  }
  value
}

validate_orientation <- function(store, orientation) {
  if (!inherits(orientation, "rill_orientation")) {
    orientation_abort("Orientation must be a {.cls rill_orientation} object.")
  }
  orientation_string(orientation$orientation_id, "orientation_id")
  orientation_string(orientation$revision_id, "revision_id")
  orientation_string(orientation$reader_id, "reader_id")
  orientation_string(orientation$status, "status")
  orientation_string(orientation$agent_run_id, "agent_run_id")
  orientation_string(orientation$policy_version, "policy_version")
  if (!is.list(orientation$dismissals)) {
    orientation_abort("Orientation {.field dismissals} must be a list.")
  }
  if (!is.list(orientation$boundary)) {
    orientation_abort("Orientation {.field boundary} must be a list.")
  }
  orientation_string(orientation$boundary$hash, "boundary.hash")
  if (!is.list(orientation$cards) || length(orientation$cards) > 3L) {
    orientation_abort("Orientation must contain zero to three cards.")
  }
  if (length(orientation$cards)) {
    orientation_string(orientation$question, "question")
    orientation_string(orientation$introduction, "introduction")
  }

  document_ids <- character()
  roles <- character()
  for (card in orientation$cards) {
    if (!is.list(card)) {
      orientation_abort("Each Orientation card must be a list.")
    }
    role <- orientation_string(card$role, "card.role")
    if (!role %in% c("anchor", "contrast", "extension")) {
      orientation_abort(
        paste(
          "Orientation card roles must be {.val anchor}, {.val contrast},",
          "or {.val extension}."
        )
      )
    }
    frame <- orientation_string(card$frame, "card.frame")
    if (
      !frame %in%
        c("change", "connection", "counterpoint", "unresolved_question")
    ) {
      orientation_abort("An Orientation card has an unknown editorial frame.")
    }
    document_id <- orientation_string(card$document_id, "card.document_id")
    entry_id <- orientation_string(card$entry_id, "card.entry_id")
    orientation_string(card$basis_hash, "card.basis_hash")
    orientation_string(card$card_id, "card.card_id")
    rationale_hash <- orientation_string(
      card$rationale_hash,
      "card.rationale_hash"
    )
    orientation_string(card$interpretation, "card.interpretation")
    orientation_string(card$why_now, "card.why_now")
    evidence <- orientation_string(card$evidence, "card.evidence")
    if (!identical(rationale_hash, orientation_card_rationale(card))) {
      orientation_abort("An Orientation card has an invalid rationale hash.")
    }
    document <- store_get_document_by_id(
      store,
      orientation$reader_id,
      document_id
    )
    if (is.null(document) || !identical(document$entry_id, entry_id)) {
      orientation_abort(
        "An Orientation card does not identify its immutable Document."
      )
    }
    if (!grepl(evidence, document$markdown, fixed = TRUE)) {
      orientation_abort(
        "Orientation Source Evidence must appear exactly in its Document."
      )
    }
    if (!document_id %in% orientation$boundary$document_ids) {
      orientation_abort(
        "An Orientation card is outside the evaluated Library boundary."
      )
    }
    document_ids <- c(document_ids, document_id)
    roles <- c(roles, role)
  }
  if (anyDuplicated(document_ids)) {
    orientation_abort("Orientation cards must identify distinct Documents.")
  }
  if (
    length(roles) &&
      (!identical(roles[[1L]], "anchor") ||
        any(roles[-1L] == "anchor"))
  ) {
    orientation_abort("An Orientation reading path must begin with one anchor.")
  }
  producing_run <- store_get_agent_run(
    store,
    orientation$reader_id,
    orientation$agent_run_id
  )
  if (
    is.null(producing_run) ||
      !identical(producing_run$kind, "orientation")
  ) {
    orientation_abort(
      "Orientation must identify its Reader's producing Orientation Agent Run."
    )
  }

  invisible(orientation)
}
