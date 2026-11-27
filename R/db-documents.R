document_select_sql <- function() {
  paste(
    "SELECT d.document_id, d.entry_id, d.reader_id, d.source_url,",
    "d.canonical_url,",
    "d.acquisition_method, d.producer, d.producer_version,",
    "d.producer_record_id, d.captured_at, d.received_at, d.title,",
    "d.author, d.site, d.published_at, d.markdown, d.word_count,",
    "d.content_hash, d.record_hash, d.provenance::text AS provenance,",
    "d.schema_version FROM documents d"
  )
}

store_get_document_record <- function(store, document_id) {
  if (identical(store$mode, "postgres")) {
    rows <- DBI::dbGetQuery(
      store$pool,
      paste(
        document_select_sql(),
        "WHERE d.document_id = $1"
      ),
      params = list(document_id)
    )
    if (!nrow(rows)) {
      return(NULL)
    }
    return(document_from_store_row(rows))
  }

  store$memory$documents[[document_id]] %||% NULL
}

memory_document_is_pinned <- function(store, reader_id, document_id) {
  agent_run_pin <- any(vapply(
    store$memory$agent_runs,
    function(run) {
      if (!identical(run$reader_id, reader_id)) {
        return(FALSE)
      }
      identical(run$pinned_inputs$document_id %||% NULL, document_id) ||
        document_id %in% (run$pinned_inputs$document_ids %||% character())
    },
    logical(1)
  ))
  orientation_pin <- any(vapply(
    store$memory$orientations,
    function(orientation) {
      if (!identical(orientation$reader_id, reader_id)) {
        return(FALSE)
      }
      document_id %in%
        vapply(
          orientation$cards %||% list(),
          `[[`,
          character(1),
          "document_id"
        )
    },
    logical(1)
  ))
  agent_run_pin || orientation_pin
}

store_get_document_by_id <- function(store, reader_id, document_id) {
  if (identical(store$mode, "postgres")) {
    rows <- DBI::dbGetQuery(
      store$pool,
      paste(
        document_select_sql(),
        "WHERE d.document_id = $2 AND (d.reader_id = $1 OR (",
        "d.reader_id IS NULL AND (EXISTS (SELECT 1 FROM entries e",
        "JOIN subscriptions s ON s.feed_id = e.feed_id",
        paste(
          "WHERE e.entry_id = d.entry_id AND s.reader_id = $1",
          "AND s.status = 'active')"
        ),
        "OR EXISTS (SELECT 1 FROM agent_runs ar WHERE ar.reader_id = $1",
        "AND (ar.pinned_inputs ->> 'document_id' = d.document_id",
        paste(
          "OR COALESCE(ar.pinned_inputs -> 'document_ids', '[]'::jsonb)",
          "? d.document_id))"
        ),
        "OR EXISTS (SELECT 1 FROM orientations o WHERE o.reader_id = $1",
        paste(
          "AND o.payload -> 'cards' @> jsonb_build_array(",
          "jsonb_build_object('document_id', d.document_id)))"
        ),
        ")))"
      ),
      params = list(reader_id, document_id)
    )
    if (!nrow(rows)) {
      return(NULL)
    }
    return(document_from_store_row(rows))
  }

  document <- store_get_document_record(store, document_id)
  if (is.null(document)) {
    return(NULL)
  }
  if (identical(document$reader_id, reader_id)) {
    return(document)
  }
  if (!is.na(document$reader_id)) {
    return(NULL)
  }
  if (
    !is.null(store_get_entry(store, reader_id, document$entry_id)) ||
      memory_document_is_pinned(store, reader_id, document_id)
  ) {
    return(document)
  }
  NULL
}

store_list_documents <- function(store, reader_id, entry_ids = NULL) {
  if (identical(store$mode, "postgres")) {
    query <- paste(
      document_select_sql(),
      "JOIN entries e ON e.entry_id = d.entry_id",
      "JOIN feeds f ON f.feed_id = e.feed_id",
      paste(
        "JOIN subscriptions sub ON sub.feed_id = e.feed_id",
        "AND sub.reader_id = $1 AND sub.status = 'active'"
      ),
      paste(
        "LEFT JOIN reader_document_selections selected",
        "ON selected.reader_id = $1 AND selected.entry_id = e.entry_id"
      ),
      "LEFT JOIN public_document_heads public ON public.entry_id = e.entry_id",
      paste(
        "WHERE d.document_id =",
        "COALESCE(selected.document_id, public.document_id)"
      ),
      "AND (f.source_kind <> 'capture' OR d.reader_id = $1)"
    )
    params <- list(reader_id)
    if (!is.null(entry_ids)) {
      entry_ids <- unique(as.character(entry_ids))
      if (!length(entry_ids)) {
        return(list())
      }
      placeholders <- paste0(
        "$",
        seq_along(entry_ids) + 1L,
        collapse = ", "
      )
      query <- paste(query, "AND e.entry_id IN (", placeholders, ")")
      params <- c(params, as.list(entry_ids))
    }
    query <- paste(
      query,
      paste(
        "ORDER BY COALESCE(selected.selected_at, public.selected_at) DESC,",
        "e.entry_id"
      )
    )
    rows <- DBI::dbGetQuery(store$pool, query, params = params)
    if (!nrow(rows)) {
      return(list())
    }
    documents <- lapply(seq_len(nrow(rows)), function(index) {
      document_from_store_row(rows[index, , drop = FALSE])
    })
    return(stats::setNames(
      documents,
      vapply(documents, `[[`, character(1), "entry_id")
    ))
  }

  visible_entry_ids <- memory_reader_visible_entry_ids(store, reader_id)
  if (!is.null(entry_ids)) {
    visible_entry_ids <- intersect(visible_entry_ids, as.character(entry_ids))
  }
  if (!length(visible_entry_ids)) {
    return(list())
  }
  heads <- store$memory$document_heads
  heads <- heads[names(heads) %in% visible_entry_ids]
  selections <- store$memory$document_selections
  selections <- selections[
    selections$reader_id == reader_id &
      selections$entry_id %in% visible_entry_ids,
    ,
    drop = FALSE
  ]
  if (nrow(selections)) {
    heads[selections$entry_id] <- selections$document_id
  }
  documents <- unname(lapply(heads, function(id) {
    store$memory$documents[[id]]
  }))
  documents <- Filter(Negate(is.null), documents)
  if (!length(documents)) {
    return(list())
  }
  stats::setNames(
    documents,
    vapply(documents, `[[`, character(1), "entry_id")
  )
}

store_get_document <- function(store, reader_id, entry_id) {
  documents <- store_list_documents(
    store,
    reader_id,
    entry_ids = entry_id
  )
  if (!length(documents)) NULL else documents[[1]]
}

store_insert_document <- function(connection, document) {
  DBI::dbExecute(
    connection,
    paste(
      "INSERT INTO documents",
      "(document_id, entry_id, reader_id, source_url, canonical_url,",
      "acquisition_method, producer, producer_version, producer_record_id,",
      "title, author, site, published_at, markdown, word_count,",
      "content_hash, record_hash, captured_at, received_at, provenance,",
      "schema_version)",
      "VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12,",
      "$13, $14, $15, $16, $17, $18, $19, $20::jsonb, $21)",
      "ON CONFLICT (document_id) DO NOTHING"
    ),
    params = list(
      document$document_id,
      document$entry_id,
      document$reader_id,
      document$source_url,
      document$canonical_url,
      document$acquisition_method,
      document$producer,
      document$producer_version,
      document$producer_record_id,
      document$title,
      document$author,
      document$site,
      document$published_at,
      document$markdown,
      as.integer(document$word_count),
      document$content_hash,
      document$record_hash,
      document$captured_at,
      document$received_at,
      document_provenance_json(document),
      as.integer(document$schema_version)
    )
  )
}

store_select_document <- function(
  store,
  reader_id,
  document_id,
  selected_at = utc_now()
) {
  if (identical(store$mode, "postgres")) {
    selected <- DBI::dbGetQuery(
      store$pool,
      paste(
        "INSERT INTO reader_document_selections",
        paste(
          "(reader_id, feed_id, entry_id, document_id, ownership_key,",
          "selected_at)"
        ),
        "SELECT $1, e.feed_id, d.entry_id, d.document_id, d.ownership_key, $3",
        "FROM documents d JOIN entries e ON e.entry_id = d.entry_id",
        "WHERE d.document_id = $2 AND (d.reader_id = $1 OR (",
        "d.reader_id IS NULL AND EXISTS (SELECT 1 FROM subscriptions s",
        paste(
          "WHERE s.reader_id = $1 AND s.feed_id = e.feed_id",
          "AND s.status = 'active')))"
        ),
        "ON CONFLICT (reader_id, entry_id) DO UPDATE SET",
        "feed_id = EXCLUDED.feed_id, document_id = EXCLUDED.document_id,",
        paste(
          "ownership_key = EXCLUDED.ownership_key,",
          "selected_at = EXCLUDED.selected_at"
        ),
        "RETURNING entry_id"
      ),
      params = list(reader_id, document_id, selected_at)
    )
    if (nrow(selected) != 1L) {
      cli::cli_abort(
        "That Document is not available to this Reader.",
        class = "rill_document_forbidden"
      )
    }
    return(invisible(document_id))
  }

  document <- store_get_document_record(store, document_id)
  if (is.null(document)) {
    cli::cli_abort(
      "That Document is not available to this Reader.",
      class = "rill_document_forbidden"
    )
  }
  public_access <- is.na(document$reader_id) &&
    document$entry_id %in% memory_reader_visible_entry_ids(store, reader_id)
  if (!identical(document$reader_id, reader_id) && !public_access) {
    cli::cli_abort(
      "That Document is not available to this Reader.",
      class = "rill_document_forbidden"
    )
  }
  entry_index <- match(document$entry_id, store$memory$entries$entry_id)
  selection <- data.frame(
    reader_id = reader_id,
    feed_id = store$memory$entries$feed_id[[entry_index]],
    entry_id = document$entry_id,
    document_id = document_id,
    ownership_key = if (is.na(document$reader_id)) {
      "public"
    } else {
      paste0("reader:", reader_id)
    },
    selected_at = selected_at,
    stringsAsFactors = FALSE
  )
  selections <- store$memory$document_selections
  index <- which(
    selections$reader_id == reader_id &
      selections$entry_id == document$entry_id
  )
  if (length(index)) {
    selections[index[[1L]], ] <- selection
    store$memory$document_selections <- selections
  } else {
    store$memory$document_selections <- rbind(selections, selection)
  }
  invisible(document_id)
}

store_replace_selected_document <- function(
  store,
  reader_id,
  expected_document_id,
  replacement_document_id,
  selected_at = utc_now()
) {
  if (identical(store$mode, "postgres")) {
    updated <- DBI::dbGetQuery(
      store$pool,
      paste(
        "UPDATE reader_document_selections selected SET",
        "document_id = replacement.document_id,",
        "ownership_key = replacement.ownership_key, selected_at = $4",
        "FROM documents replacement",
        "WHERE selected.reader_id = $1",
        "AND selected.document_id = $2",
        "AND replacement.document_id = $3",
        "AND replacement.entry_id = selected.entry_id",
        "AND (replacement.reader_id IS NULL OR replacement.reader_id = $1)",
        "RETURNING selected.entry_id"
      ),
      params = list(
        reader_id,
        expected_document_id,
        replacement_document_id,
        selected_at
      )
    )
    return(nrow(updated) == 1L)
  }

  selections <- store$memory$document_selections
  index <- which(
    selections$reader_id == reader_id &
      selections$document_id == expected_document_id
  )
  replacement <- store_get_document_by_id(
    store,
    reader_id,
    replacement_document_id
  )
  if (
    length(index) != 1L ||
      is.null(replacement) ||
      !identical(replacement$entry_id, selections$entry_id[[index]])
  ) {
    return(FALSE)
  }
  selections$document_id[[index]] <- replacement_document_id
  selections$ownership_key[[index]] <- if (is.na(replacement$reader_id)) {
    "public"
  } else {
    paste0("reader:", reader_id)
  }
  selections$selected_at[[index]] <- selected_at
  store$memory$document_selections <- selections
  TRUE
}

store_save_document <- function(store, document) {
  existing <- store_get_document_record(store, document$document_id)
  if (!is.null(existing)) {
    legacy_replay <- identical(
      existing$acquisition_method,
      "browser_capture"
    ) &&
      identical(existing$reader_id, document$reader_id) &&
      identical(
        existing$record_hash,
        document_record_hash(document, include_reader = FALSE)
      )
    if (
      !identical(existing$record_hash, document$record_hash) &&
        !legacy_replay
    ) {
      document_conflict_abort(
        paste(
          "The producer record ID was already used for different content.",
          "Create a new capture ID and retry."
        )
      )
    }
    return(invisible(list(created = FALSE, document = existing)))
  }

  if (identical(store$mode, "postgres")) {
    created <- pool::poolWithTransaction(store$pool, function(connection) {
      DBI::dbGetQuery(
        connection,
        "SELECT entry_id FROM entries WHERE entry_id = $1 FOR UPDATE",
        params = list(document$entry_id)
      )
      inserted <- store_insert_document(connection, document)
      transaction_store <- structure(
        list(mode = "postgres", pool = connection),
        class = "rill_store"
      )
      if (identical(inserted, 1L) && is.na(document$reader_id)) {
        DBI::dbExecute(
          connection,
          paste(
            "INSERT INTO public_document_heads",
            "(entry_id, document_id, ownership_key, selected_at)",
            "VALUES ($1, $2, 'public', $3)",
            "ON CONFLICT (entry_id) DO UPDATE SET",
            "document_id = EXCLUDED.document_id,",
            "selected_at = EXCLUDED.selected_at"
          ),
          params = list(
            document$entry_id,
            document$document_id,
            document$received_at
          )
        )
      }
      if (!is.na(document$reader_id)) {
        store_select_document(
          transaction_store,
          document$reader_id,
          document$document_id,
          selected_at = document$received_at
        )
      }
      identical(inserted, 1L)
    })
    if (!created) {
      raced <- store_get_document_record(store, document$document_id)
      if (
        is.null(raced) || !identical(raced$record_hash, document$record_hash)
      ) {
        document_conflict_abort("The document conflicted with another write.")
      }
    }
    return(invisible(list(created = created, document = document)))
  }

  store$memory$documents[[document$document_id]] <- document
  if (is.na(document$reader_id)) {
    store$memory$document_heads[[document$entry_id]] <- document$document_id
  } else {
    store_select_document(
      store,
      document$reader_id,
      document$document_id,
      selected_at = document$received_at
    )
  }
  invisible(list(created = TRUE, document = document))
}

store_replace_public_document <- function(
  store,
  expected_document_id,
  document
) {
  if (!is.na(document$reader_id)) {
    cli::cli_abort(
      "Only a public Document may become the shared default reading copy.",
      class = "rill_document_invalid"
    )
  }
  replace <- function(transaction_store) {
    current <- public_reading_document(transaction_store, document$entry_id)
    if (
      is.null(current) || !identical(current$document_id, expected_document_id)
    ) {
      return(invisible(FALSE))
    }
    if (identical(transaction_store$mode, "postgres")) {
      store_insert_document(transaction_store$pool, document)
      changed <- DBI::dbExecute(
        transaction_store$pool,
        paste(
          "UPDATE public_document_heads SET document_id = $1, selected_at = $2",
          "WHERE entry_id = $3 AND document_id = $4"
        ),
        params = list(
          document$document_id,
          document$received_at,
          document$entry_id,
          expected_document_id
        )
      )
      return(invisible(identical(changed, 1L)))
    }
    store_save_document(transaction_store, document)
    invisible(TRUE)
  }
  if (identical(store$mode, "postgres")) {
    return(pool::poolWithTransaction(store$pool, function(connection) {
      DBI::dbGetQuery(
        connection,
        "SELECT entry_id FROM entries WHERE entry_id = $1 FOR UPDATE",
        params = list(document$entry_id)
      )
      replace(structure(
        list(mode = "postgres", pool = connection),
        class = "rill_store"
      ))
    }))
  }
  replace(store)
}

store_save_document_if_missing_head <- function(store, reader_id, document) {
  if (!is.na(document$reader_id)) {
    cli::cli_abort(
      "Only a public Document may become the shared default reading copy.",
      class = "rill_document_invalid"
    )
  }
  current <- store_get_document(store, reader_id, document$entry_id)
  if (!is.null(current)) {
    return(invisible(list(created = FALSE, document = current)))
  }

  if (identical(store$mode, "postgres")) {
    result <- pool::poolWithTransaction(store$pool, function(connection) {
      transaction_store <- structure(
        list(mode = "postgres", pool = connection),
        class = "rill_store"
      )
      # A missing head has no row to lock, so serialize on its parent Entry.
      DBI::dbGetQuery(
        connection,
        "SELECT entry_id FROM entries WHERE entry_id = $1 FOR UPDATE",
        params = list(document$entry_id)
      )
      current <- store_get_document(
        transaction_store,
        reader_id,
        document$entry_id
      )
      if (!is.null(current)) {
        return(list(
          created = FALSE,
          document = current
        ))
      }

      inserted <- store_insert_document(connection, document)
      if (!identical(inserted, 1L)) {
        raced <- store_get_document_record(
          transaction_store,
          document$document_id
        )
        if (
          is.null(raced) ||
            !identical(raced$record_hash, document$record_hash)
        ) {
          document_conflict_abort(
            "The fallback Document conflicted with another write."
          )
        }
      }
      selected <- DBI::dbExecute(
        connection,
        paste(
          "INSERT INTO public_document_heads",
          "(entry_id, document_id, ownership_key, selected_at)",
          "VALUES ($1, $2, 'public', $3)",
          "ON CONFLICT (entry_id) DO NOTHING"
        ),
        params = list(
          document$entry_id,
          document$document_id,
          document$received_at
        )
      )
      if (identical(selected, 1L)) {
        return(list(created = TRUE, document = document))
      }
      list(
        created = FALSE,
        document = store_get_document(
          transaction_store,
          reader_id,
          document$entry_id
        )
      )
    })
    return(invisible(result))
  }

  store_save_document(store, document)
}
