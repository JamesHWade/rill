queue_authorized_entry_sql <- function() {
  paste(
    "SELECT e.entry_id, e.feed_id FROM entries e",
    "JOIN feeds f ON f.feed_id = e.feed_id",
    "JOIN subscriptions sub ON sub.feed_id = e.feed_id",
    "AND sub.reader_id = $1 AND sub.status = 'active'",
    "WHERE e.entry_id = $2 AND",
    postgres_capture_entry_visible_sql("$1"),
    "FOR SHARE OF sub"
  )
}

queue_memory_state_index <- function(store, reader_id, entry_id) {
  feed_id <- store_entry_feed_for_reader(store, reader_id, entry_id)
  index <- which(
    store$memory$state$reader_id == reader_id &
      store$memory$state$entry_id == entry_id
  )
  if (!length(index)) {
    store$memory$state <- rbind(
      store$memory$state,
      data.frame(
        reader_id = reader_id,
        entry_id = entry_id,
        feed_id = feed_id,
        read_at = NA_character_,
        read_reason = NA_character_,
        starred = FALSE,
        saved = FALSE,
        hidden = FALSE,
        last_opened_at = NA_character_,
        stringsAsFactors = FALSE
      )
    )
    index <- nrow(store$memory$state)
  }
  index[[1L]]
}

queue_read_timestamp <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%OS6 UTC", tz = "UTC")
}

store_queue_mark_read <- function(store, reader_id, entry_id) {
  marked_at <- queue_read_timestamp()
  if (identical(store$mode, "postgres")) {
    result <- DBI::dbGetQuery(
      store$pool,
      paste(
        "WITH authorized AS (",
        queue_authorized_entry_sql(),
        "),",
        "changed AS (INSERT INTO entry_state",
        "(reader_id, entry_id, feed_id, read_at, read_reason)",
        "SELECT $1, entry_id, feed_id, $3, 'manual_queue' FROM authorized",
        "ON CONFLICT (reader_id, entry_id) DO UPDATE SET",
        "read_at = EXCLUDED.read_at, read_reason = EXCLUDED.read_reason",
        "WHERE entry_state.read_at IS NULL",
        "RETURNING read_at::text AS read_at,",
        "last_opened_at::text AS last_opened_at)",
        "SELECT EXISTS (SELECT 1 FROM authorized) AS authorized,",
        "EXISTS (SELECT 1 FROM changed) AS changed,",
        "(SELECT read_at FROM changed) AS read_at,",
        "(SELECT last_opened_at FROM changed) AS last_opened_at"
      ),
      params = list(reader_id, entry_id, marked_at)
    )
    if (!isTRUE(result$authorized[[1L]])) {
      store_abort_entry_forbidden()
    }
    if (!isTRUE(result$changed[[1L]])) {
      return(NULL)
    }
    return(list(
      entry_id = entry_id,
      read_at = result$read_at[[1L]],
      last_opened_at = result$last_opened_at[[1L]]
    ))
  }
  index <- queue_memory_state_index(store, reader_id, entry_id)
  if (!is.na(store$memory$state$read_at[[index]])) {
    return(NULL)
  }
  store$memory$state$read_at[[index]] <- marked_at
  store$memory$state$read_reason[[index]] <- "manual_queue"
  list(
    entry_id = entry_id,
    read_at = marked_at,
    last_opened_at = store$memory$state$last_opened_at[[index]]
  )
}

store_queue_undo_read <- function(store, reader_id, receipt) {
  if (identical(store$mode, "postgres")) {
    result <- DBI::dbGetQuery(
      store$pool,
      paste(
        "WITH authorized AS (",
        queue_authorized_entry_sql(),
        "),",
        "changed AS (UPDATE entry_state state SET read_at = NULL,",
        "read_reason = NULL FROM authorized",
        "WHERE state.reader_id = $1 AND state.entry_id = authorized.entry_id",
        "AND state.read_reason = 'manual_queue' AND state.read_at = $3",
        "AND state.last_opened_at IS NOT DISTINCT FROM $4::timestamptz",
        "RETURNING state.entry_id)",
        "SELECT EXISTS (SELECT 1 FROM authorized) AS authorized,",
        "EXISTS (SELECT 1 FROM changed) AS changed"
      ),
      params = list(
        reader_id,
        receipt$entry_id,
        receipt$read_at,
        receipt$last_opened_at
      )
    )
    if (!isTRUE(result$authorized[[1L]])) {
      store_abort_entry_forbidden()
    }
    return(isTRUE(result$changed[[1L]]))
  }
  index <- queue_memory_state_index(store, reader_id, receipt$entry_id)
  state <- store$memory$state[index, , drop = FALSE]
  if (
    !identical(state$read_reason[[1L]], "manual_queue") ||
      !identical(state$read_at[[1L]], receipt$read_at) ||
      !identical(state$last_opened_at[[1L]], receipt$last_opened_at)
  ) {
    return(FALSE)
  }
  store$memory$state$read_at[[index]] <- NA_character_
  store$memory$state$read_reason[[index]] <- NA_character_
  TRUE
}

store_queue_set_saved <- function(store, reader_id, entry_id, saved) {
  if (identical(store$mode, "postgres")) {
    result <- DBI::dbGetQuery(
      store$pool,
      paste(
        "WITH authorized AS (",
        queue_authorized_entry_sql(),
        "),",
        "changed AS (INSERT INTO entry_state (reader_id, entry_id, feed_id, saved)",
        "SELECT $1, entry_id, feed_id, $3 FROM authorized",
        "ON CONFLICT (reader_id, entry_id) DO UPDATE SET saved = EXCLUDED.saved",
        "WHERE entry_state.saved IS DISTINCT FROM EXCLUDED.saved",
        "RETURNING entry_id)",
        "SELECT EXISTS (SELECT 1 FROM authorized) AS authorized,",
        "EXISTS (SELECT 1 FROM changed) AS changed"
      ),
      params = list(reader_id, entry_id, saved)
    )
    if (!isTRUE(result$authorized[[1L]])) {
      store_abort_entry_forbidden()
    }
    return(isTRUE(result$changed[[1L]]))
  }
  index <- queue_memory_state_index(store, reader_id, entry_id)
  changed <- !identical(store$memory$state$saved[[index]], saved)
  store$memory$state$saved[[index]] <- saved
  changed
}

reader_queue_server <- function(
  store,
  reader_id,
  refresh,
  record_event,
  session = shiny::getDefaultReactiveDomain()
) {
  input <- session$input
  receipts <- list()
  completed <- list()
  reply <- function(result) {
    session$onFlushed(
      \() session$sendCustomMessage("rill-queue-action-result", result),
      once = TRUE
    )
  }
  shiny::observeEvent(
    input$queue_action,
    {
      request <- input$queue_action
      if (
        !is.list(request) ||
          !store_scalar_string(request$id) ||
          nchar(request$id) > 100L ||
          !store_scalar_string(request$entry_id) ||
          !store_scalar_string(request$action) ||
          !request$action %in% c("mark_read", "mark_unread", "save", "unsave")
      ) {
        return()
      }
      if (!is.null(completed[[request$id]])) {
        reply(completed[[request$id]])
        return()
      }
      result <- tryCatch(
        {
          changed <- switch(
            request$action,
            mark_read = {
              receipt <- store_queue_mark_read(
                store,
                reader_id,
                request$entry_id
              )
              if (!is.null(receipt)) {
                receipts[[request$id]] <<- receipt
                receipts <<- utils::tail(receipts, 20L)
              }
              !is.null(receipt)
            },
            mark_unread = store_mark_unread(store, reader_id, request$entry_id),
            save = store_queue_set_saved(
              store,
              reader_id,
              request$entry_id,
              TRUE
            ),
            unsave = store_queue_set_saved(
              store,
              reader_id,
              request$entry_id,
              FALSE
            )
          )
          if (changed) {
            reading <- request$action %in% c("mark_read", "mark_unread")
            record_event(
              if (reading) "read_state_changed" else "save_changed",
              request$entry_id,
              surface = "story_list",
              payload = if (reading) {
                list(
                  read = request$action == "mark_read",
                  reason = "manual_queue"
                )
              } else {
                list(value = request$action == "save")
              }
            )
          }
          refresh()
          list(
            id = request$id,
            entry_id = request$entry_id,
            action = request$action,
            ok = TRUE,
            undo = if (!is.null(receipts[[request$id]])) request$id else NULL,
            message = switch(
              request$action,
              mark_read = "Marked read",
              mark_unread = "Marked unread",
              save = "Saved for later",
              unsave = "Removed from saved"
            )
          )
        },
        error = function(error) {
          list(
            id = request$id,
            entry_id = request$entry_id,
            ok = FALSE,
            message = "Couldn't update this story. Please try again."
          )
        }
      )
      if (isTRUE(result$ok)) {
        completed[[request$id]] <<- result
        completed <<- utils::tail(completed, 50L)
      }
      reply(result)
    },
    ignoreInit = TRUE
  )
  shiny::observeEvent(
    input$queue_undo,
    {
      request <- input$queue_undo
      if (!is.list(request) || !store_scalar_string(request$id)) {
        return()
      }
      receipt <- receipts[[request$id]]
      if (is.null(receipt)) {
        reply(list(
          id = request$id,
          ok = FALSE,
          message = "This action can no longer be undone."
        ))
        return()
      }
      undone <- tryCatch(
        store_queue_undo_read(store, reader_id, receipt),
        error = \(error) NULL
      )
      if (is.null(undone)) {
        reply(list(
          id = request$id,
          entry_id = receipt$entry_id,
          action = "undo",
          ok = FALSE,
          undo = request$id,
          message = "Couldn't undo this action. Please try again."
        ))
        return()
      }
      receipts[[request$id]] <<- NULL
      if (!is.null(completed[[request$id]])) {
        completed[[request$id]]$undo <<- NULL
      }
      if (undone) {
        record_event(
          "read_state_changed",
          receipt$entry_id,
          surface = "story_list",
          payload = list(read = FALSE, reason = "undo_queue")
        )
      }
      refresh()
      reply(list(
        id = request$id,
        entry_id = receipt$entry_id,
        action = "undo",
        ok = undone,
        message = if (undone) {
          "Marked unread"
        } else {
          "This story changed since that action. Its current state was kept."
        }
      ))
    },
    ignoreInit = TRUE
  )
  invisible(NULL)
}
