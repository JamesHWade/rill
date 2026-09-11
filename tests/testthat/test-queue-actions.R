testthat::test_that("queue actions acknowledge refreshed state and measure visibility", {
  testthat::skip_if_not_installed("otelsdk")
  store <- rill_store(list(demo_mode = TRUE, actor_id = "queue-reader"))
  messages <- list()
  flushed <- FALSE
  id <- strrep("a", 32L)
  record <- otelsdk::with_otel_record(
    later::with_temp_loop(shiny::testServer(
      function(input, output, session) {
        session$sendCustomMessage <- function(type, message) {
          if (type == "rill-queue-action-result") {
            messages[[length(messages) + 1L]] <<- list(
              message = message,
              flushed = flushed
            )
          }
        }
        reader_queue_server(
          store,
          "queue-reader",
          function(entry_id) NULL,
          function(...) NULL,
          session,
          telemetry_enabled = TRUE
        )
      },
      {
        session$flushReact()
        session$onFlushed(
          function() {
            flushed <<- TRUE
          },
          once = TRUE
        )
        session$setInputs(
          queue_action = list(
            id = "read-1",
            telemetry_id = id,
            entry_id = "sample-entry-2",
            action = "mark_read"
          )
        )
        testthat::expect_identical(messages[[1L]]$flushed, TRUE)
        testthat::expect_identical(messages[[1L]]$message$telemetry_id, id)
        testthat::expect_identical(messages[[1L]]$message$ok, TRUE)
        session$setInputs(queue_action_visible = list(id = id, elapsed_ms = -1))
        session$setInputs(
          queue_action_visible = list(
            id = id,
            elapsed_ms = 120,
            dom_ready_ms = 90
          )
        )
      }
    )),
    what = "traces"
  )
  action <- record$traces$queue.action
  testthat::expect_identical(
    action$attributes$queue_action.surface,
    "mark_read"
  )
  testthat::expect_identical(action$attributes$queue_action.outcome, "visible")
  testthat::expect_equal(action$attributes$queue_action.visible_ms, 120)
  testthat::expect_equal(action$attributes$queue_action.paint_delay_ms, 30)
  testthat::expect_gte(action$attributes$queue_action.server_flush_ms, 0)
  testthat::expect_identical(
    record$traces$queue.action.persist$parent,
    action$span_id
  )
  testthat::expect_no_match(
    paste(
      c(names(action$attributes), unlist(action$attributes)),
      collapse = "\n"
    ),
    "sample-entry|queue-reader"
  )
})

testthat::test_that("queue actions and conditional undo agree across stores", {
  for (backend in c("memory", "postgres")) {
    store <- local_orientation_backend_store(backend, "queue-reader")
    id <- "sample-entry-2"
    receipt <- store_queue_mark_read(store, "queue-reader", id)
    entry <- store_get_entry(store, "queue-reader", id)
    testthat::expect_identical(entry$read_reason, "manual_queue")
    testthat::expect_all_true(is.na(entry$last_opened_at))
    testthat::expect_null(store_queue_mark_read(store, "queue-reader", id))
    testthat::expect_identical(
      store_queue_set_saved(store, "queue-reader", id, TRUE),
      TRUE
    )
    testthat::expect_identical(
      store_queue_set_saved(store, "queue-reader", id, TRUE),
      FALSE
    )
    testthat::expect_identical(
      store_queue_undo_read(store, "queue-reader", receipt),
      TRUE
    )
    testthat::expect_all_true(is.na(
      store_get_entry(store, "queue-reader", id)$read_at
    ))
    testthat::expect_identical(
      store_get_entry(store, "queue-reader", id)$saved,
      TRUE
    )
    testthat::expect_identical(
      store_queue_undo_read(store, "queue-reader", receipt),
      FALSE
    )
    same_second <- "2026-09-09 12:00:00 UTC"
    store_mark_opened(store, "queue-reader", id, opened_at = same_second)
    store_mark_unread(store, "queue-reader", id)
    receipt <- store_queue_mark_read(store, "queue-reader", id)
    store_mark_opened(store, "queue-reader", id, opened_at = same_second)
    testthat::expect_identical(
      store_queue_undo_read(store, "queue-reader", receipt),
      FALSE
    )
    testthat::expect_error(
      store_queue_mark_read(store, "other-reader", id),
      class = "rill_entry_forbidden"
    )
    testthat::expect_error(
      store_queue_set_saved(store, "other-reader", id, TRUE),
      class = "rill_entry_forbidden"
    )
    testthat::expect_error(
      store_queue_undo_read(store, "other-reader", receipt),
      class = "rill_entry_forbidden"
    )
  }
})

testthat::test_that("queue requests replay without changing later reader decisions", {
  store <- rill_store(list(demo_mode = TRUE, actor_id = "queue-reader"))
  shiny::testServer(
    function(input, output, session) {
      reader_queue_server(
        store,
        "queue-reader",
        function(entry_id) NULL,
        function(...) NULL,
        session
      )
    },
    {
      session$flushReact()
      request <- list(
        id = "read-1",
        entry_id = "sample-entry-2",
        action = "mark_read"
      )
      session$setInputs(queue_action = request)
      session$setInputs(queue_undo = list(id = "read-1"))
      testthat::expect_all_true(is.na(
        store_get_entry(store, "queue-reader", "sample-entry-2")$read_at
      ))
      session$setInputs(queue_action = request)
      testthat::expect_all_true(is.na(
        store_get_entry(store, "queue-reader", "sample-entry-2")$read_at
      ))
    }
  )
})

testthat::test_that("transient undo failures retain the receipt for retry", {
  store <- rill_store(list(demo_mode = TRUE, actor_id = "queue-reader"))
  undo <- store_queue_undo_read
  attempts <- 0L
  testthat::local_mocked_bindings(store_queue_undo_read = function(...) {
    attempts <<- attempts + 1L
    if (attempts == 1L) {
      stop("Temporary storage failure")
    }
    undo(...)
  })
  shiny::testServer(
    function(input, output, session) {
      reader_queue_server(
        store,
        "queue-reader",
        function(entry_id) NULL,
        function(...) NULL,
        session
      )
    },
    {
      session$flushReact()
      session$setInputs(
        queue_action = list(
          id = "retry-undo",
          entry_id = "sample-entry-2",
          action = "mark_read"
        )
      )
      session$setInputs(queue_undo = list(id = "retry-undo", nonce = 1))
      testthat::expect_all_true(
        !is.na(store_get_entry(store, "queue-reader", "sample-entry-2")$read_at)
      )
      session$setInputs(queue_undo = list(id = "retry-undo", nonce = 2))
      testthat::expect_all_true(is.na(
        store_get_entry(store, "queue-reader", "sample-entry-2")$read_at
      ))
    }
  )
})

testthat::test_that("Undo survives completed-result cache eviction", {
  store <- rill_store(list(demo_mode = TRUE, actor_id = "queue-reader"))
  shiny::testServer(
    function(input, output, session) {
      reader_queue_server(
        store,
        "queue-reader",
        function(entry_id) NULL,
        function(...) NULL,
        session
      )
    },
    {
      session$flushReact()
      session$setInputs(
        queue_action = list(
          id = "old-read",
          entry_id = "sample-entry-2",
          action = "mark_read"
        )
      )
      for (index in seq_len(51L)) {
        session$setInputs(
          queue_action = list(
            id = paste0("save-", index),
            entry_id = "sample-entry-2",
            action = "save"
          )
        )
      }
      session$setInputs(queue_undo = list(id = "old-read"))
      entry <- store_get_entry(store, "queue-reader", "sample-entry-2")
      testthat::expect_all_true(is.na(entry$read_at))
      testthat::expect_identical(entry$saved, TRUE)
    }
  )
})

testthat::test_that("PostgreSQL Undo receipts preserve exact subsecond timestamps", {
  store <- local_orientation_backend_store("postgres", "precise-reader")
  for (fraction in c("000001", "123456", "999999")) {
    timestamp <- paste0("2026-09-10 12:34:56.", fraction, " UTC")
    testthat::local_mocked_bindings(queue_read_timestamp = function() timestamp)
    receipt <- store_queue_mark_read(store, "precise-reader", "sample-entry-2")
    stored <- DBI::dbGetQuery(
      store$pool,
      "SELECT read_at::text AS read_at FROM entry_state WHERE reader_id = 'precise-reader' AND entry_id = 'sample-entry-2'"
    )$read_at[[1L]]
    testthat::expect_identical(receipt$read_at, stored)
    testthat::expect_identical(
      store_queue_undo_read(store, "precise-reader", receipt),
      TRUE
    )
  }
})
