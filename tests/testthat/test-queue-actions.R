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
        function() NULL,
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
        function() NULL,
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
        function() NULL,
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
