testthat::test_that("selecting a story records the open and updates the queue", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  entry_id <- store$memory$entries$entry_id[[1]]

  shiny::testServer(rill_server(config, store), {
    session$setInputs(select_entry = NULL)
    session$flushReact()
    session$setInputs(
      select_entry = list(id = entry_id, position = 1L, nonce = 1)
    )
    session$flushReact()

    testthat::expect_identical(selected_id(), entry_id)
    testthat::expect_identical(selected_position(), 1L)
    testthat::expect_equal(nrow(queue_entries()), 5L)
    testthat::expect_equal(nrow(entries()), 6L)
    testthat::expect_in(entry_id, entries()$entry_id)
    testthat::expect_identical(store$memory$events$event_type, "entry_opened")
    testthat::expect_identical(store$memory$state$read_reason, "opened")

    session$setInputs(close_reader = list(nonce = 1))
    session$flushReact()

    testthat::expect_null(selected_id())
    testthat::expect_equal(nrow(entries()), 6L)
  })
})

testthat::test_that("asking about a story runs Deputy through shinychat", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  entry_id <- store$memory$entries$entry_id[[1]]
  invocation <- NULL
  appended <- NULL

  testthat::local_mocked_bindings(
    rill_reader_agent = function(document, on_stop, ...) {
      list(stream_async = function(prompt, stream, run_context) {
        invocation <<- list(
          document_id = document$document_id,
          prompt = prompt,
          stream = stream,
          run_context = run_context
        )
        list(
          consume = function() {
            on_stop(
              "complete",
              list(
                usage = list(requests = 1L, output_tokens = 42L),
                run_context = run_context,
                run_id = "deputy-run-1"
              )
            )
            "Streamed source-grounded response"
          }
        )
      })
    },
    append_reader_chat = function(response, session) {
      if (is.character(response)) {
        appended <<- response
        return(promises::promise_resolve(response))
      }
      appended <<- response$consume()
      promises::promise_resolve(appended)
    }
  )

  shiny::testServer(rill_server(config, store), {
    session$setInputs(reader_chat_user_input = NULL)
    session$flushReact()
    session$setInputs(
      select_entry = list(id = entry_id, position = 1L, nonce = 1)
    )
    session$flushReact()
    session$setInputs(
      reader_chat_user_input = "What is the author's main point?"
    )
    session$flushReact()

    run <- active_agent_run()
    testthat::expect_identical(run$status, "completed")
    testthat::expect_identical(
      run$pinned_inputs$document_id,
      invocation$document_id
    )
    document <- selected_document()
    testthat::expect_identical(
      run$pinned_inputs$submission_id,
      run$request_key
    )
    testthat::expect_identical(
      run$pinned_inputs$document_content_hash,
      document$content_hash
    )
    testthat::expect_identical(
      run$pinned_inputs$document_record_hash,
      document$record_hash
    )
    testthat::expect_identical(
      run$pinned_inputs$research_scope,
      list(
        kind = "selected_document",
        document_ids = document$document_id
      )
    )
    testthat::expect_identical(
      run$pinned_inputs$data_destination,
      "OpenAI"
    )
    testthat::expect_identical(run$pinned_inputs$model, "openai")
    testthat::expect_identical(
      run$pinned_inputs$limits,
      rill_agent_run_limits()
    )
    testthat::expect_identical(
      invocation$prompt,
      "What is the author's main point?"
    )
    testthat::expect_identical(invocation$stream, "content")
    testthat::expect_identical(
      invocation$run_context$rill_agent_run_id,
      run$run_id
    )
    testthat::expect_identical(
      appended,
      "Streamed source-grounded response"
    )
    testthat::expect_identical(run$terminal_reason, "complete")
    testthat::expect_identical(run$deputy_run_id, "deputy-run-1")
    testthat::expect_identical(run$usage$requests, 1L)
  })
})

testthat::test_that("a re-delivered chat submission reuses its Agent Run", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  entry_id <- store$memory$entries$entry_id[[1]]
  invocations <- 0L

  testthat::local_mocked_bindings(
    rill_reader_agent = function(on_stop, ...) {
      list(stream_async = function(prompt, stream, run_context) {
        invocations <<- invocations + 1L
        list(consume = function() {
          on_stop(
            "complete",
            list(
              usage = list(requests = 1L),
              run_context = run_context,
              run_id = "deputy-run-idempotent"
            )
          )
          "response"
        })
      })
    },
    append_reader_chat = function(response, session) {
      promises::promise_resolve(response$consume())
    }
  )

  shiny::testServer(rill_server(config, store), {
    session$setInputs(reader_chat_user_input = NULL)
    session$flushReact()
    session$setInputs(
      select_entry = list(id = entry_id, position = 1L, nonce = 1),
      reader_chat_submission_id = "17"
    )
    session$flushReact()
    session$setInputs(reader_chat_user_input = "What is the main claim?")
    session$flushReact()
    first <- active_agent_run()

    session$setInputs(reader_chat_user_input = NULL)
    session$flushReact()
    session$setInputs(reader_chat_user_input = "What is the main claim?")
    session$flushReact()
    replay <- active_agent_run()

    testthat::expect_identical(invocations, 1L)
    testthat::expect_identical(replay$run_id, first$run_id)
    testthat::expect_identical(replay$status, "completed")
  })
})

testthat::test_that("cancelling a question terminalizes the same Agent Run", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  entry_id <- store$memory$entries$entry_id[[1]]
  interrupted <- NULL
  stream_context <- NULL

  testthat::local_mocked_bindings(
    rill_reader_agent = function(on_stop, ...) {
      list(
        stream_async = function(prompt, stream, run_context) {
          stream_context <<- run_context
          "pending stream"
        },
        interrupt = function(reason) {
          interrupted <<- reason
          on_stop(
            reason,
            list(
              usage = list(requests = 1L),
              run_context = stream_context,
              run_id = "deputy-run-cancelled"
            )
          )
          TRUE
        }
      )
    },
    append_reader_chat = function(response, session) {
      promises::promise_resolve(response)
    }
  )

  shiny::testServer(rill_server(config, store), {
    session$setInputs(
      reader_chat_user_input = NULL,
      reader_chat_cancel = NULL
    )
    session$flushReact()
    session$setInputs(
      select_entry = list(id = entry_id, position = 1L, nonce = 1)
    )
    session$flushReact()
    session$setInputs(reader_chat_user_input = "Summarize this story.")
    session$flushReact()

    running <- active_agent_run()
    testthat::expect_identical(running$status, "running")

    session$setInputs(reader_chat_cancel = 1L)
    session$flushReact()

    cancelled <- active_agent_run()
    testthat::expect_identical(interrupted, "reader_cancelled")
    testthat::expect_identical(cancelled$run_id, running$run_id)
    testthat::expect_identical(cancelled$status, "cancelled")
    testthat::expect_identical(cancelled$terminal_reason, "reader_cancelled")
    testthat::expect_identical(
      cancelled$deputy_run_id,
      "deputy-run-cancelled"
    )
  })
})

testthat::test_that("the wall deadline interrupts and terminalizes a response", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  entry_id <- store$memory$entries$entry_id[[1]]
  interrupted <- NULL
  stop_callback <- NULL
  stream_context <- NULL

  testthat::local_mocked_bindings(
    rill_agent_wall_time_seconds = \() 0,
    rill_reader_agent = function(on_stop, ...) {
      stop_callback <<- on_stop
      list(
        stream_async = function(prompt, stream, run_context) {
          stream_context <<- run_context
          "pending stream"
        },
        interrupt = function(reason) {
          interrupted <<- reason
          TRUE
        }
      )
    },
    append_reader_chat = function(response, session) {
      promises::promise_resolve(response)
    }
  )

  shiny::testServer(rill_server(config, store), {
    session$setInputs(reader_chat_user_input = NULL)
    session$flushReact()
    session$setInputs(
      select_entry = list(id = entry_id, position = 1L, nonce = 1)
    )
    session$flushReact()
    session$setInputs(reader_chat_user_input = "Summarize this story.")
    session$flushReact()
    later::run_now(0)
    session$flushReact()

    run <- active_agent_run()
    testthat::expect_identical(interrupted, "wall_time_limit")
    testthat::expect_identical(run$status, "failed")
    testthat::expect_identical(run$terminal_reason, "wall_time_limit")
    testthat::expect_null(run$deputy_run_id)

    session$setInputs(retry_agent_run = 1L)
    session$flushReact()
    testthat::expect_length(store$memory$agent_runs, 1L)

    stop_callback(
      "wall_time_limit",
      list(
        usage = list(requests = 1L, output_tokens = 12L),
        run_context = stream_context,
        run_id = "deputy-run-time-limited"
      )
    )
    settled <- active_agent_run()
    testthat::expect_identical(settled$run_id, run$run_id)
    testthat::expect_identical(settled$status, "failed")
    testthat::expect_identical(
      settled$terminal_reason,
      "wall_time_limit"
    )
    testthat::expect_identical(
      settled$terminal_at,
      run$terminal_at
    )
    testthat::expect_identical(
      settled$deputy_run_id,
      "deputy-run-time-limited"
    )
    testthat::expect_identical(settled$usage$requests, 1L)

    stop_callback(
      "wall_time_limit",
      list(
        usage = settled$usage,
        run_context = stream_context,
        run_id = "deputy-run-time-limited"
      )
    )
    duplicate <- active_agent_run()
    testthat::expect_identical(
      duplicate$deputy_run_id,
      settled$deputy_run_id
    )

    stop_callback(
      "wall_time_limit",
      list(
        usage = list(requests = 99L),
        run_context = stream_context,
        run_id = "different-deputy-run"
      )
    )
    rejected <- active_agent_run()
    testthat::expect_identical(
      rejected$deputy_run_id,
      "deputy-run-time-limited"
    )
    testthat::expect_identical(rejected$usage$requests, 1L)
  })
})

testthat::test_that("an interrupt error keeps the timed-out Agent draining", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  entry_id <- store$memory$entries$entry_id[[1]]

  testthat::local_mocked_bindings(
    rill_agent_wall_time_seconds = \() 0,
    rill_reader_agent = function(...) {
      list(
        stream_async = \(prompt, stream, run_context) "pending stream",
        interrupt = function(reason) {
          cli::cli_abort(
            "The provider controller failed to cancel.",
            class = "test_controller_cancel_failed"
          )
        }
      )
    },
    append_reader_chat = function(response, session) {
      promises::promise_resolve(response)
    }
  )

  shiny::testServer(rill_server(config, store), {
    session$setInputs(reader_chat_user_input = NULL)
    session$flushReact()
    session$setInputs(
      select_entry = list(id = entry_id, position = 1L, nonce = 1)
    )
    session$flushReact()
    session$setInputs(reader_chat_user_input = "Summarize this story.")
    session$flushReact()
    later::run_now(0)
    session$flushReact()

    timed_out <- active_agent_run()
    testthat::expect_identical(timed_out$status, "failed")
    testthat::expect_identical(
      timed_out$terminal_reason,
      "wall_time_limit"
    )

    session$setInputs(retry_agent_run = 1L)
    session$flushReact()
    testthat::expect_length(store$memory$agent_runs, 1L)
    testthat::expect_identical(active_agent_run()$run_id, timed_out$run_id)
  })
})

testthat::test_that("asking without a story returns a usable chat response", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  appended <- NULL

  testthat::local_mocked_bindings(
    append_reader_chat = function(response, session) {
      appended <<- response
      promises::promise_resolve(response)
    }
  )

  shiny::testServer(rill_server(config, store), {
    session$setInputs(reader_chat_user_input = NULL)
    session$flushReact()
    session$setInputs(reader_chat_user_input = "What does this say?")
    session$flushReact()

    testthat::expect_match(appended, "Choose a story", fixed = TRUE)
    testthat::expect_null(active_agent_run())
    testthat::expect_length(store$memory$agent_runs, 0L)
  })
})

testthat::test_that("retry creates a linked Run over the pinned question", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  entry_id <- store$memory$entries$entry_id[[1]]
  invocations <- list()

  testthat::local_mocked_bindings(
    rill_reader_agent = function(on_stop, ...) {
      list(stream_async = function(prompt, stream, run_context) {
        index <- length(invocations) + 1L
        invocations[[index]] <<- list(
          prompt = prompt,
          run_context = run_context
        )
        list(consume = function() {
          reason <- if (index == 1L) "provider_error" else "complete"
          on_stop(
            reason,
            list(
              usage = list(requests = 1L),
              run_context = run_context,
              run_id = paste0("deputy-run-", index)
            )
          )
          paste("response", index)
        })
      })
    },
    append_reader_chat = function(response, session) {
      promises::promise_resolve(response$consume())
    }
  )

  shiny::testServer(rill_server(config, store), {
    session$setInputs(
      reader_chat_user_input = NULL,
      retry_agent_run = NULL
    )
    session$flushReact()
    session$setInputs(
      select_entry = list(id = entry_id, position = 1L, nonce = 1)
    )
    session$flushReact()
    session$setInputs(reader_chat_user_input = "What is the main claim?")
    session$flushReact()

    failed <- active_agent_run()
    testthat::expect_identical(failed$status, "failed")

    session$setInputs(retry_agent_run = 1L)
    session$flushReact()

    retried <- active_agent_run()
    testthat::expect_identical(retried$status, "completed")
    testthat::expect_identical(retried$retry_of_run_id, failed$run_id)
    testthat::expect_identical(
      retried$pinned_inputs,
      failed$pinned_inputs
    )
    testthat::expect_identical(
      vapply(invocations, `[[`, character(1), "prompt"),
      rep("What is the main claim?", 2L)
    )
    testthat::expect_identical(
      retried$deputy_run_id,
      "deputy-run-2"
    )
  })
})

testthat::test_that("changing stories resets the source-bound Ask Rill chat", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  entry_ids <- store$memory$entries$entry_id[1:2]
  agent_documents <- character()
  clear_count <- 0L

  testthat::local_mocked_bindings(
    clear_reader_chat = function(session) {
      clear_count <<- clear_count + 1L
    },
    rill_reader_agent = function(document, on_stop, ...) {
      agent_documents <<- c(agent_documents, document$document_id)
      list(stream_async = function(prompt, stream, run_context) {
        list(consume = function() {
          on_stop(
            "complete",
            list(
              usage = list(requests = 1L),
              run_context = run_context,
              run_id = paste0("deputy-run-", length(agent_documents))
            )
          )
          "response"
        })
      })
    },
    append_reader_chat = function(response, session) {
      promises::promise_resolve(response$consume())
    }
  )

  shiny::testServer(rill_server(config, store), {
    session$setInputs(reader_chat_user_input = NULL)
    session$flushReact()
    session$setInputs(
      select_entry = list(id = entry_ids[[1]], position = 1L, nonce = 1)
    )
    session$flushReact()
    session$setInputs(reader_chat_user_input = "Question one")
    session$flushReact()
    session$setInputs(
      select_entry = list(id = entry_ids[[2]], position = 2L, nonce = 2)
    )
    session$flushReact()
    session$setInputs(reader_chat_user_input = "Question two")
    session$flushReact()

    testthat::expect_identical(clear_count, 2L)
    testthat::expect_length(unique(agent_documents), 2L)
    testthat::expect_identical(
      reader_agent_document_id(),
      agent_documents[[2]]
    )
  })
})

testthat::test_that("marking a story unread keeps its open history", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  entry_id <- store$memory$entries$entry_id[[1]]

  shiny::testServer(rill_server(config, store), {
    session$setInputs(select_entry = NULL, mark_unread = NULL)
    session$flushReact()
    session$setInputs(
      select_entry = list(id = entry_id, position = 1L, nonce = 1)
    )
    session$flushReact()
    opened_at <- store$memory$state$last_opened_at[[1]]

    session$setInputs(mark_unread = 1L)
    session$flushReact()

    testthat::expect_identical(store$memory$state$read_at, NA_character_)
    testthat::expect_identical(store$memory$state$read_reason, NA_character_)
    testthat::expect_identical(
      store$memory$state$last_opened_at,
      opened_at
    )
    testthat::expect_identical(
      store$memory$events$event_type,
      c("entry_opened", "read_state_changed")
    )
    testthat::expect_identical(status_text(), "Marked story as unread")
  })
})

testthat::test_that("bulk read actions use non-engagement reasons", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  store$memory$entries$published_at <- format(
    Sys.time() - c(60, rep(2 * 24 * 60 * 60, 5)),
    tz = "UTC",
    usetz = TRUE
  )

  shiny::testServer(rill_server(config, store), {
    session$setInputs(mark_older_read = NULL, mark_all_read = NULL)
    session$flushReact()
    session$setInputs(mark_older_read = 1L)
    session$flushReact()

    testthat::expect_equal(nrow(queue_entries()), 1L)
    testthat::expect_identical(
      unique(store$memory$state$read_reason),
      "bulk_older_than_day"
    )
    testthat::expect_identical(
      status_text(),
      "Marked 5 stories older than a day as read"
    )

    session$setInputs(mark_all_read = 1L)
    session$flushReact()

    testthat::expect_equal(nrow(queue_entries()), 0L)
    testthat::expect_setequal(
      store$memory$state$read_reason,
      c("bulk_older_than_day", "bulk_all")
    )
    testthat::expect_identical(
      store$memory$events$event_type,
      c("read_state_bulk_changed", "read_state_bulk_changed")
    )
  })
})

testthat::test_that("changing views clears the current reader and retained queue", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  entry_id <- store$memory$entries$entry_id[[1]]

  shiny::testServer(rill_server(config, store), {
    session$setInputs(view = "unread")
    session$flushReact()
    session$setInputs(
      select_entry = list(id = entry_id, position = 1L, nonce = 1)
    )
    session$flushReact()
    session$setInputs(view = "starred")
    session$flushReact()

    testthat::expect_null(selected_id())
    testthat::expect_length(retained_ids(), 0L)
    testthat::expect_equal(nrow(entries()), 0L)
  })
})

testthat::test_that("renaming a selected feed updates its durable label", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  feed_id <- store$memory$feeds$feed_id[[1]]

  shiny::testServer(rill_server(config, store), {
    session$setInputs(select_feed = NULL)
    session$flushReact()
    session$setInputs(
      select_feed = list(id = feed_id, nonce = 1)
    )
    session$flushReact()
    session$setInputs(feed_title = "R news", rename_feed = 1L)
    session$flushReact()

    renamed <- store_list_feeds(store, config$actor_id)
    renamed <- renamed[renamed$feed_id == feed_id, , drop = FALSE]
    testthat::expect_identical(renamed$title, "R news")
    testthat::expect_identical(status_text(), "Renamed feed to R news")
    testthat::expect_identical(
      tail(store$memory$events$event_type, 1L),
      "feed_renamed"
    )
  })
})

testthat::test_that("changing story sort reorders the queue and clears the reader", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  newest_id <- store$memory$entries$entry_id[[1]]
  oldest_id <- store$memory$entries$entry_id[[6]]

  shiny::testServer(rill_server(config, store), {
    testthat::expect_identical(queue_entries()$entry_id[[1]], newest_id)

    session$setInputs(
      select_entry = list(id = newest_id, position = 1L, nonce = 1)
    )
    session$flushReact()
    session$setInputs(story_sort = "oldest")
    session$flushReact()

    testthat::expect_identical(queue_entries()$entry_id[[1]], oldest_id)
    testthat::expect_null(selected_id())
    testthat::expect_length(retained_ids(), 0L)
  })
})

testthat::test_that("calendar views filter the queue and clear the reader", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  recent_id <- store$memory$entries$entry_id[[1]]
  store$memory$entries$published_at <- format(
    Sys.time() - c(60, rep(60 * 60 * 24 * 40, 5)),
    tz = "UTC",
    usetz = TRUE
  )

  shiny::testServer(rill_server(config, store), {
    session$setInputs(
      select_entry = list(id = recent_id, position = 1L, nonce = 1)
    )
    session$flushReact()
    session$setInputs(view = "today")
    session$flushReact()

    testthat::expect_identical(queue_entries()$entry_id, recent_id)
    testthat::expect_null(selected_id())
    testthat::expect_length(retained_ids(), 0L)
  })
})

testthat::test_that("preparing today reports progress and records the result", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  progress_calls <- 0L
  testthat::local_mocked_bindings(
    prepare_today_documents = function(store, config, progress) {
      progress(1L, 2L, "First article")
      progress_calls <<- progress_calls + 1L
      list(
        total = 3L,
        cached = 1L,
        prepared = 2L,
        failed = 0L,
        errors = character()
      )
    }
  )

  shiny::testServer(rill_server(config, store), {
    session$setInputs(view = "today")
    session$flushReact()
    session$setInputs(prepare_today = 1L)
    session$flushReact()

    testthat::expect_identical(progress_calls, 1L)
    testthat::expect_identical(status_kind(), "success")
    testthat::expect_identical(
      status_text(),
      "Prepared 2 reading copies · 1 already ready"
    )
    testthat::expect_identical(
      tail(store$memory$events$event_type, 1L),
      "today_prepared"
    )
  })
})

testthat::test_that("client telemetry accepts only known event types", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)

  shiny::testServer(rill_server(config, store), {
    session$setInputs(client_event = NULL)
    session$flushReact()
    session$setInputs(client_event = list(type = "not-allowed", nonce = 1))
    session$flushReact()

    testthat::expect_equal(nrow(store$memory$events), 0L)
  })
})

testthat::test_that("uploading OPML reports the result and records an event", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  file <- withr::local_tempfile(
    fileext = ".opml",
    lines = paste0(
      "<opml version=\"2.0\"><head/><body>",
      "<outline type=\"rss\" text=\"Private\" ",
      "xmlUrl=\"http://127.0.0.1/feed.xml\"/>",
      "</body></opml>"
    )
  )
  upload <- data.frame(
    name = "subscriptions.opml",
    size = file.info(file)$size,
    type = "text/x-opml",
    datapath = file,
    stringsAsFactors = FALSE
  )

  shiny::testServer(rill_server(config, store), {
    session$setInputs(import_opml = NULL)
    session$flushReact()
    session$setInputs(import_opml = upload)
    session$flushReact()

    testthat::expect_identical(status_kind(), "error")
    testthat::expect_identical(status_text(), "1 feed skipped")
    testthat::expect_identical(refresh_tick(), 1L)
    testthat::expect_identical(
      store$memory$events$event_type,
      "opml_imported"
    )
  })
})

testthat::test_that("OPML import registers feeds before any refresh", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  file <- withr::local_tempfile(
    fileext = ".opml",
    lines = paste0(
      "<opml version=\"2.0\"><head/><body>",
      "<outline type=\"rss\" text=\"New feed\" ",
      "xmlUrl=\"https://example.com/feed.xml\"/>",
      "</body></opml>"
    )
  )
  upload <- data.frame(
    name = "subscriptions.opml",
    size = file.info(file)$size,
    type = "text/x-opml",
    datapath = file,
    stringsAsFactors = FALSE
  )
  refresh_calls <- 0L
  testthat::local_mocked_bindings(
    refresh_feed = function(...) {
      refresh_calls <<- refresh_calls + 1L
      cli::cli_abort("Refresh ran inline.")
    },
    .package = "rill"
  )

  shiny::testServer(rill_server(config, store), {
    session$setInputs(import_opml = NULL)
    session$flushReact()
    session$setInputs(import_opml = upload)
    session$flushReact()

    testthat::expect_identical(refresh_calls, 0L)
    testthat::expect_identical(status_kind(), "success")
    testthat::expect_match(status_text(), "Refresh feeds to fetch stories")
    testthat::expect_equal(nrow(store_list_feeds(store, config$actor_id)), 4L)
  })
})
