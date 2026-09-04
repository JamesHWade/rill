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
    reader_header <- paste(as.character(output$reader_header), collapse = "")
    testthat::expect_match(reader_header, "rillOpenQueue()", fixed = TRUE)
    testthat::expect_match(reader_header, "Queue", fixed = TRUE)

    session$setInputs(close_reader = list(nonce = 1))
    session$flushReact()

    testthat::expect_null(selected_id())
    testthat::expect_equal(nrow(entries()), 6L)
  })
})

testthat::test_that("Orientation queue status keeps rendering while covered", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  output_options <- list()
  test_session <- shiny::MockShinySession$new()
  test_session$outputOptions <- function(name, ...) {
    output_options[[name]] <<- list(...)
    invisible()
  }

  shiny::testServer(
    rill_server(config, store),
    session = test_session,
    {
      testthat::expect_identical(
        output_options$orientation_queue_status$suspendWhenHidden,
        FALSE
      )
    }
  )
})

testthat::test_that("Orientation opens and dismisses Documents with provenance", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  orientation <- store_get_orientation(store, config$actor_id)

  testthat::expect_s3_class(orientation, "rill_orientation")
  testthat::expect_length(orientation$cards, 2L)
  extracted <- FALSE
  testthat::local_mocked_bindings(
    get_or_extract_document = function(...) {
      extracted <<- TRUE
      stop("Orientation must open its pinned Document.")
    }
  )

  shiny::testServer(rill_server(config, store), {
    session$setInputs(select_entry = NULL, dismiss_orientation_card = NULL)
    session$flushReact()

    current <- orientation_state()
    testthat::expect_identical(current$due, FALSE)
    testthat::expect_length(current$candidates, 6L)

    first <- orientation$cards[[1L]]
    session$setInputs(
      select_entry = list(
        id = first$entry_id,
        position = 1L,
        surface = "orientation",
        orientation_id = orientation$orientation_id,
        revision_id = orientation$revision_id,
        card_id = first$card_id,
        document_id = first$document_id,
        basis_hash = first$basis_hash,
        rationale_hash = first$rationale_hash,
        nonce = 1
      )
    )
    session$flushReact()

    opened <- store$memory$events[nrow(store$memory$events), , drop = FALSE]
    testthat::expect_identical(opened$event_type, "entry_opened")
    testthat::expect_identical(opened$surface, "orientation")
    testthat::expect_identical(opened$entry_id, first$entry_id)
    testthat::expect_identical(
      selected_document()$document_id,
      first$document_id
    )
    testthat::expect_identical(extracted, FALSE)
    opened_payload <- jsonlite::fromJSON(opened$payload)
    testthat::expect_identical(opened_payload$card_id, first$card_id)
    testthat::expect_identical(
      opened_payload$revision_id,
      orientation$revision_id
    )
    testthat::expect_identical(
      opened_payload$document_id,
      first$document_id
    )
    testthat::expect_identical(opened_payload$entry_id, first$entry_id)
    testthat::expect_identical(
      opened_payload$agent_run_id,
      orientation$agent_run_id
    )
    testthat::expect_identical(opened_payload$role, first$role)
    testthat::expect_identical(opened_payload$frame, first$frame)
    testthat::expect_identical(
      opened_payload$interpretation,
      first$interpretation
    )
    testthat::expect_identical(opened_payload$why_now, first$why_now)
    testthat::expect_identical(opened_payload$evidence, first$evidence)
    testthat::expect_identical(
      opened_payload$boundary_hash,
      orientation$boundary$hash
    )
    testthat::expect_identical(
      opened_payload$policy_version,
      orientation$policy_version
    )

    second <- orientation$cards[[2L]]
    session$setInputs(
      dismiss_orientation_card = list(
        card_id = second$card_id,
        revision_id = orientation$revision_id,
        rationale_hash = second$rationale_hash,
        nonce = 2
      )
    )
    session$flushReact()

    stored <- store_get_orientation(store, config$actor_id)
    testthat::expect_identical(
      stored$dismissals[[1L]]$basis_hash,
      second$basis_hash
    )
    dismissed <- store$memory$events[nrow(store$memory$events), , drop = FALSE]
    testthat::expect_identical(
      dismissed$event_type,
      "orientation_card_dismissed"
    )
    testthat::expect_identical(dismissed$surface, "orientation")
    testthat::expect_identical(dismissed$entry_id, second$entry_id)
    payload <- jsonlite::fromJSON(dismissed$payload)
    testthat::expect_identical(payload$document_id, second$document_id)
    testthat::expect_identical(payload$basis_hash, second$basis_hash)
    testthat::expect_identical(
      payload$orientation_id,
      orientation$orientation_id
    )
    testthat::expect_identical(payload$revision_id, orientation$revision_id)
    testthat::expect_identical(
      payload$rationale_hash,
      stored$dismissals[[1L]]$rationale_hash
    )
  })
})

testthat::test_that("a failed Orientation event leaves the selection unopened", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  orientation <- store_get_orientation(store, config$actor_id)
  card <- orientation$cards[[1L]]
  orientation_entry_ids <- vapply(
    orientation$cards,
    `[[`,
    character(1),
    "entry_id"
  )
  story_id <- setdiff(store$memory$entries$entry_id, orientation_entry_ids)[[
    1L
  ]]
  original_record_event <- store_record_event
  testthat::local_mocked_bindings(
    store_record_event = function(store, event, ...) {
      if (identical(event$surface, "orientation")) {
        cli::cli_abort(
          "Forced Reading History failure.",
          class = "rill_test_event_failure"
        )
      }
      original_record_event(store, event, ...)
    }
  )

  shiny::testServer(rill_server(config, store), {
    session$setInputs(select_entry = NULL)
    session$flushReact()
    session$setInputs(
      select_entry = list(id = story_id, position = 3L, nonce = 1)
    )
    session$flushReact()

    testthat::expect_identical(selected_id(), story_id)
    testthat::expect_identical(selected_position(), 3L)
    session$setInputs(
      select_entry = list(
        id = card$entry_id,
        position = 1L,
        surface = "orientation",
        orientation_id = orientation$orientation_id,
        revision_id = orientation$revision_id,
        card_id = card$card_id,
        document_id = card$document_id,
        basis_hash = card$basis_hash,
        rationale_hash = card$rationale_hash,
        nonce = 2
      )
    )
    session$flushReact()

    testthat::expect_identical(selected_id(), story_id)
    testthat::expect_null(selected_document_id())
    testthat::expect_null(selected_orientation_provenance())
    testthat::expect_identical(selected_position(), 3L)
  })

  testthat::expect_equal(
    orientation_backend_entry_state(store, config$actor_id, card$entry_id),
    store$memory$state[
      0,
      c(
        "reader_id",
        "entry_id",
        "read_at",
        "read_reason",
        "last_opened_at"
      )
    ]
  )
  testthat::expect_equal(nrow(store$memory$events), 1L)
  testthat::expect_identical(store$memory$events$entry_id[[1L]], story_id)
})

testthat::test_that("an Orientation surface claim must identify a current card", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  orientation <- store_get_orientation(store, config$actor_id)
  card_entries <- vapply(
    orientation$cards,
    `[[`,
    character(1),
    "entry_id"
  )
  unselected <- setdiff(store$memory$entries$entry_id, card_entries)[[1L]]

  shiny::testServer(rill_server(config, store), {
    session$setInputs(select_entry = NULL)
    session$flushReact()
    session$setInputs(
      select_entry = list(
        id = unselected,
        position = 99L,
        surface = "orientation",
        nonce = 1
      )
    )
    session$flushReact()

    testthat::expect_null(selected_id())
    testthat::expect_equal(nrow(store$memory$events), 0L)
  })
})

testthat::test_that("Orientation actions revalidate current reading state", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  orientation <- store_get_orientation(store, config$actor_id)
  card <- orientation$cards[[1L]]

  shiny::testServer(rill_server(config, store), {
    session$setInputs(select_entry = NULL, dismiss_orientation_card = NULL)
    session$flushReact()
    initial_tick <- refresh_tick()

    store_mark_opened(store, config$actor_id, card$entry_id)
    session$setInputs(
      select_entry = list(
        id = card$entry_id,
        position = 1L,
        surface = "orientation",
        orientation_id = orientation$orientation_id,
        revision_id = orientation$revision_id,
        card_id = card$card_id,
        document_id = card$document_id,
        basis_hash = card$basis_hash,
        rationale_hash = card$rationale_hash,
        nonce = 1
      )
    )
    session$flushReact()

    testthat::expect_null(selected_id())
    testthat::expect_gt(refresh_tick(), initial_tick)

    before_dismissal <- refresh_tick()
    session$setInputs(
      dismiss_orientation_card = list(
        card_id = card$card_id,
        revision_id = orientation$revision_id,
        rationale_hash = card$rationale_hash,
        nonce = 2
      )
    )
    session$flushReact()

    testthat::expect_gt(refresh_tick(), before_dismissal)
    testthat::expect_length(
      store_get_orientation(store, config$actor_id)$dismissals,
      0L
    )
  })
})

testthat::test_that("an active response pins the selected Document identity", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  orientation <- store_get_orientation(store, config$actor_id)
  card <- orientation$cards[[1L]]

  shiny::testServer(rill_server(config, store), {
    session$setInputs(select_entry = NULL)
    session$flushReact()
    session$setInputs(
      select_entry = list(
        id = card$entry_id,
        position = 1L,
        surface = "orientation",
        orientation_id = orientation$orientation_id,
        revision_id = orientation$revision_id,
        card_id = card$card_id,
        document_id = card$document_id,
        basis_hash = card$basis_hash,
        rationale_hash = card$rationale_hash,
        nonce = 1
      )
    )
    session$flushReact()
    running <- list(run_id = "running-question", status = "running")
    active_agent_run(running)

    session$setInputs(
      select_entry = list(
        id = card$entry_id,
        position = 1L,
        surface = "story_list",
        nonce = 2
      )
    )
    session$flushReact()

    testthat::expect_identical(selected_document_id(), card$document_id)
    testthat::expect_identical(active_agent_run(), running)
  })
})

testthat::test_that("Orientation queue escape resets the unread scope", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  input_messages <- list()
  custom_messages <- character()

  shiny::testServer(rill_server(config, store), {
    session$sendInputMessage <- function(input_id, message) {
      input_messages[[input_id]] <<- message
    }
    session$sendCustomMessage <- function(type, message) {
      custom_messages <<- c(custom_messages, type)
    }
    session$setInputs(view = "saved", browse_orientation_queue = NULL)
    selected_feed(store$memory$feeds$feed_id[[1L]])
    session$flushReact()

    session$setInputs(browse_orientation_queue = list(nonce = 1))
    session$flushReact()

    testthat::expect_null(selected_feed())
    testthat::expect_identical(input_messages$view$value, "unread")
    testthat::expect_length(custom_messages, 0L)

    session$setInputs(view = "unread")
    session$flushReact()

    testthat::expect_identical(
      custom_messages,
      "rill-browse-queue-ready"
    )
  })
})

testthat::test_that("Orientation polling sees another session's dismissal", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  orientation <- store_get_orientation(store, config$actor_id)
  card <- orientation$cards[[1L]]

  shiny::testServer(rill_server(config, store), {
    session$flushReact()
    initial <- orientation_state()
    initial_tick <- refresh_tick()
    session$elapse(1000)
    session$flushReact()

    testthat::expect_identical(refresh_tick(), initial_tick)

    store_dismiss_orientation_card(
      store,
      config$actor_id,
      card$card_id,
      orientation$revision_id,
      card$rationale_hash
    )

    testthat::expect_length(initial$orientation$cards, 2L)
    session$elapse(1000)
    session$flushReact()

    current <- orientation_state()
    testthat::expect_length(current$orientation$cards, 1L)
    testthat::expect_gt(refresh_tick(), initial_tick)
  })
})

testthat::test_that("durable Orientation is prepared and launched only when enabled", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  config$demo_mode <- FALSE
  config$orientation_enabled <- TRUE
  config$agent_policy_url <- "https://provider.example/privacy"
  store <- rill_store(list(demo_mode = TRUE, actor_id = config$actor_id))
  confirm_test_orientation_destination(store, config)
  store$memory$orientations[[config$actor_id]] <- NULL
  store$memory$documents <- list()
  store$memory$document_heads <- character()
  calls <- 0L
  testthat::local_mocked_bindings(
    maintain_orientation_async = function(...) {
      calls <<- calls + 1L
      list(
        status = "current",
        orientation = NULL,
        boundary = list(hash = "prepared-boundary"),
        run = NULL,
        promise = NULL,
        deadline = NULL,
        interrupt = \(reason = "interrupted") invisible(NULL)
      )
    }
  )

  shiny::testServer(rill_server(config, store), {
    session$flushReact()

    testthat::expect_identical(calls, 1L)
    testthat::expect_identical(
      length(store$memory$document_heads),
      nrow(store$memory$entries)
    )
    testthat::expect_all_equal(
      vapply(store$memory$documents, `[[`, character(1), "producer"),
      "orientation-feed-copy"
    )
    testthat::expect_identical(orientation_preparing(), FALSE)
  })
})

testthat::test_that("ending a session leaves its running Orientation alive", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  config$demo_mode <- FALSE
  config$orientation_enabled <- TRUE
  config$agent_policy_url <- "https://provider.example/privacy"
  store <- rill_store(list(demo_mode = TRUE, actor_id = config$actor_id))
  confirm_test_orientation_destination(store, config)
  store$memory$orientations[[config$actor_id]] <- NULL
  resolve_run <- NULL
  interruptions <- character()
  testthat::local_mocked_bindings(
    rill_orientation_agent = function(...) {
      agent <- new.env(parent = emptyenv())
      agent$get_model <- \() "gpt-test"
      agent$get_provider <- \() stop("No provider object in this test.")
      orientation_test_tool_state(
        agent,
        list(
          status = "Nothing material has cleared the Orientation threshold.",
          cards = list()
        )
      )
      agent$run_async <- function(...) {
        promises::promise(function(resolve, reject) {
          resolve_run <<- resolve
        })
      }
      agent$interrupt <- function(reason) {
        interruptions <<- c(interruptions, reason)
        TRUE
      }
      agent
    }
  )

  shiny::testServer(rill_server(config, store), {
    session$flushReact()
    control <- orientation_control()
    run_id <- control$run$run_id

    testthat::expect_identical(control$status, "running")
    session$close()

    testthat::expect_length(interruptions, 0L)
    testthat::expect_identical(
      store_get_agent_run(store, config$actor_id, run_id)$status,
      "running"
    )

    resolve_run(orientation_test_agent_result(
      run_id = "deputy-orientation-after-reconnect"
    ))
    deadline <- Sys.time() + 2
    while (
      identical(
        store_get_agent_run(store, config$actor_id, run_id)$status,
        "running"
      ) &&
        Sys.time() < deadline
    ) {
      later::run_now(0.01)
    }
    completed <- store_get_agent_run(store, config$actor_id, run_id)
    testthat::expect_identical(completed$status, "completed")
    testthat::expect_identical(
      completed$deputy_run_id,
      "deputy-orientation-after-reconnect"
    )
  })
})

testthat::test_that("automatic Orientation retries once across sessions", {
  withr::local_envvar(DATABASE_URL = "")
  withr::local_seed(20260902)
  config <- rill_config()
  config$demo_mode <- FALSE
  config$orientation_enabled <- TRUE
  config$agent_policy_url <- "https://provider.example/privacy"
  store <- rill_store(list(demo_mode = TRUE, actor_id = config$actor_id))
  confirm_test_orientation_destination(store, config)
  store$memory$orientations[[config$actor_id]] <- NULL
  initial_run_ids <- names(store$memory$agent_runs)
  provider_calls <- 0L
  testthat::local_mocked_bindings(
    rill_orientation_agent = function(...) {
      agent <- new.env(parent = emptyenv())
      agent$get_model <- \() "gpt-test"
      agent$get_provider <- \() stop("No provider object in this test.")
      orientation_test_tool_state(agent)
      agent$run_async <- function(...) {
        provider_calls <<- provider_calls + 1L
        promises::promise_reject(simpleError("Provider unavailable."))
      }
      agent$interrupt <- \(reason) TRUE
      agent
    }
  )

  for (index in seq_len(3L)) {
    shiny::testServer(rill_server(config, store), {
      session$flushReact()
      deadline <- Sys.time() + 2
      active <- TRUE
      while (isTRUE(active) && Sys.time() < deadline) {
        later::run_now(0.01)
        active <- any(vapply(
          store$memory$agent_runs,
          \(run) run$status %in% c("pending", "running", "cancelling"),
          logical(1)
        ))
      }
      testthat::expect_identical(active, FALSE)
    })
  }

  runs <- unname(store$memory$agent_runs[
    setdiff(names(store$memory$agent_runs), initial_run_ids)
  ])
  original <- Filter(\(run) is.null(run$retry_of_run_id), runs)[[1L]]
  retry <- Filter(\(run) !is.null(run$retry_of_run_id), runs)[[1L]]

  testthat::expect_length(runs, 2L)
  testthat::expect_identical(provider_calls, 2L)
  testthat::expect_all_equal(
    vapply(runs, `[[`, character(1), "status"),
    "failed"
  )
  testthat::expect_identical(retry$retry_of_run_id, original$run_id)
  testthat::expect_identical(
    retry$request_key,
    orientation_retry_request_key(
      original$request_key,
      orientation_automatic_retry_id()
    )
  )
})

testthat::test_that("automatic Orientation remains off without confirmation", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  config$demo_mode <- FALSE
  config$orientation_enabled <- TRUE
  config$agent_policy_url <- "https://provider.example/privacy"
  store <- rill_store(list(demo_mode = TRUE, actor_id = config$actor_id))
  store$memory$orientations[[config$actor_id]] <- NULL
  store$memory$documents <- list()
  store$memory$document_heads <- character()
  calls <- 0L
  testthat::local_mocked_bindings(
    maintain_orientation_async = function(...) {
      calls <<- calls + 1L
      stop("Automatic Orientation was not confirmed.")
    }
  )

  shiny::testServer(rill_server(config, store), {
    session$flushReact()

    testthat::expect_identical(calls, 0L)
    testthat::expect_length(store$memory$documents, 0L)
  })
})

testthat::test_that("confirming the Data Destination starts Orientation", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  config$demo_mode <- FALSE
  config$orientation_enabled <- TRUE
  config$agent_policy_url <- "https://provider.example/privacy"
  store <- rill_store(list(demo_mode = TRUE, actor_id = config$actor_id))
  store$memory$orientations[[config$actor_id]] <- NULL
  calls <- 0L
  testthat::local_mocked_bindings(
    maintain_orientation_async = function(...) {
      calls <<- calls + 1L
      list(
        status = "current",
        orientation = NULL,
        boundary = list(hash = "confirmed-boundary"),
        run = NULL,
        promise = NULL,
        deadline = NULL,
        interrupt = \(reason = "interrupted") invisible(NULL)
      )
    }
  )

  shiny::testServer(rill_server(config, store), {
    session$setInputs(
      orientation_enable = NULL,
      orientation_confirm = NULL
    )
    session$flushReact()
    testthat::expect_identical(calls, 0L)

    session$setInputs(orientation_enable = 1L)
    session$flushReact()
    testthat::expect_identical(calls, 0L)
    testthat::expect_identical(
      orientation_destination_status()$needs_confirmation,
      TRUE
    )

    session$setInputs(orientation_confirm = 1L)
    session$flushReact()
    testthat::expect_identical(calls, 1L)
    testthat::expect_identical(
      orientation_destination_status()$enabled,
      TRUE
    )
  })
})

testthat::test_that("disabling Orientation cooperatively cancels active work", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  config$demo_mode <- FALSE
  config$orientation_enabled <- TRUE
  config$agent_policy_url <- "https://provider.example/privacy"
  store <- rill_store(list(demo_mode = TRUE, actor_id = config$actor_id))
  confirm_test_orientation_destination(store, config)
  active <- store_start_agent_run(
    store,
    reader_id = config$actor_id,
    kind = "orientation",
    request_key = "orientation-before-disable",
    pinned_inputs = list(boundary_hash = "boundary-before-disable"),
    worker_id = "orientation-worker"
  )
  active <- store_claim_agent_run(
    store,
    reader_id = config$actor_id,
    run_id = active$run_id,
    worker_id = "orientation-worker",
    lease_expires_at = Sys.time() + 120
  )
  signalled <- NULL
  testthat::local_mocked_bindings(
    rill_signal_orientation_interrupt = function(run_id, reason) {
      signalled <<- list(run_id = run_id, reason = reason)
      TRUE
    }
  )

  shiny::testServer(rill_server(config, store), {
    selected_id(store$memory$documents[[1L]]$entry_id)
    session$flushReact()
    session$setInputs(orientation_disable = 1L)
    session$flushReact()

    testthat::expect_identical(
      orientation_destination_status()$enabled,
      FALSE
    )
    cancelling <- store_get_agent_run(store, config$actor_id, active$run_id)
    testthat::expect_identical(cancelling$status, "cancelling")
    testthat::expect_null(cancelling$terminal_at)
    testthat::expect_identical(
      signalled,
      list(run_id = active$run_id, reason = "orientation_disabled")
    )
  })
})

testthat::test_that("a session observes an external Orientation disable", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  config$demo_mode <- FALSE
  config$orientation_enabled <- TRUE
  config$agent_policy_url <- "https://provider.example/privacy"
  store <- rill_store(list(demo_mode = TRUE, actor_id = config$actor_id))
  confirm_test_orientation_destination(store, config)
  calls <- 0L
  testthat::local_mocked_bindings(
    maintain_orientation_async = function(...) {
      calls <<- calls + 1L
      stop("Disabled Orientation must not restart.")
    }
  )

  shiny::testServer(rill_server(config, store), {
    selected_id(store$memory$documents[[1L]]$entry_id)
    session$flushReact()
    testthat::expect_identical(orientation_destination_status()$enabled, TRUE)

    set_orientation_enabled(
      store,
      config$actor_id,
      enabled = FALSE,
      config = config
    )
    session$elapse(1000)
    session$flushReact()

    testthat::expect_identical(orientation_destination_status()$enabled, FALSE)
    selected_id(NULL)
    session$flushReact()
    testthat::expect_identical(calls, 0L)
  })
})

testthat::test_that("an external disable blocks the next provider call", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  config$demo_mode <- FALSE
  config$orientation_enabled <- TRUE
  config$agent_policy_url <- "https://provider.example/privacy"
  store <- rill_store(list(demo_mode = TRUE, actor_id = config$actor_id))
  store$memory$orientations[[config$actor_id]] <- NULL
  confirm_test_orientation_destination(store, config)
  provider_calls <- 0L
  testthat::local_mocked_bindings(
    rill_orientation_agent = function(...) {
      agent <- new.env(parent = emptyenv())
      agent$get_model <- \() "gpt-test"
      agent$get_provider <- \() stop("No provider object in this test.")
      orientation_test_tool_state(agent)
      agent$run_async <- function(...) {
        provider_calls <<- provider_calls + 1L
        promises::promise_reject(simpleError("Provider must not be called."))
      }
      agent$interrupt <- \(reason) TRUE
      agent
    }
  )

  shiny::testServer(rill_server(config, store), {
    selected_id(store$memory$documents[[1L]]$entry_id)
    session$flushReact()

    set_orientation_enabled(
      store,
      config$actor_id,
      enabled = FALSE,
      config = config
    )
    selected_id(NULL)
    session$flushReact()

    testthat::expect_identical(provider_calls, 0L)
  })
})

testthat::test_that("preparation failure leaves the current Orientation usable", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  config$demo_mode <- FALSE
  config$orientation_enabled <- TRUE
  config$agent_policy_url <- "https://provider.example/privacy"
  store <- rill_store(list(demo_mode = TRUE, actor_id = config$actor_id))
  confirm_test_orientation_destination(store, config)
  expected <- store_get_orientation(store, config$actor_id)
  calls <- 0L
  testthat::local_mocked_bindings(
    prepare_orientation_documents = function(...) {
      cli::cli_abort("Reading-copy preparation unavailable.")
    },
    maintain_orientation_async = function(...) {
      calls <<- calls + 1L
      stop("Maintenance must not start after preparation fails.")
    },
    telemetry_log = \(...) invisible(NULL)
  )

  shiny::testServer(rill_server(config, store), {
    session$flushReact()

    current <- orientation_state()
    testthat::expect_identical(
      current$orientation$revision_id,
      expected$revision_id
    )
    testthat::expect_identical(calls, 0L)
    testthat::expect_identical(orientation_preparing(), FALSE)
  })
})

testthat::test_that("busy Orientation maintenance remains retryable", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  config$demo_mode <- FALSE
  config$orientation_enabled <- TRUE
  config$agent_policy_url <- "https://provider.example/privacy"
  store <- rill_store(list(demo_mode = TRUE, actor_id = config$actor_id))
  confirm_test_orientation_destination(store, config)
  store$memory$orientations[[config$actor_id]] <- NULL
  calls <- 0L
  testthat::local_mocked_bindings(
    maintain_orientation_async = function(...) {
      calls <<- calls + 1L
      list(
        status = if (calls == 1L) "busy" else "current",
        orientation = NULL,
        boundary = list(hash = "busy-boundary"),
        run = NULL,
        promise = NULL,
        deadline = NULL,
        interrupt = \(reason = "interrupted") invisible(NULL)
      )
    }
  )

  shiny::testServer(rill_server(config, store), {
    session$flushReact()
    testthat::expect_identical(calls, 1L)
    testthat::expect_identical(
      orientation_attempted_boundary(),
      orientation_status(store, config$actor_id)$boundary$hash
    )

    orientation_attempted_boundary(NULL)
    start_orientation_maintenance()

    testthat::expect_identical(calls, 2L)
    testthat::expect_identical(orientation_preparing(), FALSE)
  })
})

testthat::test_that("a late Orientation settlement cannot clear its successor", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  config$demo_mode <- FALSE
  config$orientation_enabled <- TRUE
  config$agent_policy_url <- "https://provider.example/privacy"
  store <- rill_store(list(demo_mode = TRUE, actor_id = config$actor_id))
  confirm_test_orientation_destination(store, config)
  store$memory$orientations[[config$actor_id]] <- NULL
  resolvers <- list()
  calls <- 0L
  testthat::local_mocked_bindings(
    maintain_orientation_async = function(...) {
      calls <<- calls + 1L
      promise <- promises::promise(function(resolve, reject) {
        resolvers[[calls]] <<- resolve
      })
      list(
        status = "running",
        orientation = NULL,
        boundary = list(hash = paste0("boundary-", calls)),
        run = list(run_id = paste0("orientation-run-", calls)),
        promise = promise,
        deadline = Sys.time() + 120,
        interrupt = \(reason = "interrupted") invisible(NULL)
      )
    }
  )

  shiny::testServer(rill_server(config, store), {
    session$flushReact()
    first <- orientation_control()
    testthat::expect_identical(calls, 1L)

    orientation_control(NULL)
    orientation_preparing(FALSE)
    orientation_attempted_boundary(NULL)
    start_orientation_maintenance()
    second <- orientation_control()
    testthat::expect_identical(calls, 2L)
    testthat::expect_identical(
      identical(first$session_token, second$session_token),
      FALSE
    )

    resolvers[[1L]](list())
    later::run_now(0.01)

    testthat::expect_identical(
      orientation_control()$session_token,
      second$session_token
    )
    testthat::expect_identical(orientation_preparing(), TRUE)
    resolvers[[2L]](list())
    later::run_now(0.01)
  })
})

testthat::test_that("a Reader question waits for Orientation to stop", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  document <- store$memory$documents[[1L]]
  orientation_run <- store_start_agent_run(
    store,
    reader_id = config$actor_id,
    kind = "orientation",
    request_key = "remote-orientation",
    pinned_inputs = list(boundary_hash = "remote-boundary"),
    worker_id = "other-session"
  )
  orientation_run <- store_claim_agent_run(
    store,
    reader_id = config$actor_id,
    run_id = orientation_run$run_id,
    worker_id = "other-session",
    lease_expires_at = Sys.time() + 120
  )
  resolve_orientation <- NULL
  interruption <- NULL
  appended <- character()
  get_deferred_reader_question <- store_get_deferred_reader_question
  get_agent_run <- store_get_agent_run
  deferred_read <- new.env(parent = emptyenv())
  deferred_read$fail <- FALSE
  deferred_read$failures <- 0L
  agent_run_read <- new.env(parent = emptyenv())
  agent_run_read$fail <- FALSE
  agent_run_read$failures <- 0L
  testthat::local_mocked_bindings(
    rill_reader_agent = function(..., on_stop) {
      list(stream_async = function(prompt, stream, run_context) {
        list(consume = function() {
          on_stop(
            "complete",
            list(
              usage = list(requests = 1L),
              run_context = run_context,
              run_id = "deputy-question-after-orientation"
            )
          )
          "Answer after Orientation stopped."
        })
      })
    },
    append_reader_chat = function(response, session) {
      value <- if (is.character(response)) response else response$consume()
      appended <<- c(appended, value)
      promises::promise_resolve(value)
    },
    store_get_deferred_reader_question = function(...) {
      if (isTRUE(deferred_read$fail)) {
        deferred_read$failures <- deferred_read$failures + 1L
        cli::cli_abort(
          "The deferred question read failed.",
          class = "test_database_error"
        )
      }
      get_deferred_reader_question(...)
    },
    store_get_agent_run = function(...) {
      if (isTRUE(agent_run_read$fail)) {
        agent_run_read$failures <- agent_run_read$failures + 1L
        cli::cli_abort(
          "The Agent Run read failed.",
          class = "test_database_error"
        )
      }
      get_agent_run(...)
    }
  )

  shiny::testServer(rill_server(config, store), {
    session$setInputs(select_entry = NULL)
    session$flushReact()
    selected_id(document$entry_id)
    selected_document_id(document$document_id)
    promise <- promises::promise(function(resolve, reject) {
      resolve_orientation <<- resolve
    })
    promise <- promises::then(promise, function(value) {
      store_finish_agent_run(
        store,
        reader_id = config$actor_id,
        run_id = orientation_run$run_id,
        worker_id = "other-session",
        status = "cancelled",
        terminal_reason = "reader_question"
      )
      value
    })
    orientation_control(list(
      status = "running",
      promise = promise,
      interrupt = function(reason) {
        interruption <<- reason
        list(run = list(status = "cancelling"), interrupted = TRUE)
      }
    ))

    run_prioritized_reader_question(
      "What changed?",
      document,
      request_token = "question-1"
    )
    testthat::expect_identical(interruption, "reader_question")
    runs <- Filter(
      \(run) !identical(run$terminal_reason, "bundled_demo"),
      unname(store$memory$agent_runs)
    )
    testthat::expect_length(runs, 1L)
    testthat::expect_identical(
      store_get_agent_run(
        store,
        config$actor_id,
        orientation_run$run_id
      )$status,
      "cancelling"
    )
    testthat::expect_null(
      store_get_agent_run(
        store,
        config$actor_id,
        orientation_run$run_id
      )$terminal_at
    )
    testthat::expect_identical(
      pending_reader_question()$request_key,
      rill_id("ask-rill", session_id, "question-1")
    )
    testthat::expect_identical(
      store_get_deferred_reader_question(store, config$actor_id)$request_key,
      pending_reader_question()$request_key
    )
    testthat::expect_length(appended, 0L)

    deferred_read$fail <- TRUE
    deadline <- Sys.time() + 1
    while (deferred_read$failures == 0L && Sys.time() < deadline) {
      later::run_now(0.05)
    }
    deferred_read$fail <- FALSE
    testthat::expect_gte(deferred_read$failures, 1L)
    testthat::expect_identical(
      pending_reader_question()$request_key,
      rill_id("ask-rill", session_id, "question-1")
    )
    testthat::expect_identical(
      get_deferred_reader_question(store, config$actor_id)$request_key,
      pending_reader_question()$request_key
    )

    agent_run_read$fail <- TRUE
    deadline <- Sys.time() + 1
    while (agent_run_read$failures == 0L && Sys.time() < deadline) {
      later::run_now(0.05)
    }
    agent_run_read$fail <- FALSE
    testthat::expect_gte(agent_run_read$failures, 1L)
    testthat::expect_identical(
      pending_reader_question()$request_key,
      rill_id("ask-rill", session_id, "question-1")
    )
    testthat::expect_identical(
      get_deferred_reader_question(store, config$actor_id)$request_key,
      pending_reader_question()$request_key
    )

    other_entry_id <- store$memory$entries$entry_id[[2L]]
    session$setInputs(
      select_entry = list(id = other_entry_id, position = 2L, nonce = 2L)
    )
    session$flushReact()
    testthat::expect_identical(selected_id(), document$entry_id)
    testthat::expect_identical(
      selected_document_id(),
      document$document_id
    )

    pending <- pending_reader_question()
    store_finish_agent_run(
      store,
      reader_id = config$actor_id,
      run_id = orientation_run$run_id,
      worker_id = "other-session",
      status = "cancelled",
      terminal_reason = "reader_question"
    )
    adopted <- store_start_prioritized_reader_question(
      store,
      reader_id = config$actor_id,
      request_key = pending$request_key,
      pinned_inputs = pending$pinned_inputs,
      requested_at = pending$requested_at,
      worker_id = "other-question-session",
      transition_at = Sys.time()
    )$run
    adopted <- store_claim_agent_run(
      store,
      reader_id = config$actor_id,
      run_id = adopted$run_id,
      worker_id = "other-question-session",
      lease_expires_at = Sys.time() + 120
    )
    deadline <- Sys.time() + 2
    while (
      (is.null(active_agent_run()) ||
        !identical(active_agent_run()$run_id, adopted$run_id)) &&
        Sys.time() < deadline
    ) {
      later::run_now(0.05)
    }

    testthat::expect_null(pending_reader_question())
    testthat::expect_identical(active_agent_run()$status, "running")
    testthat::expect_identical(reader_response_in_flight(), TRUE)
    testthat::expect_identical(selected_id(), document$entry_id)
    store_record_agent_run_response(
      store,
      reader_id = config$actor_id,
      run_id = adopted$run_id,
      worker_id = "other-question-session",
      response_text = "Answer from the winning session."
    )
    store_finish_agent_run(
      store,
      reader_id = config$actor_id,
      run_id = adopted$run_id,
      worker_id = "other-question-session",
      status = "completed",
      terminal_reason = "complete"
    )
    deadline <- Sys.time() + 3
    while (
      !"Answer from the winning session." %in% appended &&
        Sys.time() < deadline
    ) {
      later::run_now(0.05)
    }

    resolve_orientation(list())
    later::run_now(0.01)
    testthat::expect_null(
      store_get_deferred_reader_question(store, config$actor_id)
    )
    testthat::expect_identical(
      store_get_agent_run(
        store,
        config$actor_id,
        orientation_run$run_id
      )$status,
      "cancelled"
    )
    testthat::expect_identical(active_agent_run()$status, "completed")
    testthat::expect_in("Answer from the winning session.", appended)
  })
})

testthat::test_that("a replacement session resumes a deferred question", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  document <- store$memory$documents[[1L]]
  orientation <- store_start_agent_run(
    store,
    reader_id = config$actor_id,
    kind = "orientation",
    request_key = "orientation-before-reconnect",
    pinned_inputs = list(boundary_hash = "boundary-before-reconnect"),
    worker_id = "orientation-worker"
  )
  orientation <- store_claim_agent_run(
    store,
    reader_id = config$actor_id,
    run_id = orientation$run_id,
    worker_id = "orientation-worker",
    lease_expires_at = Sys.time() + 120
  )
  pinned_inputs <- list(
    submission_id = "deferred-question",
    entry_id = document$entry_id,
    document_id = document$document_id,
    document_content_hash = document$content_hash,
    document_record_hash = document$record_hash,
    research_scope = list(
      kind = "selected_document",
      document_ids = document$document_id
    ),
    data_destination = "OpenAI at api.openai.com",
    data_destination_id = rill_agent_data_destination_details("openai")$id,
    question = "What changed?",
    model = "openai",
    policy_version = "ask-rill-v1",
    limits = rill_agent_run_limits()
  )
  store_start_prioritized_reader_question(
    store,
    reader_id = config$actor_id,
    request_key = "deferred-question",
    pinned_inputs = pinned_inputs,
    worker_id = "departed-session"
  )
  store_finish_agent_run(
    store,
    reader_id = config$actor_id,
    run_id = orientation$run_id,
    worker_id = "orientation-worker",
    status = "cancelled",
    terminal_reason = "reader_question"
  )
  appended <- character()
  get_deferred_reader_question <- store_get_deferred_reader_question
  get_document_by_id <- store_get_document_by_id
  deferred_reads <- 0L
  deferred_read_failures_remaining <- 3L
  document_reads <- 0L
  document_read_failures_remaining <- 1L
  testthat::local_mocked_bindings(
    rill_reader_agent = function(on_stop, ...) {
      list(stream_async = function(prompt, stream, run_context) {
        list(consume = function() {
          on_stop(
            "complete",
            list(
              usage = list(requests = 1L),
              run_context = run_context,
              run_id = "deputy-resumed-question"
            )
          )
          "Resumed answer."
        })
      })
    },
    append_reader_chat = function(response, session) {
      value <- response$consume()
      appended <<- c(appended, value)
      promises::promise_resolve(value)
    },
    store_get_deferred_reader_question = function(...) {
      deferred_reads <<- deferred_reads + 1L
      if (deferred_read_failures_remaining > 0L) {
        deferred_read_failures_remaining <<-
          deferred_read_failures_remaining - 1L
        cli::cli_abort(
          "The startup deferred question read failed.",
          class = "test_database_error"
        )
      }
      get_deferred_reader_question(...)
    },
    store_get_document_by_id = function(...) {
      document_reads <<- document_reads + 1L
      if (document_read_failures_remaining > 0L) {
        document_read_failures_remaining <<-
          document_read_failures_remaining - 1L
        cli::cli_abort(
          "The deferred question document read failed.",
          class = "test_database_error"
        )
      }
      get_document_by_id(...)
    }
  )

  shiny::testServer(rill_server(config, store), {
    session$flushReact()
    deadline <- Sys.time() + 2
    while (
      (is.null(active_agent_run()) ||
        !identical(active_agent_run()$status, "completed")) &&
        Sys.time() < deadline
    ) {
      later::run_now(0.01)
      session$flushReact()
    }

    testthat::expect_identical(selected_id(), document$entry_id)
    testthat::expect_gte(deferred_reads, 4L)
    testthat::expect_gte(document_reads, 2L)
    testthat::expect_identical(active_agent_run()$status, "completed")
    testthat::expect_identical(
      active_agent_run()$request_key,
      "deferred-question"
    )
    testthat::expect_null(
      store_get_deferred_reader_question(store, config$actor_id)
    )
    testthat::expect_identical(appended, "Resumed answer.")
  })
})

testthat::test_that("a replacement session restores a completed question", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  config$orientation_enabled <- FALSE
  store <- rill_store(config)
  document <- store$memory$documents[[1L]]
  run <- store_start_agent_run(
    store,
    reader_id = config$actor_id,
    kind = "question",
    request_key = "completed-before-reconnect",
    pinned_inputs = list(document_id = document$document_id)
  )
  run <- store_claim_agent_run(
    store,
    reader_id = config$actor_id,
    run_id = run$run_id,
    worker_id = "departed-session",
    lease_expires_at = Sys.time() + 120
  )
  store_record_agent_run_response(
    store,
    reader_id = config$actor_id,
    run_id = run$run_id,
    worker_id = "departed-session",
    response_text = "Answer completed before reconnection."
  )
  store_finish_agent_run(
    store,
    reader_id = config$actor_id,
    run_id = run$run_id,
    worker_id = "departed-session",
    status = "completed",
    terminal_reason = "complete"
  )
  entry_index <- match(document$entry_id, store$memory$entries$entry_id)
  feed_id <- store$memory$entries$feed_id[[entry_index]]
  store_unsubscribe_feed(store, config$actor_id, feed_id)
  testthat::expect_null(
    store_get_entry(store, config$actor_id, document$entry_id)
  )
  appended <- character()
  testthat::local_mocked_bindings(
    rill_reader_agent = function(...) {
      list(get_model = \() config$agent_model)
    },
    append_reader_chat = function(response, session) {
      appended <<- c(appended, response)
      promises::promise_resolve(response)
    }
  )

  shiny::testServer(rill_server(config, store), {
    session$flushReact()
    deadline <- Sys.time() + 3
    while (length(appended) == 0L && Sys.time() < deadline) {
      later::run_now(0.05)
      session$flushReact()
    }

    testthat::expect_identical(active_agent_run()$run_id, run$run_id)
    testthat::expect_identical(active_agent_run()$status, "completed")
    testthat::expect_identical(selected_id(), document$entry_id)
    testthat::expect_identical(
      selected_document()$document_id,
      document$document_id
    )
    reader_header <- as.character(output$reader_header)
    testthat::expect_no_match(reader_header, 'id="mark_unread"', fixed = TRUE)
    testthat::expect_no_match(reader_header, 'id="toggle_star"', fixed = TRUE)
    testthat::expect_no_match(reader_header, 'id="toggle_save"', fixed = TRUE)
    testthat::expect_identical(
      appended,
      "Answer completed before reconnection."
    )
  })
})

testthat::test_that("a replacement session stops polling a legacy response", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  config$orientation_enabled <- FALSE
  store <- rill_store(config)
  document <- store$memory$documents[[1L]]
  run <- store_start_agent_run(
    store,
    reader_id = config$actor_id,
    kind = "question",
    request_key = "legacy-completed-question",
    pinned_inputs = list(document_id = document$document_id)
  )
  run <- store_claim_agent_run(
    store,
    reader_id = config$actor_id,
    run_id = run$run_id,
    worker_id = "legacy-worker",
    lease_expires_at = Sys.time() + 120
  )
  store_finish_agent_run(
    store,
    reader_id = config$actor_id,
    run_id = run$run_id,
    worker_id = "legacy-worker",
    status = "completed",
    terminal_reason = "complete",
    finished_at = Sys.time() - 10
  )
  get_agent_run <- store_get_agent_run
  poll_reads <- 0L
  appended <- character()
  testthat::local_mocked_bindings(
    rill_reader_agent = function(...) {
      list(get_model = \() config$agent_model)
    },
    store_get_agent_run = function(store, reader_id, run_id) {
      if (identical(run_id, run$run_id)) {
        poll_reads <<- poll_reads + 1L
      }
      get_agent_run(store, reader_id, run_id)
    },
    append_reader_chat = function(response, session) {
      appended <<- c(appended, response)
      promises::promise_resolve(response)
    }
  )

  shiny::testServer(rill_server(config, store), {
    session$flushReact()
    deadline <- Sys.time() + 1
    while (poll_reads == 0L && Sys.time() < deadline) {
      later::run_now(0.05)
      session$flushReact()
    }
    observed_reads <- poll_reads
    settle_deadline <- Sys.time() + 0.6
    while (Sys.time() < settle_deadline) {
      later::run_now(0.05)
      session$flushReact()
    }

    testthat::expect_gte(observed_reads, 1L)
    testthat::expect_identical(poll_reads, observed_reads)
    testthat::expect_identical(
      completed_response_may_arrive(active_agent_run()),
      FALSE
    )
    testthat::expect_length(appended, 0L)
  })
})

testthat::test_that("a replacement session polls a promoted partial response", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  document <- store$memory$documents[[1L]]
  orientation <- store_start_agent_run(
    store,
    reader_id = config$actor_id,
    kind = "orientation",
    request_key = "orientation-before-adopted-question",
    pinned_inputs = list(boundary_hash = "boundary-before-adopted-question"),
    worker_id = "orientation-worker"
  )
  store_claim_agent_run(
    store,
    reader_id = config$actor_id,
    run_id = orientation$run_id,
    worker_id = "orientation-worker",
    lease_expires_at = Sys.time() + 120
  )
  request_key <- "adopted-question"
  pinned_inputs <- list(
    submission_id = request_key,
    entry_id = document$entry_id,
    document_id = document$document_id,
    document_content_hash = document$content_hash,
    document_record_hash = document$record_hash,
    research_scope = list(
      kind = "selected_document",
      document_ids = document$document_id
    ),
    data_destination = "OpenAI at api.openai.com",
    data_destination_id = rill_agent_data_destination_details("openai")$id,
    question = "What changed?",
    model = config$agent_model,
    policy_version = "ask-rill-v1",
    limits = rill_agent_run_limits()
  )
  store_start_prioritized_reader_question(
    store,
    reader_id = config$actor_id,
    request_key = request_key,
    pinned_inputs = pinned_inputs,
    worker_id = "departed-session"
  )
  adopted <- list(
    run_id = rill_id("agent-run", config$actor_id, request_key),
    reader_id = config$actor_id,
    kind = "question",
    request_key = request_key,
    status = "running",
    pinned_inputs = pinned_inputs
  )
  completed <- adopted
  completed$status <- "completed"
  completed$terminal_at <- Sys.time()
  completed$response_text <- "An incomplete answer"
  completed_with_response <- completed
  completed_with_response$response_text <-
    "Answer from the owning session."
  poll_reads <- 0L
  appended <- character()
  real_get_agent_run <- store_get_agent_run
  testthat::local_mocked_bindings(
    rill_reader_agent = function(...) {
      list(get_model = \() config$agent_model)
    },
    store_start_prioritized_reader_question = function(...) {
      store_delete_deferred_reader_question(
        store,
        config$actor_id,
        request_key
      )
      list(
        run = adopted,
        preempted = NULL,
        deferred = NULL,
        orientation_signalled = FALSE
      )
    },
    store_get_agent_run = function(store, reader_id, run_id) {
      if (!identical(run_id, adopted$run_id)) {
        return(real_get_agent_run(store, reader_id, run_id))
      }
      poll_reads <<- poll_reads + 1L
      if (poll_reads < 2L) {
        adopted
      } else if (poll_reads == 2L) {
        completed
      } else {
        completed_with_response
      }
    },
    append_reader_chat = function(response, session) {
      appended <<- c(appended, response)
      promises::promise_resolve(response)
    }
  )

  shiny::testServer(rill_server(config, store), {
    session$flushReact()
    deadline <- Sys.time() + 3
    while (
      length(appended) == 0L &&
        Sys.time() < deadline
    ) {
      later::run_now(0.05)
      session$flushReact()
    }

    testthat::expect_gte(poll_reads, 3L)
    testthat::expect_identical(active_agent_run()$status, "completed")
    testthat::expect_identical(
      appended,
      "Answer from the owning session."
    )
  })
})

testthat::test_that("preserved questions require their runtime identity", {
  pinned <- list(
    model = "gpt-accepted",
    data_destination = "Provider at accepted.example",
    data_destination_id = "destination-accepted"
  )
  testthat::expect_no_error(rill_assert_question_runtime_identity(
    pinned,
    pinned
  ))
  testthat::expect_error(
    rill_assert_question_runtime_identity(
      pinned,
      list(
        model = "gpt-changed",
        data_destination = pinned$data_destination,
        data_destination_id = pinned$data_destination_id
      )
    ),
    class = "rill_agent_runtime_identity_changed"
  )
  testthat::expect_error(
    rill_assert_question_runtime_identity(
      pinned,
      list(
        model = pinned$model,
        data_destination = "Provider at changed.example",
        data_destination_id = "destination-changed"
      )
    ),
    class = "rill_agent_runtime_identity_changed"
  )
  testthat::expect_error(
    rill_assert_question_runtime_identity(
      pinned,
      list(
        model = pinned$model,
        data_destination = pinned$data_destination,
        data_destination_id = "destination-changed"
      )
    ),
    class = "rill_agent_runtime_identity_changed"
  )
})

testthat::test_that("a deferred question rejects a changed destination", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  document <- store$memory$documents[[1L]]
  orientation <- store_start_agent_run(
    store,
    reader_id = config$actor_id,
    kind = "orientation",
    request_key = "orientation-before-destination-change",
    pinned_inputs = list(boundary_hash = "boundary-before-destination-change"),
    worker_id = "orientation-worker"
  )
  orientation <- store_claim_agent_run(
    store,
    reader_id = config$actor_id,
    run_id = orientation$run_id,
    worker_id = "orientation-worker",
    lease_expires_at = Sys.time() + 120
  )
  request_key <- "deferred-destination-change"
  pinned_inputs <- list(
    submission_id = request_key,
    entry_id = document$entry_id,
    document_id = document$document_id,
    document_content_hash = document$content_hash,
    document_record_hash = document$record_hash,
    research_scope = list(
      kind = "selected_document",
      document_ids = document$document_id
    ),
    data_destination = "OpenAI at api.openai.com",
    data_destination_id = "agent-data-destination-old-endpoint",
    question = "What changed?",
    model = config$agent_model,
    policy_version = "ask-rill-v1",
    limits = rill_agent_run_limits()
  )
  store_start_prioritized_reader_question(
    store,
    reader_id = config$actor_id,
    request_key = request_key,
    pinned_inputs = pinned_inputs,
    worker_id = "departed-session"
  )
  store_finish_agent_run(
    store,
    reader_id = config$actor_id,
    run_id = orientation$run_id,
    worker_id = "orientation-worker",
    status = "cancelled",
    terminal_reason = "reader_question"
  )
  stream_calls <- 0L
  appended <- character()
  testthat::local_mocked_bindings(
    rill_reader_agent = function(...) {
      list(
        get_model = \() config$agent_model,
        stream_async = function(...) {
          stream_calls <<- stream_calls + 1L
          stop("The changed destination must not receive the question.")
        }
      )
    },
    append_reader_chat = function(response, session) {
      appended <<- c(appended, response)
      promises::promise_resolve(response)
    }
  )

  shiny::testServer(rill_server(config, store), {
    session$flushReact()

    testthat::expect_identical(stream_calls, 0L)
    testthat::expect_null(
      store_get_agent_run_by_request_key(
        store,
        config$actor_id,
        request_key
      )
    )
    testthat::expect_null(
      store_get_deferred_reader_question(store, config$actor_id)
    )
    testthat::expect_match(
      appended,
      "didn't send the preserved question",
      fixed = TRUE
    )
  })
})

testthat::test_that("a replacement session cancels a deferred question", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  document <- store$memory$documents[[1L]]
  orientation <- store_start_agent_run(
    store,
    reader_id = config$actor_id,
    kind = "orientation",
    request_key = "orientation-before-deferred-cancel",
    pinned_inputs = list(boundary_hash = "boundary-before-deferred-cancel"),
    worker_id = "orientation-worker"
  )
  orientation <- store_claim_agent_run(
    store,
    reader_id = config$actor_id,
    run_id = orientation$run_id,
    worker_id = "orientation-worker",
    lease_expires_at = Sys.time() + 120
  )
  initial_run_ids <- names(store$memory$agent_runs)
  request_key <- NULL
  reader_starts <- 0L
  testthat::local_mocked_bindings(
    rill_reader_agent = function(...) {
      list(
        stream_async = function(prompt, stream, run_context) {
          reader_starts <<- reader_starts + 1L
          "unexpected response"
        },
        interrupt = \(reason) TRUE
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
      select_entry = list(
        id = document$entry_id,
        position = 1L,
        nonce = 1L
      ),
      reader_chat_submission_id = "deferred-cancel"
    )
    session$flushReact()
    session$setInputs(reader_chat_user_input = "What changed?")
    session$flushReact()

    request_key <<- pending_reader_question()$request_key
    testthat::expect_identical(
      store_get_agent_run(
        store,
        config$actor_id,
        orientation$run_id
      )$status,
      "cancelling"
    )
    testthat::expect_identical(
      store_get_deferred_reader_question(
        store,
        config$actor_id
      )$request_key,
      request_key
    )
    session$close()
  })

  shiny::testServer(rill_server(config, store), {
    session$setInputs(reader_chat_cancel = NULL)
    session$flushReact()
    testthat::expect_identical(
      pending_reader_question()$request_key,
      request_key
    )
    testthat::expect_identical(
      store_get_agent_run(
        store,
        config$actor_id,
        orientation$run_id
      )$status,
      "cancelling"
    )

    session$setInputs(reader_chat_cancel = 1L)
    session$flushReact()
    testthat::expect_null(pending_reader_question())
    testthat::expect_null(
      store_get_deferred_reader_question(store, config$actor_id)
    )

    store_finish_agent_run(
      store,
      reader_id = config$actor_id,
      run_id = orientation$run_id,
      worker_id = "orientation-worker",
      status = "cancelled",
      terminal_reason = "reader_question"
    )
    later::run_now(0.1)
    session$flushReact()

    testthat::expect_identical(reader_starts, 0L)
    testthat::expect_identical(names(store$memory$agent_runs), initial_run_ids)
  })
})

testthat::test_that("question replay does not signal current Orientation", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  document <- store$memory$documents[[1L]]
  interrupted <- NULL

  testthat::local_mocked_bindings(
    rill_reader_agent = function(on_stop, ...) {
      list(stream_async = function(prompt, stream, run_context) {
        list(consume = function() {
          on_stop(
            "complete",
            list(
              usage = list(requests = 1L),
              run_context = run_context,
              run_id = "deputy-replayed-question"
            )
          )
          "The answer."
        })
      })
    },
    append_reader_chat = function(response, session) {
      promises::promise_resolve(response$consume())
    }
  )

  shiny::testServer(rill_server(config, store), {
    run_prioritized_reader_question(
      "What changed?",
      document,
      request_token = "same-submission"
    )
    first <- active_agent_run()
    testthat::expect_identical(first$status, "completed")
    orientation_run <- store_start_agent_run(
      store,
      reader_id = config$actor_id,
      kind = "orientation",
      request_key = "orientation-after-answer",
      pinned_inputs = list(boundary_hash = "current-boundary"),
      worker_id = "other-session"
    )
    orientation_run <- store_claim_agent_run(
      store,
      reader_id = config$actor_id,
      run_id = orientation_run$run_id,
      worker_id = "other-session",
      lease_expires_at = Sys.time() + 120
    )
    orientation_control(list(
      status = "running",
      interrupt = function(reason) {
        interrupted <<- reason
        TRUE
      }
    ))

    run_prioritized_reader_question(
      "What changed?",
      document,
      request_token = "same-submission"
    )

    testthat::expect_null(interrupted)
    testthat::expect_identical(active_agent_run()$run_id, first$run_id)
    testthat::expect_identical(
      store_get_agent_run(
        store,
        config$actor_id,
        orientation_run$run_id
      )$status,
      "running"
    )
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
    rill_reader_agent = function(..., document, on_stop) {
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
    session$setInputs(
      reader_chat_user_input = NULL,
      reader_chat_cancel = NULL
    )
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
      "OpenAI at api.openai.com"
    )
    testthat::expect_identical(
      run$pinned_inputs$data_destination_id,
      rill_agent_data_destination_details("openai")$id
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
    testthat::expect_equal(
      as.numeric(
        difftime(
          running$lease_expires_at,
          running$started_at,
          units = "secs"
        )
      ),
      rill_agent_wall_time_seconds()
    )

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

testthat::test_that("a Reader cancellation remains the first terminal intent", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  entry_id <- store$memory$entries$entry_id[[1L]]
  stop_callback <- NULL
  stream_context <- NULL
  interruptions <- character()
  testthat::local_mocked_bindings(
    rill_agent_wall_time_seconds = \() 1,
    rill_reader_agent = function(on_stop, ...) {
      stop_callback <<- on_stop
      list(
        stream_async = function(prompt, stream, run_context) {
          stream_context <<- run_context
          "pending stream"
        },
        interrupt = function(reason) {
          interruptions <<- c(interruptions, reason)
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
      select_entry = list(id = entry_id, position = 1L, nonce = 1L)
    )
    session$flushReact()
    session$setInputs(reader_chat_user_input = "What changed?")
    session$flushReact()
    running <- active_agent_run()

    session$setInputs(reader_chat_cancel = 1L)
    session$flushReact()
    testthat::expect_identical(interruptions, "reader_cancelled")
    deadline <- Sys.time() + 1.2
    while (Sys.time() < deadline) {
      later::run_now(0.05)
      session$flushReact()
    }
    testthat::expect_identical(interruptions, "reader_cancelled")

    stop_callback(
      "cancelled",
      list(
        usage = list(requests = 1L),
        run_context = stream_context,
        run_id = "deputy-reader-cancelled-first"
      )
    )
    cancelled <- store_get_agent_run(
      store,
      config$actor_id,
      running$run_id
    )
    testthat::expect_identical(cancelled$status, "cancelled")
    testthat::expect_identical(
      cancelled$terminal_reason,
      "reader_cancelled"
    )
  })
})

testthat::test_that("cancellation wins a raced completion", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  entry_id <- store$memory$entries$entry_id[[1L]]
  stop_callback <- NULL
  stream_context <- NULL
  testthat::local_mocked_bindings(
    rill_reader_agent = function(on_stop, ...) {
      stop_callback <<- on_stop
      list(
        stream_async = function(prompt, stream, run_context) {
          stream_context <<- run_context
          "pending stream"
        },
        interrupt = \(reason) TRUE
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
      select_entry = list(id = entry_id, position = 1L, nonce = 1L)
    )
    session$flushReact()
    session$setInputs(reader_chat_user_input = "What changed?")
    session$flushReact()
    running <- active_agent_run()

    session$setInputs(reader_chat_cancel = 1L)
    session$flushReact()
    stop_callback(
      "complete",
      list(
        usage = list(requests = 1L),
        run_context = stream_context,
        run_id = "deputy-completed-during-cancel"
      )
    )

    cancelled <- store_get_agent_run(
      store,
      config$actor_id,
      running$run_id
    )
    testthat::expect_identical(cancelled$status, "cancelled")
    testthat::expect_identical(
      cancelled$terminal_reason,
      "reader_cancelled"
    )
  })
})

testthat::test_that("a raced provider failure remains failed after cancellation", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  entry_id <- store$memory$entries$entry_id[[1L]]
  stop_callback <- NULL
  stream_context <- NULL
  testthat::local_mocked_bindings(
    rill_reader_agent = function(on_stop, ...) {
      stop_callback <<- on_stop
      list(
        stream_async = function(prompt, stream, run_context) {
          stream_context <<- run_context
          "pending stream"
        },
        interrupt = \(reason) TRUE
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
      select_entry = list(id = entry_id, position = 1L, nonce = 1L)
    )
    session$flushReact()
    session$setInputs(reader_chat_user_input = "What changed?")
    session$flushReact()
    running <- active_agent_run()

    session$setInputs(reader_chat_cancel = 1L)
    session$flushReact()
    stop_callback(
      "provider_error",
      list(
        usage = list(requests = 1L),
        run_context = stream_context,
        run_id = "deputy-failed-during-cancel"
      )
    )

    failed <- store_get_agent_run(store, config$actor_id, running$run_id)
    testthat::expect_identical(failed$status, "failed")
    testthat::expect_identical(failed$terminal_reason, "provider_error")
  })
})

testthat::test_that("a failed cancellation retries the first Reader intent", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  entry_id <- store$memory$entries$entry_id[[1L]]
  attempts <- character()
  testthat::local_mocked_bindings(
    agent_run_interrupt_retry_delay = \(attempt) 0,
    agent_run_interrupt_confirmation_seconds = \() 0.05,
    rill_reader_agent = function(...) {
      list(
        stream_async = \(prompt, stream, run_context) "pending stream",
        interrupt = function(reason) {
          attempts <<- c(attempts, reason)
          if (length(attempts) == 1L) {
            cli::cli_abort(
              "The controller temporarily failed.",
              class = "test_controller_cancel_failed"
            )
          }
          FALSE
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
      select_entry = list(id = entry_id, position = 1L, nonce = 1L)
    )
    session$flushReact()
    session$setInputs(reader_chat_user_input = "What changed?")
    session$flushReact()
    run_id <- active_agent_run()$run_id

    session$setInputs(reader_chat_cancel = 1L)
    session$flushReact()
    deadline <- Sys.time() + 2
    while (
      !active_agent_run()$status %in% c("cancelled", "interrupted") &&
        Sys.time() < deadline
    ) {
      later::run_now(0.01)
      session$flushReact()
    }

    cancelled <- store_get_agent_run(store, config$actor_id, run_id)
    testthat::expect_identical(
      attempts,
      c("reader_cancelled", "reader_cancelled")
    )
    testthat::expect_identical(cancelled$status, "cancelled")
    testthat::expect_identical(cancelled$terminal_reason, "reader_cancelled")
  })
})

testthat::test_that("unconfirmed Reader cancellation is bounded", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  entry_id <- store$memory$entries$entry_id[[1L]]
  attempts <- character()
  testthat::local_mocked_bindings(
    agent_run_interrupt_retry_limit = \() 2L,
    agent_run_interrupt_retry_delay = \(attempt) 0,
    rill_reader_agent = function(...) {
      list(
        stream_async = \(prompt, stream, run_context) "pending stream",
        interrupt = function(reason) {
          attempts <<- c(attempts, reason)
          cli::cli_abort(
            "The controller cannot confirm cancellation.",
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
    session$setInputs(
      reader_chat_user_input = NULL,
      reader_chat_cancel = NULL
    )
    session$flushReact()
    session$setInputs(
      select_entry = list(id = entry_id, position = 1L, nonce = 1L)
    )
    session$flushReact()
    session$setInputs(reader_chat_user_input = "What changed?")
    session$flushReact()
    run_id <- active_agent_run()$run_id

    session$setInputs(reader_chat_cancel = 1L)
    session$flushReact()
    deadline <- Sys.time() + 2
    while (
      !identical(active_agent_run()$status, "interrupted") &&
        Sys.time() < deadline
    ) {
      later::run_now(0.01)
      session$flushReact()
    }

    interrupted <- store_get_agent_run(store, config$actor_id, run_id)
    testthat::expect_identical(
      attempts,
      c("reader_cancelled", "reader_cancelled")
    )
    testthat::expect_identical(interrupted$status, "interrupted")
    testthat::expect_identical(
      interrupted$terminal_reason,
      "interrupt_unconfirmed:reader_cancelled"
    )
  })
})

testthat::test_that("accepted cancellation still requires bounded confirmation", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  entry_id <- store$memory$entries$entry_id[[1L]]
  testthat::local_mocked_bindings(
    agent_run_interrupt_confirmation_seconds = \() 0,
    rill_reader_agent = function(...) {
      list(
        stream_async = \(prompt, stream, run_context) "pending stream",
        interrupt = \(reason) TRUE
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
      select_entry = list(id = entry_id, position = 1L, nonce = 1L)
    )
    session$flushReact()
    session$setInputs(reader_chat_user_input = "What changed?")
    session$flushReact()

    session$setInputs(reader_chat_cancel = 1L)
    session$flushReact()
    later::run_now(0.01)
    session$flushReact()

    testthat::expect_identical(active_agent_run()$status, "interrupted")
    testthat::expect_identical(
      active_agent_run()$terminal_reason,
      "interrupt_unconfirmed:reader_cancelled"
    )
  })
})

testthat::test_that("a replacement session follows a running question", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  entry_id <- store$memory$entries$entry_id[[1L]]
  stop_callback <- NULL
  stream_context <- NULL
  interrupted <- NULL
  run_id <- NULL
  testthat::local_mocked_bindings(
    rill_agent_wall_time_seconds = \() 1.5,
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
      select_entry = list(id = entry_id, position = 1L, nonce = 1L)
    )
    session$flushReact()
    session$setInputs(reader_chat_user_input = "What changed?")
    session$flushReact()
    run_id <<- active_agent_run()$run_id
    session$close()
  })

  testthat::expect_identical(
    store_get_agent_run(store, config$actor_id, run_id)$status,
    "running"
  )
  shiny::testServer(rill_server(config, store), {
    session$flushReact()
    testthat::expect_identical(active_agent_run()$run_id, run_id)
    testthat::expect_identical(active_agent_run()$status, "running")

    deadline <- Sys.time() + 3
    while (
      !identical(active_agent_run()$status, "cancelling") &&
        Sys.time() < deadline
    ) {
      later::run_now(0.01)
      session$flushReact()
    }
    testthat::expect_identical(interrupted, "wall_time_limit")
    testthat::expect_identical(active_agent_run()$status, "cancelling")

    stop_callback(
      "wall_time_limit",
      list(
        usage = list(requests = 1L),
        run_context = stream_context,
        run_id = "deputy-session-replacement"
      )
    )
    deadline <- Sys.time() + 2
    while (
      !identical(active_agent_run()$status, "failed") &&
        Sys.time() < deadline
    ) {
      later::run_now(0.01)
      session$flushReact()
    }
    testthat::expect_identical(active_agent_run()$status, "failed")
    testthat::expect_identical(
      active_agent_run()$terminal_reason,
      "wall_time_limit"
    )
  })
})

testthat::test_that("an owning worker observes durable cancellation", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  entry_id <- store$memory$entries$entry_id[[1L]]
  stop_callback <- NULL
  stream_context <- NULL
  interruptions <- character()
  testthat::local_mocked_bindings(
    rill_agent_wall_time_seconds = \() 2,
    rill_reader_agent = function(on_stop, ...) {
      stop_callback <<- on_stop
      list(
        stream_async = function(prompt, stream, run_context) {
          stream_context <<- run_context
          "pending stream"
        },
        interrupt = function(reason) {
          interruptions <<- c(interruptions, reason)
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
      select_entry = list(id = entry_id, position = 1L, nonce = 1L)
    )
    session$flushReact()
    session$setInputs(reader_chat_user_input = "What changed?")
    session$flushReact()
    run_id <- active_agent_run()$run_id

    store_request_agent_run_cancel(
      store,
      reader_id = config$actor_id,
      run_id = run_id
    )
    testthat::expect_length(interruptions, 0L)

    deadline <- Sys.time() + 2
    while (length(interruptions) == 0L && Sys.time() < deadline) {
      later::run_now(0.05)
      session$flushReact()
    }
    testthat::expect_identical(interruptions, "reader_cancelled")

    stop_callback(
      "complete",
      list(
        usage = list(requests = 1L),
        run_context = stream_context,
        run_id = "deputy-completed-after-cancel"
      )
    )
    cancelled <- store_get_agent_run(store, config$actor_id, run_id)
    testthat::expect_identical(cancelled$status, "cancelled")
    testthat::expect_identical(
      cancelled$terminal_reason,
      "reader_cancelled"
    )
  })
})

testthat::test_that("a replacement session cancels a running question", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  entry_id <- store$memory$entries$entry_id[[1L]]
  stop_callback <- NULL
  stream_context <- NULL
  interruptions <- character()
  run_id <- NULL
  terminalizations <- 0L
  finish_agent_run <- store_finish_agent_run
  testthat::local_mocked_bindings(
    rill_agent_wall_time_seconds = \() 2,
    store_finish_agent_run = function(...) {
      finished <- finish_agent_run(...)
      if (!is.null(finished)) {
        terminalizations <<- terminalizations + 1L
      }
      finished
    },
    rill_reader_agent = function(on_stop, ...) {
      stop_callback <<- on_stop
      list(
        stream_async = function(prompt, stream, run_context) {
          stream_context <<- run_context
          "pending stream"
        },
        interrupt = function(reason) {
          interruptions <<- c(interruptions, reason)
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
      select_entry = list(id = entry_id, position = 1L, nonce = 1L)
    )
    session$flushReact()
    session$setInputs(reader_chat_user_input = "What changed?")
    session$flushReact()
    run_id <<- active_agent_run()$run_id
    session$close()
  })

  testthat::expect_identical(
    store_get_agent_run(store, config$actor_id, run_id)$status,
    "running"
  )
  shiny::testServer(rill_server(config, store), {
    session$setInputs(reader_chat_cancel = NULL)
    session$flushReact()
    testthat::expect_identical(active_agent_run()$run_id, run_id)

    session$setInputs(reader_chat_cancel = 1L)
    session$flushReact()
    testthat::expect_identical(interruptions, "reader_cancelled")
    testthat::expect_identical(active_agent_run()$status, "cancelling")

    later::run_now(2.1)
    session$flushReact()
    testthat::expect_identical(interruptions, "reader_cancelled")

    stop_callback(
      "cancelled",
      list(
        usage = list(requests = 1L),
        run_context = stream_context,
        run_id = "deputy-cancelled-after-reconnect"
      )
    )
    cancelled <- store_get_agent_run(store, config$actor_id, run_id)
    terminal_at <- cancelled$terminal_at
    testthat::expect_identical(cancelled$status, "cancelled")
    testthat::expect_identical(
      cancelled$terminal_reason,
      "reader_cancelled"
    )
    testthat::expect_identical(terminalizations, 1L)

    stop_callback(
      "wall_time_limit",
      list(
        usage = list(requests = 1L),
        run_context = stream_context,
        run_id = "deputy-cancelled-after-reconnect"
      )
    )
    unchanged <- store_get_agent_run(store, config$actor_id, run_id)
    testthat::expect_identical(unchanged$terminal_reason, "reader_cancelled")
    testthat::expect_identical(unchanged$terminal_at, terminal_at)
    testthat::expect_identical(terminalizations, 1L)
  })
})

testthat::test_that("a restarted question remains visible and retryable", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  document <- store$memory$documents[[1L]]
  requested_at <- Sys.time() - 2
  interrupted <- store_start_agent_run(
    store,
    reader_id = config$actor_id,
    kind = "question",
    request_key = "question-before-process-restart",
    pinned_inputs = list(
      submission_id = "question-before-process-restart",
      entry_id = document$entry_id,
      document_id = document$document_id,
      document_content_hash = document$content_hash,
      document_record_hash = document$record_hash,
      research_scope = list(
        kind = "selected_document",
        document_ids = document$document_id
      ),
      data_destination = "OpenAI at api.openai.com",
      data_destination_id = rill_agent_data_destination_details("openai")$id,
      question = "What changed?",
      model = "openai",
      policy_version = "ask-rill-v1",
      limits = rill_agent_run_limits()
    ),
    requested_at = requested_at,
    worker_id = "old-process"
  )
  interrupted <- store_claim_agent_run(
    store,
    reader_id = config$actor_id,
    run_id = interrupted$run_id,
    worker_id = "old-process",
    started_at = requested_at,
    lease_expires_at = requested_at + 300
  )
  store_interrupt_agent_runs(
    store,
    recovery = "process_restart",
    recovered_at = requested_at + 1
  )
  testthat::local_mocked_bindings(
    rill_reader_agent = function(on_stop, ...) {
      list(stream_async = function(prompt, stream, run_context) {
        on_stop(
          "complete",
          list(
            usage = list(requests = 1L),
            run_context = run_context,
            run_id = "deputy-retry-after-restart"
          )
        )
        "Retried answer."
      })
    },
    append_reader_chat = function(response, session) {
      promises::promise_resolve(response)
    }
  )

  shiny::testServer(rill_server(config, store), {
    session$flushReact()

    testthat::expect_identical(active_agent_run()$run_id, interrupted$run_id)
    testthat::expect_identical(active_agent_run()$status, "interrupted")
    testthat::expect_identical(selected_id(), document$entry_id)
    testthat::expect_identical(selected_document_id(), document$document_id)
    testthat::expect_match(
      as.character(output$reader_agent_status)[[1L]],
      "Retry",
      fixed = TRUE
    )

    session$setInputs(retry_agent_run = 1L)
    session$flushReact()

    retried <- active_agent_run()
    testthat::expect_identical(retried$status, "completed")
    testthat::expect_identical(retried$retry_of_run_id, interrupted$run_id)
  })
})

testthat::test_that("a claim error terminalizes the pending Agent Run", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  entry_id <- store$memory$entries$entry_id[[1]]
  claim_agent_run <- store_claim_agent_run

  testthat::local_mocked_bindings(
    rill_reader_agent = \(...) list(),
    append_reader_chat = function(response, session) {
      promises::promise_resolve(response)
    },
    store_claim_agent_run = function(...) {
      claim_agent_run(...)
      cli::cli_abort(
        "The database connection was interrupted.",
        class = "test_database_error"
      )
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

    failed <- active_agent_run()
    testthat::expect_identical(failed$status, "failed")
    testthat::expect_identical(
      failed$terminal_reason,
      "claim_error:test_database_error"
    )
    next_run <- store_start_agent_run(
      store,
      reader_id = config$actor_id,
      kind = "question",
      request_key = "next-question",
      pinned_inputs = list(document_id = "next-document")
    )
    testthat::expect_identical(next_run$status, "pending")
  })
})

testthat::test_that("a start error terminalizes its owned pending Agent Run", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  entry_id <- store$memory$entries$entry_id[[1]]
  start_agent_run <- store_start_agent_run

  testthat::local_mocked_bindings(
    rill_reader_agent = \(...) list(),
    append_reader_chat = function(response, session) {
      promises::promise_resolve(response)
    },
    store_start_agent_run = function(...) {
      start_agent_run(...)
      cli::cli_abort(
        "The database response was lost after the write.",
        class = "test_database_error"
      )
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

    failed <- active_agent_run()
    testthat::expect_identical(failed$status, "failed")
    testthat::expect_identical(
      failed$terminal_reason,
      "start_error:test_database_error"
    )
    next_run <- start_agent_run(
      store,
      reader_id = config$actor_id,
      kind = "question",
      request_key = "next-question",
      pinned_inputs = list(document_id = "next-document")
    )
    testthat::expect_identical(next_run$status, "pending")
  })
})

testthat::test_that("a duplicate start error preserves the active Agent Run", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  entry_id <- store$memory$entries$entry_id[[1]]
  start_agent_run <- store_start_agent_run
  state <- new.env(parent = emptyenv())
  state$fail_start <- FALSE

  testthat::local_mocked_bindings(
    rill_reader_agent = function(..., document, on_stop) {
      list(
        stream_async = \(prompt, stream, run_context) "pending stream",
        interrupt = \(reason) TRUE
      )
    },
    append_reader_chat = function(response, session) {
      promises::promise_resolve(response)
    },
    store_start_agent_run = function(...) {
      if (state$fail_start) {
        cli::cli_abort(
          "The database read was interrupted.",
          class = "test_database_error"
        )
      }
      start_agent_run(...)
    }
  )

  shiny::testServer(rill_server(config, store), {
    session$setInputs(
      reader_chat_user_input = NULL,
      reader_chat_submission_id = "submission-1"
    )
    session$flushReact()
    session$setInputs(
      select_entry = list(id = entry_id, position = 1L, nonce = 1)
    )
    session$flushReact()
    session$setInputs(reader_chat_user_input = "Summarize this story.")
    session$flushReact()
    running <- active_agent_run()
    state$fail_start <- TRUE

    session$setInputs(reader_chat_user_input = NULL)
    session$flushReact()
    session$setInputs(reader_chat_user_input = "Summarize this story.")
    session$flushReact()

    testthat::expect_identical(active_agent_run(), running)
    testthat::expect_identical(
      store_get_agent_run(store, config$actor_id, running$run_id),
      running
    )
    deadline <- agent_run_deadlines[[running$run_id]]
    if (is.function(deadline)) {
      deadline()
    }
  })
})

testthat::test_that("an inactive Agent confirms cancellation", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  entry_id <- store$memory$entries$entry_id[[1]]

  testthat::local_mocked_bindings(
    rill_reader_agent = function(..., document, on_stop) {
      list(
        stream_async = \(prompt, stream, run_context) "pending stream",
        interrupt = \(reason) FALSE
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

    session$setInputs(reader_chat_cancel = 1L)
    session$flushReact()

    cancelled <- active_agent_run()
    testthat::expect_identical(cancelled$run_id, running$run_id)
    testthat::expect_identical(cancelled$status, "cancelled")
    testthat::expect_identical(cancelled$terminal_reason, "reader_cancelled")
    testthat::expect_null(agent_run_deadlines[[cancelled$run_id]])
  })
})

testthat::test_that("a raced cancellation does not interrupt the Agent", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  entry_id <- store$memory$entries$entry_id[[1]]
  interrupted <- character()

  testthat::local_mocked_bindings(
    rill_reader_agent = function(..., document, on_stop) {
      list(
        stream_async = \(prompt, stream, run_context) "pending stream",
        interrupt = function(reason) {
          interrupted <<- c(interrupted, reason)
          TRUE
        }
      )
    },
    append_reader_chat = function(response, session) {
      promises::promise_resolve(response)
    },
    store_request_agent_run_cancel = \(...) NULL
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
    session$setInputs(reader_chat_cancel = 1L)
    session$flushReact()

    testthat::expect_length(interrupted, 0L)
    testthat::expect_identical(active_agent_run()$status, "running")
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
    later::run_now(0)
    session$flushReact()

    run <- active_agent_run()
    testthat::expect_identical(interrupted, "wall_time_limit")
    testthat::expect_identical(run$status, "cancelling")
    testthat::expect_null(run$terminal_reason)
    testthat::expect_null(run$terminal_at)
    testthat::expect_null(run$deputy_run_id)

    session$setInputs(reader_chat_cancel = 1L)
    session$flushReact()
    testthat::expect_identical(interrupted, "wall_time_limit")

    session$setInputs(retry_agent_run = 1L)
    session$flushReact()
    testthat::expect_length(
      Filter(
        \(run) !identical(run$terminal_reason, "bundled_demo"),
        store$memory$agent_runs
      ),
      1L
    )

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
    testthat::expect_type(settled$terminal_at, "character")
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

testthat::test_that("a deadline setup read error is retried", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  entry_id <- store$memory$entries$entry_id[[1]]
  get_agent_run <- store_get_agent_run
  interrupted <- character()
  state <- new.env(parent = emptyenv())
  state$fail_read <- FALSE
  state$read_failures <- 0L

  testthat::local_mocked_bindings(
    rill_agent_wall_time_seconds = \() 0,
    rill_reader_agent = function(...) {
      list(
        stream_async = function(prompt, stream, run_context) {
          state$fail_read <- TRUE
          "pending stream"
        },
        interrupt = function(reason) {
          interrupted <<- c(interrupted, reason)
          TRUE
        }
      )
    },
    append_reader_chat = function(response, session) {
      promises::promise_resolve(response)
    },
    store_get_agent_run = function(...) {
      if (state$fail_read) {
        state$fail_read <- FALSE
        state$read_failures <- state$read_failures + 1L
        cli::cli_abort(
          "The database read was interrupted.",
          class = "test_database_error"
        )
      }
      get_agent_run(...)
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

    testthat::expect_identical(state$read_failures, 1L)
    testthat::expect_length(interrupted, 0L)

    deadline <- Sys.time() + 2
    while (length(interrupted) == 0L && Sys.time() < deadline) {
      later::run_now(0.25)
      session$flushReact()
    }

    testthat::expect_identical(interrupted, "wall_time_limit")
    testthat::expect_identical(active_agent_run()$status, "cancelling")
  })
})

testthat::test_that("a deadline read error still interrupts and settles", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  entry_id <- store$memory$entries$entry_id[[1]]
  get_agent_run <- store_get_agent_run
  request_cancel <- store_request_agent_run_cancel
  interrupted <- character()
  stop_callback <- NULL
  stream_context <- NULL
  state <- new.env(parent = emptyenv())
  state$fail_read <- FALSE
  state$fail_cancel <- FALSE

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
          interrupted <<- c(interrupted, reason)
          TRUE
        }
      )
    },
    append_reader_chat = function(response, session) {
      state$fail_cancel <- TRUE
      promises::promise_resolve(response)
    },
    store_request_agent_run_cancel = function(...) {
      if (state$fail_cancel) {
        state$fail_cancel <- FALSE
        cli::cli_abort(
          "The database read was interrupted.",
          class = "test_database_error"
        )
      }
      request_cancel(...)
    },
    store_get_agent_run = function(...) {
      if (state$fail_read) {
        state$fail_read <- FALSE
        cli::cli_abort(
          "The database read was interrupted.",
          class = "test_database_error"
        )
      }
      get_agent_run(...)
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
    running <- active_agent_run()

    later::run_now(0)
    session$flushReact()

    testthat::expect_identical(interrupted, "wall_time_limit")
    testthat::expect_identical(active_agent_run()$status, "running")
    testthat::expect_type(
      rill_question_drain_registry[[running$run_id]]$on_result,
      "closure"
    )

    state$fail_read <- TRUE
    testthat::expect_error(
      stop_callback(
        "wall_time_limit",
        list(
          usage = list(requests = 1L),
          run_context = stream_context,
          run_id = "deputy-run-time-limited"
        )
      ),
      class = "test_database_error"
    )
    testthat::expect_null(draining_agent_run_id())

    for (iteration in seq_len(10)) {
      later::run_now(0.25)
      session$flushReact()
      if (identical(active_agent_run()$status, "failed")) {
        break
      }
    }

    settled <- active_agent_run()
    testthat::expect_identical(settled$status, "failed")
    testthat::expect_identical(settled$terminal_reason, "wall_time_limit")
    testthat::expect_null(draining_agent_run_id())
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
    testthat::expect_identical(timed_out$status, "cancelling")
    testthat::expect_null(timed_out$terminal_reason)
    testthat::expect_identical(
      draining_agent_run_id(),
      timed_out$run_id
    )

    session$setInputs(retry_agent_run = 1L)
    session$flushReact()
    testthat::expect_length(
      Filter(
        \(run) !identical(run$terminal_reason, "bundled_demo"),
        store$memory$agent_runs
      ),
      1L
    )
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
    testthat::expect_length(
      Filter(
        \(run) !identical(run$terminal_reason, "bundled_demo"),
        store$memory$agent_runs
      ),
      0L
    )
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

    orientation_run <- store_start_agent_run(
      store,
      reader_id = config$actor_id,
      kind = "orientation",
      request_key = "orientation-before-retry",
      pinned_inputs = list(boundary_hash = "retry-boundary"),
      worker_id = "other-session"
    )
    session$setInputs(retry_agent_run = 1L)
    session$flushReact()

    retried <- active_agent_run()
    testthat::expect_identical(retried$status, "completed")
    testthat::expect_identical(retried$retry_of_run_id, failed$run_id)
    testthat::expect_identical(
      store_get_agent_run(
        store,
        config$actor_id,
        orientation_run$run_id
      )$status,
      "cancelled"
    )
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

testthat::test_that("organizing a selected feed updates its Subscription", {
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

    session$setInputs(feed_folder = "Research", move_feed = 1L)
    session$flushReact()
    moved <- store_list_feeds(store, config$actor_id)
    moved <- moved[moved$feed_id == feed_id, , drop = FALSE]
    testthat::expect_identical(moved$folder, "Research")
    testthat::expect_identical(status_text(), "Moved feed to Research")
    testthat::expect_identical(
      tail(store$memory$events$event_type, 1L),
      "feed_moved"
    )

    session$setInputs(unsubscribe_feed = 1L)
    session$flushReact()
    testthat::expect_disjoint(
      store_list_feeds(store, config$actor_id)$feed_id,
      feed_id
    )
    testthat::expect_null(selected_feed())
    testthat::expect_identical(
      tail(store$memory$events$event_type, 1L),
      "feed_unsubscribed"
    )
  })
})

testthat::test_that("feed management keeps browsing separate and restores subscriptions", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  entry_id <- store$memory$entries$entry_id[[1L]]
  reading_feed <- store$memory$entries$feed_id[[1L]]
  managed_id <- setdiff(
    store_list_feeds(store, config$actor_id)$feed_id,
    reading_feed
  )[[1L]]
  refresh_calls <- character()
  testthat::local_mocked_bindings(refresh_feed = function(store, feed) {
    refresh_calls <<- c(refresh_calls, feed$feed_id)
    list(added = 0L, not_modified = TRUE)
  })

  shiny::testServer(rill_server(config, store), {
    session$setInputs(
      select_entry = NULL,
      managed_feed = "",
      restore_feed = 0L,
      unsubscribe_feed = 0L,
      refresh_selected_feed = 0L,
      rename_feed = 0L
    )
    session$flushReact()
    session$setInputs(
      select_entry = list(id = entry_id, position = 1L, nonce = 1)
    )
    session$setInputs(managed_feed = managed_id)
    session$setInputs(feed_title = "Renamed in manager", rename_feed = 1L)
    testthat::expect_identical(selected_id(), entry_id)
    testthat::expect_null(selected_feed())
    rows <- store_list_feeds(store, config$actor_id)
    testthat::expect_identical(
      rows$title[rows$feed_id == managed_id],
      "Renamed in manager"
    )

    session$setInputs(refresh_selected_feed = 1L)
    testthat::expect_identical(refresh_calls, managed_id)
    testthat::expect_identical(selected_id(), entry_id)
    testthat::expect_identical(
      status_text(),
      "1 feed checked \u00b7 0 new stories."
    )

    session$setInputs(unsubscribe_feed = 1L)
    testthat::expect_identical(selected_id(), entry_id)
    testthat::expect_disjoint(
      store_list_feeds(store, config$actor_id)$feed_id,
      managed_id
    )
    session$setInputs(restore_feed = 1L)
    rows <- store_list_feeds(store, config$actor_id)
    testthat::expect_in(managed_id, rows$feed_id)
    testthat::expect_identical(
      rows$title[rows$feed_id == managed_id],
      "Renamed in manager"
    )

    session$setInputs(managed_feed = "unowned", restore_feed = 2L)
    testthat::expect_disjoint(
      store_list_feeds(store, config$actor_id)$feed_id,
      "unowned"
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
  prepared_reader_id <- NULL
  testthat::local_mocked_bindings(
    prepare_today_documents = function(store, config, reader_id, progress) {
      prepared_reader_id <<- reader_id
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
    testthat::expect_identical(prepared_reader_id, config$actor_id)
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
