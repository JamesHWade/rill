testthat::test_that("Orientation maintenance is deterministic and source-pinned", {
  store <- rill_store(list(demo_mode = TRUE))
  reader_id <- "reader-1"
  worker_id <- "orientation-worker-1"
  previous_candidates <- orientation_candidates(store, reader_id, limit = 2L)
  previous_boundary <- orientation_boundary(previous_candidates)
  document <- previous_candidates[[1L]]$document
  previous <- new_rill_orientation(
    reader_id = reader_id,
    boundary = previous_boundary,
    question = "What must stay separate?",
    introduction = "Begin with the source boundary.",
    cards = list(list(
      role = "anchor",
      frame = "unresolved_question",
      document_id = document$document_id,
      entry_id = document$entry_id,
      interpretation = "Keep this exact useful interpretation.",
      why_now = "It remains the clearest unread account.",
      evidence = "Rill keeps the source feed"
    )),
    agent_run_id = "seed-orientation-run"
  )
  register_orientation_test_run(store, reader_id, "seed-orientation-run")
  store_save_orientation(store, previous)

  call <- new.env(parent = emptyenv())
  agent_factory <- function(
    candidates,
    reader_id,
    session_id,
    boundary_hash,
    model
  ) {
    agent <- new.env(parent = emptyenv())
    agent$get_model <- \() "gpt-test"
    agent$get_provider <- \() stop("No provider object in this test.")
    output <- list(
      status = "One source boundary deserves attention.",
      question = "What must stay separate?",
      introduction = "Begin with the source boundary.",
      cards = list(list(
        document_id = candidates[[1L]]$document$document_id,
        role = "anchor",
        frame = "unresolved_question",
        interpretation = "Keep this exact useful interpretation.",
        why_now = "It remains the clearest unread account.",
        evidence = "Rill keeps the source feed"
      ))
    )
    orientation_test_tool_state(agent, output)
    agent$run_async <- function(prompt, run_context) {
      call$prompt <- prompt
      call$run_context <- run_context
      promises::promise_resolve(orientation_test_agent_result(
        run_id = "deputy-orientation-1"
      ))
    }
    agent$interrupt <- \(reason) TRUE
    agent
  }

  control <- maintain_orientation_async(
    store = store,
    reader_id = reader_id,
    worker_id = worker_id,
    model = "openai/gpt-test",
    candidate_limit = 3L,
    destination_check = orientation_test_destination_check(store, reader_id),
    agent_factory = agent_factory,
    schedule_timeout = FALSE
  )
  resolved <- NULL
  rejected <- NULL
  promises::then(
    control$promise,
    \(value) resolved <<- value,
    onRejected = \(error) rejected <<- error
  )
  deadline <- Sys.time() + 2
  while (is.null(resolved) && is.null(rejected) && Sys.time() < deadline) {
    later::run_now(0.01)
  }

  run <- store_get_agent_run(store, reader_id, control$run$run_id)
  candidates <- orientation_candidates(store, reader_id, limit = 3L)
  boundary <- orientation_boundary(candidates)
  expected_key <- orientation_request_key(run$pinned_inputs)

  testthat::expect_null(rejected)
  testthat::expect_identical(resolved$run$status, "completed")
  testthat::expect_identical(run$request_key, expected_key)
  testthat::expect_identical(run$pinned_inputs$boundary_hash, boundary$hash)
  testthat::expect_identical(
    run$pinned_inputs$candidates,
    orientation_candidate_pins(boundary)
  )
  testthat::expect_identical(
    unname(vapply(
      run$pinned_inputs$candidates,
      `[[`,
      character(1),
      "entry_id"
    )),
    vapply(boundary$candidates, `[[`, character(1), "entry_id")
  )
  testthat::expect_identical(
    run$pinned_inputs$limits,
    rill_orientation_run_limits()
  )
  roundtrip <- jsonlite::fromJSON(
    agent_run_json(run$pinned_inputs),
    simplifyVector = TRUE
  )
  testthat::expect_identical(
    as.character(canonical_json(roundtrip)),
    as.character(canonical_json(run$pinned_inputs))
  )
  testthat::expect_identical(run$pinned_inputs$model, "gpt-test")
  testthat::expect_identical(
    call$run_context$rill_agent_run_id,
    run$run_id
  )
  testthat::expect_match(
    call$prompt,
    "Keep this exact useful interpretation.",
    fixed = TRUE
  )
  testthat::expect_match(call$prompt, "Keep unchanged wording", fixed = TRUE)
  testthat::expect_identical(run$deputy_run_id, "deputy-orientation-1")
  testthat::expect_identical(run$usage$requests, 1L)
})

testthat::test_that("Orientation rechecks consent at the provider boundary", {
  store <- rill_store(list(demo_mode = TRUE))
  reader_id <- "reader-consent-boundary"
  check_calls <- 0L
  provider_calls <- 0L
  destination_check <- function() {
    check_calls <<- check_calls + 1L
    list(
      enabled = identical(check_calls, 1L),
      destination = list(id = "consent-boundary-v1")
    )
  }
  agent_factory <- function(...) {
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

  control <- maintain_orientation_async(
    store = store,
    reader_id = reader_id,
    worker_id = "orientation-consent-worker",
    model = "openai/gpt-test",
    candidate_limit = 3L,
    destination_check = destination_check,
    agent_factory = agent_factory,
    schedule_timeout = FALSE
  )
  rejected <- NULL
  promises::then(
    control$promise,
    onRejected = \(error) rejected <<- error
  )
  deadline <- Sys.time() + 2
  while (is.null(rejected) && Sys.time() < deadline) {
    later::run_now(0.01)
  }

  testthat::expect_s3_class(
    rejected,
    "rill_orientation_destination_disabled"
  )
  testthat::expect_identical(check_calls, 2L)
  testthat::expect_identical(provider_calls, 0L)
})

testthat::test_that("a current Orientation does not launch another Agent Run", {
  store <- rill_store(list(demo_mode = TRUE))
  reader_id <- "reader-1"
  candidates <- orientation_candidates(store, reader_id, limit = 3L)
  boundary <- orientation_boundary(candidates)
  orientation <- new_rill_orientation(
    reader_id = reader_id,
    boundary = boundary,
    question = NULL,
    introduction = NULL,
    cards = list(),
    agent_run_id = "seed-orientation-run"
  )
  register_orientation_test_run(store, reader_id, "seed-orientation-run")
  store_save_orientation(store, orientation)
  launched <- FALSE

  control <- maintain_orientation_async(
    store = store,
    reader_id = reader_id,
    worker_id = "orientation-worker-1",
    model = "openai/gpt-test",
    candidate_limit = 3L,
    destination_check = orientation_test_destination_check(store, reader_id),
    agent_factory = function(...) {
      launched <<- TRUE
      stop("This factory must not run.")
    },
    schedule_timeout = FALSE
  )

  testthat::expect_identical(control$status, "current")
  testthat::expect_identical(control$orientation, orientation)
  testthat::expect_null(control$run)
  testthat::expect_null(control$promise)
  testthat::expect_identical(launched, FALSE)
})

testthat::test_that("Orientation rejects output that skipped source inspection", {
  store <- rill_store(list(demo_mode = TRUE))
  reader_id <- "reader-1"
  agent_factory <- function(...) {
    agent <- new.env(parent = emptyenv())
    agent$get_model <- \() "gpt-test"
    agent$get_provider <- \() stop("No provider object in this test.")
    orientation_test_tool_state(
      agent,
      list(status = "Nothing was inspected.", cards = list()),
      source_calls = 0L
    )
    agent$run_async <- function(...) {
      promises::promise_resolve(orientation_test_agent_result(
        tool_calls = 1L,
        run_id = "deputy-orientation-no-tool"
      ))
    }
    agent$interrupt <- \(reason) TRUE
    agent
  }

  control <- maintain_orientation_async(
    store = store,
    reader_id = reader_id,
    worker_id = "orientation-worker-1",
    model = "openai/gpt-test",
    candidate_limit = 3L,
    destination_check = orientation_test_destination_check(store, reader_id),
    agent_factory = agent_factory,
    schedule_timeout = FALSE
  )
  failure <- NULL
  promises::then(
    control$promise,
    onRejected = \(error) failure <<- error
  )
  deadline <- Sys.time() + 2
  while (is.null(failure) && Sys.time() < deadline) {
    later::run_now(0.01)
  }
  run <- store_get_agent_run(store, reader_id, control$run$run_id)

  testthat::expect_s3_class(failure, "rill_orientation_agent_stopped")
  testthat::expect_identical(run$status, "failed")
  testthat::expect_identical(run$terminal_reason, "source_not_inspected")
  testthat::expect_null(store_get_orientation(store, reader_id))
})

testthat::test_that("Orientation rejects a completed run without submission", {
  store <- rill_store(list(demo_mode = TRUE))
  reader_id <- "reader-1"
  agent_factory <- function(...) {
    agent <- new.env(parent = emptyenv())
    agent$get_model <- \() "gpt-test"
    agent$get_provider <- \() stop("No provider object in this test.")
    orientation_test_tool_state(agent, source_calls = 1L)
    agent$run_async <- function(...) {
      promises::promise_resolve(orientation_test_agent_result(
        tool_calls = 1L,
        run_id = "deputy-orientation-no-submission"
      ))
    }
    agent$interrupt <- \(reason) TRUE
    agent
  }

  control <- maintain_orientation_async(
    store = store,
    reader_id = reader_id,
    worker_id = "orientation-worker-1",
    model = "openai/gpt-test",
    candidate_limit = 3L,
    destination_check = orientation_test_destination_check(store, reader_id),
    agent_factory = agent_factory,
    schedule_timeout = FALSE
  )
  failure <- NULL
  promises::then(
    control$promise,
    onRejected = \(error) failure <<- error
  )
  deadline <- Sys.time() + 2
  while (is.null(failure) && Sys.time() < deadline) {
    later::run_now(0.01)
  }
  run <- store_get_agent_run(store, reader_id, control$run$run_id)

  testthat::expect_s3_class(failure, "rill_orientation_agent_stopped")
  testthat::expect_identical(run$status, "failed")
  testthat::expect_identical(run$terminal_reason, "output_not_submitted")
  testthat::expect_null(store_get_orientation(store, reader_id))
})

testthat::test_that("a Reader's active question leaves Orientation maintenance busy", {
  store <- rill_store(list(demo_mode = TRUE))
  reader_id <- "reader-1"
  active <- store_start_agent_run(
    store,
    reader_id = reader_id,
    kind = "question",
    request_key = "question-in-progress",
    pinned_inputs = list(question = "What changed?"),
    worker_id = "question-worker"
  )

  control <- maintain_orientation_async(
    store = store,
    reader_id = reader_id,
    worker_id = "orientation-worker-1",
    model = "openai/gpt-test",
    candidate_limit = 3L,
    destination_check = orientation_test_destination_check(store, reader_id),
    agent_factory = \(...) stop("Agent setup is deferred by the run."),
    schedule_timeout = FALSE
  )

  testthat::expect_identical(control$status, "busy")
  testthat::expect_null(control$run)
  testthat::expect_null(control$promise)
  testthat::expect_s3_class(control$error, "rill_agent_run_conflict")
  testthat::expect_identical(
    store_get_agent_run(store, reader_id, active$run_id)$status,
    "pending"
  )
})

testthat::test_that("an external Reader question signals the Orientation owner", {
  store <- rill_store(list(demo_mode = TRUE))
  reader_id <- "reader-1"
  resolve_output <- NULL
  reject_output <- NULL
  interrupted <- NULL
  last_result <- list(
    stop_reason = "cancelled",
    usage = deputy::AgentUsage(requests = 1L, tool_calls = 1L),
    run_id = "deputy-orientation-preempted"
  )
  agent_factory <- function(...) {
    agent <- new.env(parent = emptyenv())
    agent$get_model <- \() "gpt-test"
    agent$get_provider <- \() stop("No provider object in this test.")
    state <- orientation_test_tool_state(agent)
    agent$run_async <- function(...) {
      promises::promise(function(resolve, reject) {
        resolve_output <<- function(output) {
          state$source_calls <- 1L
          orientation_test_submit(state, output)
          resolve(last_result)
        }
        reject_output <<- reject
      })
    }
    agent$interrupt <- function(reason) {
      interrupted <<- reason
      TRUE
    }
    agent
  }
  control <- maintain_orientation_async(
    store = store,
    reader_id = reader_id,
    worker_id = "orientation-owner",
    model = "openai/gpt-test",
    candidate_limit = 3L,
    destination_check = orientation_test_destination_check(store, reader_id),
    agent_factory = agent_factory
  )

  waiting <- store_start_prioritized_reader_question(
    store,
    reader_id = reader_id,
    request_key = "external-reader-question",
    pinned_inputs = list(document_id = "document-1"),
    worker_id = "question-owner"
  )
  deadline <- Sys.time() + 2
  while (is.null(interrupted) && Sys.time() < deadline) {
    later::run_now(0.05)
  }

  testthat::expect_identical(interrupted, "reader_question")
  testthat::expect_null(waiting$run)
  testthat::expect_identical(waiting$preempted$status, "cancelling")
  resolve_output(list(status = "Ignored after preemption.", cards = list()))
  deadline <- Sys.time() + 2
  settled <- NULL
  while (is.null(settled) && Sys.time() < deadline) {
    later::run_now(0.01)
    settled <- store_get_agent_run(store, reader_id, control$run$run_id)
    if (is.null(settled$deputy_run_id)) {
      settled <- NULL
    }
  }
  testthat::expect_identical(settled$status, "cancelled")
  testthat::expect_identical(settled$terminal_reason, "reader_question")
  testthat::expect_identical(
    settled$deputy_run_id,
    "deputy-orientation-preempted"
  )
})

testthat::test_that("an inactive Orientation releases a deferred question", {
  store <- rill_store(list(demo_mode = TRUE))
  reader_id <- "reader-1"
  resolve_run <- NULL
  interrupts <- character()
  agent_factory <- function(...) {
    agent <- new.env(parent = emptyenv())
    agent$get_model <- \() "gpt-test"
    agent$get_provider <- \() stop("No provider object in this test.")
    orientation_test_tool_state(agent)
    agent$run_async <- function(...) {
      promises::promise(function(resolve, reject) {
        resolve_run <<- resolve
      })
    }
    agent$interrupt <- function(reason) {
      interrupts <<- c(interrupts, reason)
      FALSE
    }
    agent
  }
  control <- maintain_orientation_async(
    store = store,
    reader_id = reader_id,
    worker_id = "orientation-owner",
    model = "openai/gpt-test",
    candidate_limit = 3L,
    destination_check = orientation_test_destination_check(store, reader_id),
    agent_factory = agent_factory
  )
  pinned_inputs <- list(
    document_id = "document-for-question",
    question = "What changed?"
  )

  waiting <- store_start_prioritized_reader_question(
    store,
    reader_id = reader_id,
    request_key = "question-after-inactive-orientation",
    pinned_inputs = pinned_inputs,
    worker_id = "question-owner"
  )
  orientation_run <- store_get_agent_run(
    store,
    reader_id,
    control$run$run_id
  )
  resumed <- store_start_prioritized_reader_question(
    store,
    reader_id = reader_id,
    request_key = waiting$deferred$request_key,
    pinned_inputs = waiting$deferred$pinned_inputs,
    requested_at = waiting$deferred$requested_at,
    worker_id = "question-owner"
  )

  testthat::expect_identical(interrupts, "reader_question")
  testthat::expect_identical(orientation_run$status, "cancelled")
  testthat::expect_identical(
    orientation_run$terminal_reason,
    "reader_question"
  )
  testthat::expect_identical(resumed$run$status, "pending")
  testthat::expect_null(store_get_deferred_reader_question(store, reader_id))

  resolve_run(orientation_test_agent_result(
    stop_reason = "reader_question",
    run_id = "deputy-orientation-inactive"
  ))
  later::run_now(0.01)
})

testthat::test_that("Orientation retries transient interruption", {
  testthat::local_mocked_bindings(
    agent_run_interrupt_retry_delay = \(attempt) 0
  )
  store <- rill_store(list(demo_mode = TRUE))
  reader_id <- "reader-1"
  resolve_run <- NULL
  interrupts <- character()
  agent_factory <- function(...) {
    agent <- new.env(parent = emptyenv())
    agent$get_model <- \() "gpt-test"
    agent$get_provider <- \() stop("No provider object in this test.")
    orientation_test_tool_state(agent)
    agent$run_async <- function(...) {
      promises::promise(function(resolve, reject) {
        resolve_run <<- resolve
      })
    }
    agent$interrupt <- function(reason) {
      interrupts <<- c(interrupts, reason)
      if (length(interrupts) == 1L) {
        stop("The controller was briefly unavailable.")
      }
      FALSE
    }
    agent
  }
  control <- maintain_orientation_async(
    store = store,
    reader_id = reader_id,
    worker_id = "orientation-owner",
    model = "openai/gpt-test",
    candidate_limit = 3L,
    destination_check = orientation_test_destination_check(store, reader_id),
    agent_factory = agent_factory,
    schedule_timeout = FALSE
  )

  first <- control$interrupt("reader_question")
  control$interrupt("wall_time_limit")
  settled <- NULL
  deadline <- Sys.time() + 0.5
  while (is.null(settled) && Sys.time() < deadline) {
    later::run_now(0.01)
    candidate <- store_get_agent_run(store, reader_id, control$run$run_id)
    if (candidate$status %in% c("cancelled", "interrupted")) {
      settled <- candidate
    }
  }

  testthat::expect_identical(first$interrupted, NA)
  testthat::expect_identical(
    interrupts,
    c("reader_question", "reader_question")
  )
  testthat::expect_identical(settled$status, "cancelled")
  testthat::expect_identical(settled$terminal_reason, "reader_question")

  resolve_run(orientation_test_agent_result(
    stop_reason = "reader_question",
    run_id = "deputy-orientation-retried"
  ))
  later::run_now(0.01)
})

testthat::test_that("exhausted Orientation interruption releases a question", {
  testthat::local_mocked_bindings(
    agent_run_interrupt_retry_delay = \(attempt) 0
  )
  store <- rill_store(list(demo_mode = TRUE))
  reader_id <- "reader-1"
  resolve_run <- NULL
  interrupts <- character()
  agent_factory <- function(...) {
    agent <- new.env(parent = emptyenv())
    agent$get_model <- \() "gpt-test"
    agent$get_provider <- \() stop("No provider object in this test.")
    orientation_test_tool_state(agent)
    agent$run_async <- function(...) {
      promises::promise(function(resolve, reject) {
        resolve_run <<- resolve
      })
    }
    agent$interrupt <- function(reason) {
      interrupts <<- c(interrupts, reason)
      stop("The controller is unavailable.")
    }
    agent
  }
  control <- maintain_orientation_async(
    store = store,
    reader_id = reader_id,
    worker_id = "orientation-owner",
    model = "openai/gpt-test",
    candidate_limit = 3L,
    destination_check = orientation_test_destination_check(store, reader_id),
    agent_factory = agent_factory
  )
  pinned_inputs <- list(
    document_id = "document-for-question",
    question = "What changed?"
  )

  waiting <- store_start_prioritized_reader_question(
    store,
    reader_id = reader_id,
    request_key = "question-after-lost-orientation-controller",
    pinned_inputs = pinned_inputs,
    worker_id = "question-owner"
  )
  control$interrupt("wall_time_limit")
  interrupted_run <- NULL
  deadline <- Sys.time() + 0.5
  while (is.null(interrupted_run) && Sys.time() < deadline) {
    later::run_now(0.01)
    candidate <- store_get_agent_run(store, reader_id, control$run$run_id)
    if (identical(candidate$status, "interrupted")) {
      interrupted_run <- candidate
    }
  }
  resumed <- store_start_prioritized_reader_question(
    store,
    reader_id = reader_id,
    request_key = waiting$deferred$request_key,
    pinned_inputs = waiting$deferred$pinned_inputs,
    requested_at = waiting$deferred$requested_at,
    worker_id = "question-owner"
  )

  testthat::expect_identical(interrupts, rep("reader_question", 3L))
  testthat::expect_identical(
    interrupted_run$terminal_reason,
    "interrupt_unconfirmed:reader_question"
  )
  testthat::expect_identical(resumed$run$status, "pending")
  testthat::expect_null(store_get_deferred_reader_question(store, reader_id))

  resolve_run(orientation_test_agent_result(
    stop_reason = "reader_question",
    run_id = "deputy-orientation-lost-controller"
  ))
  later::run_now(0.01)
})

testthat::test_that("unconfirmed Orientation interruption is bounded", {
  testthat::local_mocked_bindings(
    agent_run_interrupt_confirmation_seconds = \() 0
  )
  store <- rill_store(list(demo_mode = TRUE))
  reader_id <- "reader-1"
  resolve_run <- NULL
  agent_factory <- function(...) {
    agent <- new.env(parent = emptyenv())
    agent$get_model <- \() "gpt-test"
    agent$get_provider <- \() stop("No provider object in this test.")
    orientation_test_tool_state(agent)
    agent$run_async <- function(...) {
      promises::promise(function(resolve, reject) {
        resolve_run <<- resolve
      })
    }
    agent$interrupt <- \(reason) TRUE
    agent
  }
  control <- maintain_orientation_async(
    store = store,
    reader_id = reader_id,
    worker_id = "orientation-owner",
    model = "openai/gpt-test",
    candidate_limit = 3L,
    destination_check = orientation_test_destination_check(store, reader_id),
    agent_factory = agent_factory,
    schedule_timeout = FALSE
  )

  requested <- control$interrupt("orientation_disabled")
  interrupted_run <- NULL
  deadline <- Sys.time() + 0.5
  while (is.null(interrupted_run) && Sys.time() < deadline) {
    later::run_now(0.01)
    candidate <- store_get_agent_run(store, reader_id, control$run$run_id)
    if (identical(candidate$status, "interrupted")) {
      interrupted_run <- candidate
    }
  }

  testthat::expect_identical(requested$interrupted, TRUE)
  testthat::expect_identical(
    interrupted_run$terminal_reason,
    "interrupt_unconfirmed:orientation_disabled"
  )

  late_result <- NULL
  late_error <- NULL
  promises::then(
    control$promise,
    \(value) late_result <<- value,
    onRejected = \(error) late_error <<- error
  )
  resolve_run(orientation_test_agent_result(
    stop_reason = "complete",
    run_id = "deputy-orientation-late-completion"
  ))
  deadline <- Sys.time() + 0.5
  while (
    is.null(late_result) &&
      is.null(late_error) &&
      Sys.time() < deadline
  ) {
    later::run_now(0.01)
  }
  terminal <- store_get_agent_run(store, reader_id, control$run$run_id)
  testthat::expect_null(late_error)
  testthat::expect_identical(late_result$run$status, "interrupted")
  testthat::expect_identical(terminal$status, "interrupted")
  testthat::expect_identical(
    terminal$deputy_run_id,
    "deputy-orientation-late-completion"
  )
  testthat::expect_null(store_get_orientation(store, reader_id))
})

testthat::test_that("process recovery interrupts the Orientation owner", {
  store <- rill_store(list(demo_mode = TRUE))
  reader_id <- "reader-1"
  resolve_run <- NULL
  interrupted <- NULL
  agent_factory <- function(...) {
    agent <- new.env(parent = emptyenv())
    agent$get_model <- \() "gpt-test"
    agent$get_provider <- \() stop("No provider object in this test.")
    orientation_test_tool_state(agent)
    agent$run_async <- function(...) {
      promises::promise(function(resolve, reject) {
        resolve_run <<- resolve
      })
    }
    agent$interrupt <- function(reason) {
      interrupted <<- reason
      TRUE
    }
    agent
  }
  control <- maintain_orientation_async(
    store = store,
    reader_id = reader_id,
    worker_id = "orientation-owner",
    model = "openai/gpt-test",
    candidate_limit = 3L,
    destination_check = orientation_test_destination_check(store, reader_id),
    agent_factory = agent_factory
  )

  recovered <- store_interrupt_agent_runs(
    store,
    recovery = "process_restart",
    recovered_at = control$deadline
  )
  deadline <- Sys.time() + 2
  while (is.null(interrupted) && Sys.time() < deadline) {
    later::run_now(0.05)
  }

  testthat::expect_length(recovered, 1L)
  testthat::expect_identical(interrupted, "process_restarted")
  terminal <- store_get_agent_run(store, reader_id, control$run$run_id)
  testthat::expect_identical(terminal$status, "interrupted")
  testthat::expect_identical(terminal$terminal_reason, "process_restarted")

  resolve_run(orientation_test_agent_result(
    stop_reason = "process_restarted",
    run_id = "deputy-orientation-expired"
  ))
  settled <- NULL
  promises::then(control$promise, \(value) settled <<- value)
  deadline <- Sys.time() + 2
  while (is.null(settled) && Sys.time() < deadline) {
    later::run_now(0.01)
  }
  testthat::expect_identical(
    settled$run$terminal_reason,
    "process_restarted"
  )
  testthat::expect_identical(
    settled$run$deputy_run_id,
    "deputy-orientation-expired"
  )
})

testthat::test_that("a terminal boundary run gets one automatic retry", {
  store <- rill_store(list(demo_mode = TRUE))
  reader_id <- "reader-1"
  failed_agent_factory <- function(...) {
    agent <- new.env(parent = emptyenv())
    agent$get_model <- \() "gpt-test"
    agent$get_provider <- \() stop("No provider object in this test.")
    orientation_test_tool_state(agent)
    agent$run_async <- function(...) {
      promises::promise_reject(simpleError("Provider unavailable."))
    }
    agent$interrupt <- \(reason) TRUE
    agent
  }
  first <- maintain_orientation_async(
    store = store,
    reader_id = reader_id,
    worker_id = "orientation-worker-1",
    model = "openai/gpt-test",
    candidate_limit = 3L,
    destination_check = orientation_test_destination_check(store, reader_id),
    agent_factory = failed_agent_factory,
    schedule_timeout = FALSE
  )
  first_error <- NULL
  promises::then(
    first$promise,
    onRejected = \(error) first_error <<- error
  )
  deadline <- Sys.time() + 2
  while (is.null(first_error) && Sys.time() < deadline) {
    later::run_now(0.01)
  }

  replay <- maintain_orientation_async(
    store = store,
    reader_id = reader_id,
    worker_id = "orientation-worker-2",
    model = "openai/gpt-test",
    candidate_limit = 3L,
    destination_check = orientation_test_destination_check(store, reader_id),
    agent_factory = failed_agent_factory,
    schedule_timeout = FALSE
  )

  successful_agent_factory <- function(candidates, ...) {
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
      promises::promise_resolve(orientation_test_agent_result(
        run_id = "deputy-orientation-retry"
      ))
    }
    agent$interrupt <- \(reason) TRUE
    agent
  }
  retry_id <- orientation_automatic_retry_id()
  retry <- maintain_orientation_async(
    store = store,
    reader_id = reader_id,
    worker_id = "orientation-worker-2",
    model = "openai/gpt-test",
    candidate_limit = 3L,
    destination_check = orientation_test_destination_check(store, reader_id),
    retry_id = retry_id,
    agent_factory = successful_agent_factory,
    schedule_timeout = FALSE
  )
  retry_replay <- maintain_orientation_async(
    store = store,
    reader_id = reader_id,
    worker_id = "orientation-worker-2",
    model = "openai/gpt-test",
    candidate_limit = 3L,
    destination_check = orientation_test_destination_check(store, reader_id),
    retry_id = retry_id,
    agent_factory = successful_agent_factory,
    schedule_timeout = FALSE
  )
  retried <- NULL
  promises::then(retry$promise, \(value) retried <<- value)
  deadline <- Sys.time() + 2
  while (is.null(retried) && Sys.time() < deadline) {
    later::run_now(0.01)
  }

  expected_retry_key <- orientation_retry_request_key(
    first$run$request_key,
    retry_id
  )
  testthat::expect_s3_class(first_error, "simpleError")
  testthat::expect_identical(replay$status, "failed")
  testthat::expect_identical(replay$run$run_id, first$run$run_id)
  testthat::expect_identical(retry$run$retry_of_run_id, first$run$run_id)
  testthat::expect_identical(retry$run$request_key, expected_retry_key)
  testthat::expect_identical(retry_replay$status, "running")
  testthat::expect_identical(retry_replay$run$run_id, retry$run$run_id)
  testthat::expect_null(retry_replay$promise)
  testthat::expect_identical(
    orientation_retry_request_key(first$run$request_key, retry_id),
    expected_retry_key
  )
  testthat::expect_identical(retried$run$status, "completed")
  testthat::expect_identical(
    store_get_orientation(store, reader_id)$agent_run_id,
    retry$run$run_id
  )
})

testthat::test_that("Orientation Agent errors terminalize the owned run", {
  store <- rill_store(list(demo_mode = TRUE))
  reader_id <- "reader-1"
  agent_factory <- function(...) {
    agent <- new.env(parent = emptyenv())
    agent$get_model <- \() "gpt-test"
    agent$get_provider <- \() stop("No provider object in this test.")
    orientation_test_tool_state(agent)
    agent$run_async <- function(...) {
      promises::promise_reject(simpleError("Provider unavailable."))
    }
    agent$interrupt <- \(reason) TRUE
    agent
  }

  control <- maintain_orientation_async(
    store = store,
    reader_id = reader_id,
    worker_id = "orientation-worker-1",
    model = "openai/gpt-test",
    candidate_limit = 3L,
    destination_check = orientation_test_destination_check(store, reader_id),
    agent_factory = agent_factory,
    schedule_timeout = FALSE
  )
  rejected <- NULL
  promises::then(
    control$promise,
    onRejected = \(error) rejected <<- error
  )
  deadline <- Sys.time() + 2
  while (is.null(rejected) && Sys.time() < deadline) {
    later::run_now(0.01)
  }
  run <- store_get_agent_run(store, reader_id, control$run$run_id)

  testthat::expect_s3_class(rejected, "simpleError")
  testthat::expect_identical(run$status, "failed")
  testthat::expect_identical(
    run$terminal_reason,
    "agent_error:simpleError"
  )
  testthat::expect_null(store_get_orientation(store, reader_id))
})

testthat::test_that("a governed stop preserves Deputy terminal metadata", {
  store <- rill_store(list(demo_mode = TRUE))
  reader_id <- "reader-1"
  agent_factory <- function(...) {
    agent <- new.env(parent = emptyenv())
    agent$get_model <- \() "gpt-test"
    agent$get_provider <- \() stop("No provider object in this test.")
    orientation_test_tool_state(agent, source_calls = 1L)
    agent$run_async <- function(...) {
      promises::promise_resolve(orientation_test_agent_result(
        stop_reason = "request_limit",
        requests = 4L,
        tool_calls = 1L,
        run_id = "deputy-orientation-limited"
      ))
    }
    agent$interrupt <- \(reason) TRUE
    agent
  }

  control <- maintain_orientation_async(
    store = store,
    reader_id = reader_id,
    worker_id = "orientation-worker-1",
    model = "openai/gpt-test",
    candidate_limit = 3L,
    destination_check = orientation_test_destination_check(store, reader_id),
    agent_factory = agent_factory,
    schedule_timeout = FALSE
  )
  rejected <- NULL
  promises::then(
    control$promise,
    onRejected = \(error) rejected <<- error
  )
  deadline <- Sys.time() + 2
  while (is.null(rejected) && Sys.time() < deadline) {
    later::run_now(0.01)
  }
  run <- store_get_agent_run(store, reader_id, control$run$run_id)

  testthat::expect_s3_class(rejected, "rill_orientation_agent_stopped")
  testthat::expect_identical(run$status, "failed")
  testthat::expect_identical(run$terminal_reason, "request_limit")
  testthat::expect_identical(run$usage$requests, 4L)
  testthat::expect_identical(
    run$usage$deputy_stop_reason,
    "request_limit"
  )
  testthat::expect_identical(
    run$deputy_run_id,
    "deputy-orientation-limited"
  )
})

testthat::test_that("a changed source boundary rejects stale publication", {
  store <- rill_store(list(demo_mode = TRUE))
  reader_id <- "reader-1"
  resolve_output <- NULL
  candidates <- NULL
  agent_factory <- function(
    candidates,
    reader_id,
    session_id,
    boundary_hash,
    model
  ) {
    candidates <<- candidates
    agent <- new.env(parent = emptyenv())
    agent$get_model <- \() "gpt-test"
    agent$get_provider <- \() stop("No provider object in this test.")
    state <- orientation_test_tool_state(agent)
    agent$run_async <- function(...) {
      promises::promise(function(resolve, reject) {
        resolve_output <<- function(output) {
          state$source_calls <- 1L
          orientation_test_submit(state, output)
          resolve(orientation_test_agent_result(
            run_id = "deputy-orientation-stale"
          ))
        }
      })
    }
    agent$interrupt <- \(reason) TRUE
    agent
  }

  control <- maintain_orientation_async(
    store = store,
    reader_id = reader_id,
    worker_id = "orientation-worker-1",
    model = "openai/gpt-test",
    candidate_limit = 3L,
    destination_check = orientation_test_destination_check(store, reader_id),
    agent_factory = agent_factory,
    schedule_timeout = FALSE
  )
  selected <- candidates[[1L]]$document
  store_mark_opened(store, reader_id, selected$entry_id)
  resolve_output(list(
    status = "One source boundary deserves attention.",
    question = "What changed?",
    introduction = "Begin here.",
    cards = list(list(
      document_id = selected$document_id,
      role = "anchor",
      frame = "change",
      interpretation = "This was selected from the old boundary.",
      why_now = "It had been unread when maintenance began.",
      evidence = "Rill keeps the source feed"
    ))
  ))
  rejected <- NULL
  promises::then(
    control$promise,
    onRejected = \(error) rejected <<- error
  )
  deadline <- Sys.time() + 2
  while (is.null(rejected) && Sys.time() < deadline) {
    later::run_now(0.01)
  }
  run <- store_get_agent_run(store, reader_id, control$run$run_id)

  testthat::expect_s3_class(rejected, "rill_orientation_boundary_changed")
  testthat::expect_identical(run$status, "failed")
  testthat::expect_identical(
    run$terminal_reason,
    "agent_error:rill_orientation_boundary_changed"
  )
  testthat::expect_null(store_get_orientation(store, reader_id))
})

testthat::test_that("Orientation exposes cancellation and wall-time boundaries", {
  store <- rill_store(list(demo_mode = TRUE))
  reader_id <- "reader-1"
  resolve_output <- NULL
  reject_output <- NULL
  last_result <- NULL
  interrupted <- character()
  agent_factory <- function(...) {
    agent <- new.env(parent = emptyenv())
    agent$get_model <- \() "gpt-test"
    agent$get_provider <- \() stop("No provider object in this test.")
    state <- orientation_test_tool_state(agent)
    agent$run_async <- function(...) {
      promises::promise(function(resolve, reject) {
        resolve_output <<- function(output) {
          state$source_calls <- 1L
          orientation_test_submit(state, output)
          resolve(last_result)
        }
        reject_output <<- \(error) reject(error)
      })
    }
    agent$last_run <- \() last_result
    agent$interrupt <- function(reason) {
      interrupted <<- c(interrupted, reason)
      FALSE
    }
    agent
  }

  cancelled <- maintain_orientation_async(
    store = store,
    reader_id = reader_id,
    worker_id = "orientation-worker-1",
    model = "openai/gpt-test",
    candidate_limit = 3L,
    destination_check = orientation_test_destination_check(store, reader_id),
    agent_factory = agent_factory,
    schedule_timeout = FALSE
  )
  cancel_result <- cancelled$interrupt("reader_question")

  testthat::expect_identical(cancel_result$run$status, "cancelled")
  testthat::expect_identical(cancel_result$interrupted, FALSE)
  testthat::expect_identical(interrupted, "reader_question")
  later_timeout <- cancelled$interrupt("wall_time_limit")
  testthat::expect_identical(later_timeout$interrupted, FALSE)
  testthat::expect_identical(interrupted, "reader_question")
  testthat::expect_identical(
    store_get_agent_run(store, reader_id, cancelled$run$run_id)$status,
    "cancelled"
  )

  last_result <- list(
    stop_reason = "reader_question",
    usage = deputy::AgentUsage(requests = 2L, tool_calls = 1L),
    run_id = "deputy-orientation-cancelled"
  )
  resolve_output(list(status = "Ignored", cards = list()))
  deadline <- Sys.time() + 2
  cancelled_run <- NULL
  while (
    (is.null(cancelled_run) || !identical(cancelled_run$status, "cancelled")) &&
      Sys.time() < deadline
  ) {
    later::run_now(0.01)
    cancelled_run <- store_get_agent_run(
      store,
      reader_id,
      cancelled$run$run_id
    )
  }

  testthat::expect_identical(cancelled_run$status, "cancelled")
  testthat::expect_identical(
    cancelled_run$terminal_reason,
    "reader_question"
  )
  testthat::expect_identical(cancelled_run$usage$requests, 2L)
  testthat::expect_identical(
    cancelled_run$deputy_run_id,
    "deputy-orientation-cancelled"
  )

  disabled_store <- rill_store(list(demo_mode = TRUE))
  interrupted <- character()
  last_result <- NULL
  disabled <- maintain_orientation_async(
    store = disabled_store,
    reader_id = reader_id,
    worker_id = "orientation-worker-disabled",
    model = "openai/gpt-test",
    candidate_limit = 3L,
    destination_check = orientation_test_destination_check(
      disabled_store,
      reader_id
    ),
    agent_factory = agent_factory,
    schedule_timeout = FALSE
  )
  disabled$interrupt("orientation_disabled")
  last_result <- list(
    stop_reason = "orientation_disabled",
    usage = deputy::AgentUsage(requests = 1L),
    run_id = "deputy-orientation-disabled"
  )
  resolve_output(list(status = "Ignored", cards = list()))
  deadline <- Sys.time() + 2
  disabled_run <- NULL
  while (
    (is.null(disabled_run) || !identical(disabled_run$status, "cancelled")) &&
      Sys.time() < deadline
  ) {
    later::run_now(0.01)
    disabled_run <- store_get_agent_run(
      disabled_store,
      reader_id,
      disabled$run$run_id
    )
  }
  testthat::expect_identical(disabled_run$status, "cancelled")
  testthat::expect_identical(
    disabled_run$terminal_reason,
    "orientation_disabled"
  )

  completed_store <- rill_store(list(demo_mode = TRUE))
  last_result <- list(
    stop_reason = "complete",
    usage = deputy::AgentUsage(requests = 1L, tool_calls = 1L),
    run_id = "deputy-orientation-completed"
  )
  completed <- maintain_orientation_async(
    store = completed_store,
    reader_id = reader_id,
    worker_id = "orientation-worker-completed",
    model = "openai/gpt-test",
    candidate_limit = 3L,
    destination_check = orientation_test_destination_check(
      completed_store,
      reader_id
    ),
    agent_factory = agent_factory,
    schedule_timeout = FALSE
  )
  completion_race <- completed$interrupt("session_ended")
  testthat::expect_identical(completion_race$interrupted, FALSE)
  resolve_output(list(
    status = "The source boundary was inspected.",
    cards = list()
  ))
  deadline <- Sys.time() + 2
  completed_run <- NULL
  while (
    (is.null(completed_run) ||
      !identical(completed_run$status, "completed")) &&
      Sys.time() < deadline
  ) {
    later::run_now(0.01)
    completed_run <- store_get_agent_run(
      completed_store,
      reader_id,
      completed$run$run_id
    )
  }
  testthat::expect_identical(completed_run$status, "completed")
  testthat::expect_s3_class(
    store_get_orientation(completed_store, reader_id),
    "rill_orientation"
  )

  failed_store <- rill_store(list(demo_mode = TRUE))
  last_result <- list(
    stop_reason = "provider_error",
    usage = deputy::AgentUsage(requests = 1L, tool_calls = 1L),
    run_id = "deputy-orientation-failed"
  )
  failed <- maintain_orientation_async(
    store = failed_store,
    reader_id = reader_id,
    worker_id = "orientation-worker-failed",
    model = "openai/gpt-test",
    candidate_limit = 3L,
    destination_check = orientation_test_destination_check(
      failed_store,
      reader_id
    ),
    agent_factory = agent_factory,
    schedule_timeout = FALSE
  )
  failure_race <- failed$interrupt("session_ended")
  testthat::expect_identical(failure_race$interrupted, FALSE)
  provider_failure <- structure(
    simpleError("The provider failed."),
    stop_reason = "provider_error"
  )
  failed_rejection <- NULL
  promises::then(
    failed$promise,
    onRejected = function(error) {
      failed_rejection <<- error
      NULL
    }
  )
  reject_output(provider_failure)
  deadline <- Sys.time() + 2
  failed_run <- NULL
  while (
    (is.null(failed_run) ||
      !identical(failed_run$status, "failed") ||
      is.null(failed_rejection)) &&
      Sys.time() < deadline
  ) {
    later::run_now(0.01)
    failed_run <- store_get_agent_run(
      failed_store,
      reader_id,
      failed$run$run_id
    )
  }
  testthat::expect_identical(failed_run$status, "failed")
  testthat::expect_identical(failed_run$terminal_reason, "provider_error")
  testthat::expect_identical(failed_rejection, provider_failure)

  timed_store <- rill_store(list(demo_mode = TRUE))
  interrupted <- character()
  last_result <- NULL
  timed <- maintain_orientation_async(
    store = timed_store,
    reader_id = reader_id,
    worker_id = "orientation-worker-2",
    model = "openai/gpt-test",
    candidate_limit = 3L,
    destination_check = orientation_test_destination_check(
      timed_store,
      reader_id
    ),
    agent_factory = agent_factory,
    schedule_timeout = FALSE
  )
  timeout_result <- timed$interrupt("wall_time_limit")
  later_cancel <- timed$interrupt("reader_question")

  testthat::expect_identical(timeout_result$run$status, "failed")
  testthat::expect_identical(later_cancel$interrupted, FALSE)
  testthat::expect_identical(interrupted, "wall_time_limit")
  testthat::expect_identical(
    timeout_result$run$terminal_reason,
    "wall_time_limit"
  )
  testthat::expect_type(timeout_result$run$terminal_at, "character")
  testthat::expect_identical(
    timed$deadline,
    timed$run$started_at + rill_orientation_wall_time_seconds()
  )

  last_result <- list(
    stop_reason = "wall_time_limit",
    usage = deputy::AgentUsage(requests = 4L, tool_calls = 2L),
    run_id = "deputy-orientation-timed-out"
  )
  resolve_output(list(status = "Late output", cards = list()))
  deadline <- Sys.time() + 2
  enriched <- NULL
  while (
    (is.null(enriched) || is.null(enriched$deputy_run_id)) &&
      Sys.time() < deadline
  ) {
    later::run_now(0.01)
    enriched <- store_get_agent_run(timed_store, reader_id, timed$run$run_id)
  }

  testthat::expect_identical(enriched$status, "failed")
  testthat::expect_identical(enriched$terminal_reason, "wall_time_limit")
  testthat::expect_identical(enriched$usage$requests, 4L)
  testthat::expect_identical(
    enriched$usage$deputy_stop_reason,
    "wall_time_limit"
  )
  testthat::expect_identical(
    enriched$deputy_run_id,
    "deputy-orientation-timed-out"
  )
  testthat::expect_null(store_get_orientation(timed_store, reader_id))
})
