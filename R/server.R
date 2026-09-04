rill_question_interrupt_registry <- new.env(parent = emptyenv())
rill_question_terminal_intents <- new.env(parent = emptyenv())
rill_question_drain_registry <- new.env(parent = emptyenv())

rill_register_question_interrupt <- function(run_id, interrupt) {
  rill_question_interrupt_registry[[run_id]] <- interrupt
  invisible(NULL)
}

rill_unregister_question_interrupt <- function(run_id) {
  rill_question_interrupt_registry[[run_id]] <- NULL
  drain <- rill_question_drain_registry[[run_id]]
  if (!is.null(drain) && is.function(drain$cancel)) {
    try(drain$cancel(), silent = TRUE)
  }
  rill_question_drain_registry[[run_id]] <- NULL
  invisible(NULL)
}

rill_register_question_drain <- function(run_id, on_result, cancel) {
  rill_question_drain_registry[[run_id]] <- list(
    on_result = on_result,
    cancel = cancel
  )
  invisible(NULL)
}

rill_signal_question_interrupt <- function(run_id, reason, fallback = NULL) {
  interrupt <- rill_question_interrupt_registry[[run_id]]
  if (!is.function(interrupt)) {
    interrupt <- fallback
  }
  result <- if (is.function(interrupt)) {
    tryCatch(interrupt(reason), error = \(error) NA)
  } else {
    NULL
  }
  drain <- rill_question_drain_registry[[run_id]]
  if (!is.null(drain) && is.function(drain$on_result)) {
    drain$on_result(reason, result)
  }
  result
}

rill_question_cancellation_confirmed <- function(
  reason,
  requested_reason = NULL
) {
  is.character(reason) &&
    length(reason) == 1L &&
    !is.na(reason) &&
    reason %in% unique(c("cancelled", "reader_cancelled", requested_reason))
}

rill_assert_question_runtime_identity <- function(pinned_inputs, runtime) {
  if (is.null(pinned_inputs)) {
    return(invisible(runtime))
  }
  expected <- list(
    model = pinned_inputs$model %||% NULL,
    data_destination = pinned_inputs$data_destination %||% NULL,
    data_destination_id = pinned_inputs$data_destination_id %||% NULL
  )
  actual <- list(
    model = runtime$model %||% NULL,
    data_destination = runtime$data_destination %||% NULL,
    data_destination_id = runtime$data_destination_id %||% NULL
  )
  if (!identical(expected, actual)) {
    cli::cli_abort(
      c(
        "The configured model destination changed before Rill could answer.",
        "i" = "Ask the question again to confirm the current destination."
      ),
      class = "rill_agent_runtime_identity_changed"
    )
  }
  invisible(runtime)
}

rill_server <- function(config, store) {
  force(config)
  force(store)

  function(input, output, session, reader_id = config$actor_id) {
    actor_id <- reader_id
    session_id <- rill_id(
      "session",
      actor_id,
      session$token,
      utc_now(),
      stats::runif(1)
    )
    selected_id <- shiny::reactiveVal(NULL)
    selected_document_id <- shiny::reactiveVal(NULL)
    selected_orientation_provenance <- shiny::reactiveVal(NULL)
    selected_position <- shiny::reactiveVal(NA_integer_)
    selected_feed <- shiny::reactiveVal(NULL)
    orientation_preparing <- shiny::reactiveVal(FALSE)
    orientation_control <- shiny::reactiveVal(NULL)
    orientation_attempted_boundary <- shiny::reactiveVal(NULL)
    orientation_destination_tick <- shiny::reactiveVal(0L)
    orientation_destination_poll_token <- NULL
    browse_queue_pending <- shiny::reactiveVal(FALSE)
    orientation_poll_token <- NULL
    orientation_retry_cancel <- NULL
    retained_ids <- shiny::reactiveVal(character())
    retained_context <- shiny::reactiveVal(NULL)
    refresh_tick <- shiny::reactiveVal(0L)
    status_text <- shiny::reactiveVal(NULL)
    status_kind <- shiny::reactiveVal("info")
    reader_agent <- shiny::reactiveVal(NULL)
    reader_agent_document_id <- shiny::reactiveVal(NULL)
    active_agent_run <- shiny::reactiveVal(NULL)
    draining_agent_run_id <- shiny::reactiveVal(NULL)
    pending_reader_question <- shiny::reactiveVal(NULL)
    agent_request_index <- shiny::reactiveVal(0L)
    agent_run_deadlines <- new.env(parent = emptyenv())
    agent_run_terminal_intents <- rill_question_terminal_intents
    agent_run_stop_confirmations <- new.env(parent = emptyenv())
    pending_reader_question_cancel <- NULL
    deferred_reader_question_resume_cancel <- NULL
    visible_agent_run_poll_cancel <- NULL

    existing_active_agent_run <- tryCatch(
      store_get_active_agent_run(store, actor_id),
      error = \(error) NULL
    )
    existing_agent_run <- if (
      !is.null(existing_active_agent_run) &&
        identical(existing_active_agent_run$kind, "question")
    ) {
      existing_active_agent_run
    } else if (is.null(existing_active_agent_run)) {
      tryCatch(
        store_get_latest_question_agent_run(store, actor_id),
        error = \(error) NULL
      )
    } else {
      NULL
    }
    if (
      !is.null(existing_agent_run) &&
        identical(existing_agent_run$kind, "question")
    ) {
      active_agent_run(existing_agent_run)
      document <- tryCatch(
        store_get_document_by_id(
          store,
          actor_id,
          existing_agent_run$pinned_inputs$document_id
        ),
        error = \(error) NULL
      )
      if (!is.null(document)) {
        selected_id(document$entry_id)
        selected_document_id(document$document_id)
      }
    }

    bump_refresh <- function() {
      refresh_tick(shiny::isolate(refresh_tick()) + 1L)
    }

    acknowledge_orientation_queue <- function() {
      session$onFlushed(
        \() session$sendCustomMessage("rill-browse-queue-ready", list()),
        once = TRUE
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

    schedule_orientation_retry <- function(delay = 2) {
      if (is.function(orientation_retry_cancel)) {
        return(invisible(NULL))
      }
      orientation_retry_cancel <<- later::later(
        function() {
          orientation_retry_cancel <<- NULL
          if (!session$isClosed()) {
            orientation_attempted_boundary(NULL)
            bump_refresh()
          }
          NULL
        },
        delay = delay
      )
      invisible(NULL)
    }

    current_context <- function() {
      list(
        view = input$view %||% "unread",
        feed_id = selected_feed(),
        sort = input$story_sort %||% "newest"
      )
    }

    clear_selection <- function(clear_retained = TRUE, force = FALSE) {
      if (!isTRUE(force) && reader_response_in_flight()) {
        return(invisible(FALSE))
      }
      selected_id(NULL)
      selected_document_id(NULL)
      selected_orientation_provenance(NULL)
      selected_position(NA_integer_)
      if (clear_retained) {
        retained_ids(character())
        retained_context(NULL)
      }
      invisible(TRUE)
    }

    retain_entry <- function(entry_id) {
      context <- current_context()
      ids <- retained_ids()
      if (!identical(retained_context(), context)) {
        ids <- character()
      }
      retained_context(context)
      retained_ids(unique(c(ids, entry_id)))
    }

    record_event <- function(
      type,
      entry_id = NULL,
      surface = "reader",
      position = NULL,
      payload = list(),
      event_id = NULL,
      happened_at = NULL
    ) {
      event <- list(
        event_id = event_id %||%
          rill_id("event", session_id, type, utc_now(), stats::runif(1)),
        reader_id = actor_id,
        entry_id = entry_id,
        session_id = session_id,
        event_type = type,
        happened_at = happened_at %||% utc_now(),
        surface = surface,
        position = position,
        payload = payload
      )
      tryCatch(
        store_record_event(store, event),
        error = function(error) {
          telemetry_log(
            "warn",
            "event.write_failed",
            list("event.type" = type, "error.type" = class(error)[[1]])
          )
        }
      )
      invisible(event)
    }

    terminal_agent_run_statuses <- c(
      "completed",
      "failed",
      "cancelled",
      "interrupted"
    )
    completed_response_grace_seconds <- 2

    completed_response_may_arrive <- function(run) {
      if (
        !identical(run$status, "completed") ||
          is.null(run$terminal_at)
      ) {
        return(FALSE)
      }
      terminal_at <- tryCatch(
        as.POSIXct(run$terminal_at, tz = "UTC"),
        error = \(error) as.POSIXct(NA, tz = "UTC")
      )
      length(terminal_at) == 1L &&
        !is.na(terminal_at) &&
        Sys.time() < terminal_at + completed_response_grace_seconds
    }

    schedule_visible_agent_run_poll <- NULL
    schedule_visible_agent_run_poll <- function(run_id, delay = 0.05) {
      if (is.function(visible_agent_run_poll_cancel)) {
        return(invisible(NULL))
      }
      visible_agent_run_poll_cancel <<- later::later(
        function() {
          visible_agent_run_poll_cancel <<- NULL
          if (session$isClosed()) {
            return(NULL)
          }
          visible <- shiny::isolate(active_agent_run())
          if (is.null(visible) || !identical(visible$run_id, run_id)) {
            return(NULL)
          }
          current <- tryCatch(
            store_get_agent_run(store, actor_id, run_id),
            error = function(error) {
              telemetry_log(
                "warn",
                "agent_run.visible_poll_failed",
                list("error.type" = class(error)[[1L]])
              )
              NULL
            }
          )
          if (is.null(current)) {
            schedule_visible_agent_run_poll(run_id, delay = 0.25)
            return(NULL)
          }
          active_agent_run(current)
          if (current$status %in% terminal_agent_run_statuses) {
            if (
              identical(current$status, "completed") &&
                completed_response_may_arrive(current)
            ) {
              schedule_visible_agent_run_poll(run_id, delay = 0.25)
              return(NULL)
            }
            if (identical(draining_agent_run_id(), run_id)) {
              draining_agent_run_id(NULL)
            }
            if (
              identical(current$status, "completed") &&
                nzchar(current$response_text %||% "")
            ) {
              append_reader_chat(current$response_text, session)
            }
            return(NULL)
          }
          schedule_visible_agent_run_poll(run_id, delay = 0.25)
          NULL
        },
        delay = delay
      )
      invisible(NULL)
    }

    if (
      !is.null(existing_agent_run) &&
        (!existing_agent_run$status %in% terminal_agent_run_statuses ||
          identical(existing_agent_run$status, "completed"))
    ) {
      schedule_visible_agent_run_poll(existing_agent_run$run_id)
    }

    reader_response_in_flight <- function() {
      pending <- shiny::isolate(pending_reader_question())
      deferred <- tryCatch(
        store_get_deferred_reader_question(store, actor_id),
        error = \(error) NULL
      )
      run <- shiny::isolate(active_agent_run())
      !is.null(pending) ||
        !is.null(deferred) ||
        !is.null(shiny::isolate(draining_agent_run_id())) ||
        (!is.null(run) &&
          !run$status %in% terminal_agent_run_statuses)
    }

    update_visible_agent_run <- function(run) {
      if (session$isClosed()) {
        return(invisible(run))
      }
      visible <- active_agent_run()
      if (!is.null(visible) && identical(visible$run_id, run$run_id)) {
        active_agent_run(run)
      }
      invisible(run)
    }

    cancel_agent_run_deadline <- function(run_id) {
      canceller <- agent_run_deadlines[[run_id]]
      if (is.function(canceller)) {
        try(canceller(), silent = TRUE)
      }
      agent_run_deadlines[[run_id]] <- NULL
      invisible(NULL)
    }

    finish_agent_run <- function(reason, context) {
      run_id <- context$run_context$rill_agent_run_id %||% NULL
      if (is.null(run_id)) {
        telemetry_log(
          "warn",
          "agent_run.stop_uncorrelated",
          list("terminal.reason" = reason %||% "unknown")
        )
        return(NULL)
      }
      agent_run_stop_confirmations[[run_id]] <- list(
        reason = reason,
        context = context
      )
      if (
        !session$isClosed() &&
          identical(draining_agent_run_id(), run_id)
      ) {
        draining_agent_run_id(NULL)
      }
      usage <- context$usage %||% list()
      if (inherits(usage, "AgentUsage")) {
        usage <- unclass(usage)
      }
      run <- store_get_agent_run(store, actor_id, run_id)
      if (is.null(run)) {
        agent_run_stop_confirmations[[run_id]] <- NULL
        return(NULL)
      }
      terminal_intent <- agent_run_terminal_intents[[run_id]]
      if (run$status %in% terminal_agent_run_statuses) {
        cancel_agent_run_deadline(run_id)
        rill_unregister_question_interrupt(run_id)
        agent_run_terminal_intents[[run_id]] <- NULL
        if (
          identical(run$status, "failed") &&
            identical(run$terminal_reason, "wall_time_limit")
        ) {
          enriched <- store_enrich_timed_out_agent_run(
            store,
            reader_id = actor_id,
            run_id = run_id,
            worker_id = session_id,
            usage = usage,
            deputy_run_id = context$run_id %||% NULL
          )
          if (is.null(enriched)) {
            telemetry_log(
              "warn",
              "agent_run.stop_enrichment_rejected",
              list("terminal.reason" = run$terminal_reason)
            )
          } else {
            update_visible_agent_run(enriched)
          }
        } else {
          enriched <- store_enrich_terminal_agent_run(
            store,
            reader_id = actor_id,
            run_id = run_id,
            worker_id = session_id,
            usage = usage,
            deputy_run_id = context$run_id %||% NULL
          )
          if (!is.null(enriched)) {
            update_visible_agent_run(enriched)
          }
        }
        agent_run_stop_confirmations[[run_id]] <- NULL
        return(NULL)
      }

      reason <- as.character(reason %||% "unknown")[[1L]]
      if (identical(terminal_intent, "wall_time_limit")) {
        reason <- terminal_intent
      } else if (
        !is.null(terminal_intent) &&
          rill_question_cancellation_confirmed(reason, terminal_intent)
      ) {
        reason <- terminal_intent
      }
      if (
        identical(run$status, "cancelling") &&
          identical(reason, "complete")
      ) {
        reason <- terminal_intent %||% "reader_cancelled"
      }
      status <- if (identical(reason, "complete")) "completed" else "failed"
      if (reason %in% c("cancelled", "reader_cancelled")) {
        run <- store_request_agent_run_cancel(
          store,
          reader_id = actor_id,
          run_id = run$run_id
        ) %||%
          run
        update_visible_agent_run(run)
        status <- "cancelled"
      }

      finished <- store_finish_agent_run(
        store,
        reader_id = actor_id,
        run_id = run$run_id,
        worker_id = session_id,
        status = status,
        usage = usage,
        terminal_reason = reason,
        deputy_run_id = context$run_id %||% NULL
      )
      if (!is.null(finished)) {
        cancel_agent_run_deadline(run_id)
        rill_unregister_question_interrupt(run_id)
        agent_run_terminal_intents[[run_id]] <- NULL
        agent_run_stop_confirmations[[run_id]] <- NULL
        update_visible_agent_run(finished)
      }
      NULL
    }

    fail_agent_run <- function(run, reason) {
      failed <- store_finish_agent_run(
        store,
        reader_id = actor_id,
        run_id = run$run_id,
        worker_id = session_id,
        status = "failed",
        terminal_reason = reason
      )
      if (!is.null(failed)) {
        cancel_agent_run_deadline(run$run_id)
        rill_unregister_question_interrupt(run$run_id)
        update_visible_agent_run(failed)
      }
      invisible(failed)
    }

    fail_unstarted_agent_run <- function(run_id, phase, reason) {
      failed <- tryCatch(
        store_fail_unstarted_agent_run(
          store,
          reader_id = actor_id,
          run_id = run_id,
          worker_id = session_id,
          phase = phase,
          terminal_reason = reason
        ),
        error = function(error) {
          telemetry_log(
            "warn",
            "agent_run.claim_cleanup_failed",
            list("error.type" = class(error)[[1]])
          )
          NULL
        }
      )
      if (!is.null(failed)) {
        active_agent_run(failed)
      }
      invisible(failed)
    }

    schedule_agent_run_deadline <- function(run, agent, deadline) {
      current_read <- tryCatch(
        list(value = store_get_agent_run(store, actor_id, run$run_id)),
        error = function(error) {
          telemetry_log(
            "warn",
            "agent_run.deadline_read_failed",
            list("error.type" = class(error)[[1L]])
          )
          NULL
        }
      )
      if (is.null(current_read)) {
        retry_cancel <- NULL
        retry_cancel <- later::later(
          function() {
            if (
              !identical(
                agent_run_deadlines[[run$run_id]],
                retry_cancel
              )
            ) {
              return(NULL)
            }
            agent_run_deadlines[[run$run_id]] <- NULL
            schedule_agent_run_deadline(run, agent, deadline)
            NULL
          },
          delay = 0.25
        )
        agent_run_deadlines[[run$run_id]] <- retry_cancel
        return(invisible(deadline))
      }
      current <- current_read$value
      if (
        is.null(current) ||
          current$status %in% terminal_agent_run_statuses
      ) {
        return(invisible(deadline))
      }

      interrupt_attempts <- 0L
      interrupt_outcome <- NULL
      interrupt_started_at <- as.POSIXct(NA, tz = "UTC")
      drain_cancel <- NULL
      state_poll_cancel <- NULL
      cancel_drain <- function() {
        if (is.function(drain_cancel)) {
          try(drain_cancel(), silent = TRUE)
          drain_cancel <<- NULL
        }
        invisible(NULL)
      }
      cancel_state_poll <- function() {
        if (is.function(state_poll_cancel)) {
          try(state_poll_cancel(), silent = TRUE)
          state_poll_cancel <<- NULL
        }
        invisible(NULL)
      }
      clear_run_control <- function() {
        cancel_agent_run_deadline(run$run_id)
        cancel_state_poll()
        rill_unregister_question_interrupt(run$run_id)
        agent_run_terminal_intents[[run$run_id]] <- NULL
        agent_run_stop_confirmations[[run$run_id]] <- NULL
        if (!session$isClosed()) {
          draining_agent_run_id(NULL)
        }
        invisible(NULL)
      }
      terminalize_inactive <- function(intent) {
        current <- store_get_agent_run(store, actor_id, run$run_id)
        if (
          is.null(current) ||
            current$status %in% terminal_agent_run_statuses
        ) {
          clear_run_control()
          return(invisible(current))
        }
        if (identical(current$status, "running")) {
          current <- store_request_agent_run_cancel(
            store,
            reader_id = actor_id,
            run_id = run$run_id
          ) %||%
            current
        }
        status <- if (identical(intent, "wall_time_limit")) {
          "failed"
        } else {
          "cancelled"
        }
        terminal <- store_finish_agent_run(
          store,
          reader_id = actor_id,
          run_id = run$run_id,
          worker_id = current$worker_id,
          status = status,
          terminal_reason = intent
        )
        if (!is.null(terminal)) {
          update_visible_agent_run(terminal)
          clear_run_control()
        }
        invisible(terminal)
      }
      interrupt_unconfirmed <- function(intent) {
        current <- store_get_agent_run(store, actor_id, run$run_id)
        if (
          is.null(current) ||
            current$status %in% terminal_agent_run_statuses
        ) {
          clear_run_control()
          return(invisible(current))
        }
        interrupted <- store_interrupt_agent_run(
          store,
          reader_id = actor_id,
          run_id = run$run_id,
          worker_id = current$worker_id,
          terminal_reason = paste0("interrupt_unconfirmed:", intent)
        )
        if (!is.null(interrupted)) {
          update_visible_agent_run(interrupted)
          clear_run_control()
        }
        invisible(interrupted)
      }
      schedule_drain_heartbeat <- NULL
      schedule_state_poll <- NULL
      request_interrupt <- NULL
      schedule_drain_heartbeat <- function(delay = 0.25) {
        if (is.function(drain_cancel)) {
          return(invisible(NULL))
        }
        drain_cancel <<- later::later(
          function() {
            drain_cancel <<- NULL
            confirmation <- agent_run_stop_confirmations[[run$run_id]]
            if (!is.null(confirmation)) {
              try(
                finish_agent_run(
                  confirmation$reason,
                  confirmation$context
                ),
                silent = TRUE
              )
            }
            current <- tryCatch(
              store_get_agent_run(store, actor_id, run$run_id),
              error = function(error) {
                telemetry_log(
                  "warn",
                  "agent_run.drain_read_failed",
                  list("error.type" = class(error)[[1]])
                )
                NULL
              }
            )
            if (
              !is.null(current) &&
                current$status %in% terminal_agent_run_statuses
            ) {
              if (!session$isClosed()) {
                draining_agent_run_id(NULL)
              }
              clear_run_control()
              return(NULL)
            }
            if (!is.null(current)) {
              if (identical(current$status, "running")) {
                current <- tryCatch(
                  store_request_agent_run_cancel(
                    store,
                    reader_id = actor_id,
                    run_id = run$run_id
                  ),
                  error = function(error) {
                    telemetry_log(
                      "warn",
                      "agent_run.cancel_write_failed",
                      list("error.type" = class(error)[[1]])
                    )
                    NULL
                  }
                ) %||%
                  current
              }
              renewed <- tryCatch(
                store_renew_agent_run_lease(
                  store,
                  reader_id = actor_id,
                  run_id = run$run_id,
                  worker_id = current$worker_id,
                  lease_expires_at = Sys.time() + 30
                ),
                error = function(error) {
                  telemetry_log(
                    "warn",
                    "agent_run.drain_lease_failed",
                    list("error.type" = class(error)[[1]])
                  )
                  NULL
                }
              )
              update_visible_agent_run(renewed %||% current)
            }
            intent <- agent_run_terminal_intents[[run$run_id]] %||%
              "interrupted"
            if (identical(interrupt_outcome, TRUE)) {
              confirmation_seconds <- agent_run_interrupt_confirmation_seconds()
              elapsed <- as.numeric(difftime(
                Sys.time(),
                interrupt_started_at,
                units = "secs"
              ))
              if (elapsed >= confirmation_seconds) {
                interrupt_unconfirmed(intent)
              } else {
                schedule_drain_heartbeat(
                  delay = min(5, confirmation_seconds - elapsed)
                )
              }
            } else {
              request_interrupt(intent)
            }
            NULL
          },
          delay = delay
        )
        invisible(NULL)
      }
      handle_interrupt_result <- function(reason, result) {
        intent <- agent_run_terminal_intents[[run$run_id]] %||% reason
        if (is.null(agent_run_terminal_intents[[run$run_id]])) {
          agent_run_terminal_intents[[run$run_id]] <- intent
        }
        if (is.na(interrupt_started_at)) {
          interrupt_started_at <<- Sys.time()
        }
        interrupt_attempts <<- interrupt_attempts + 1L
        interrupt_outcome <<- result
        if (identical(result, FALSE)) {
          terminalize_inactive(intent)
          return(invisible(NULL))
        }
        if (
          !identical(result, TRUE) &&
            interrupt_attempts >= agent_run_interrupt_retry_limit()
        ) {
          interrupt_unconfirmed(intent)
          return(invisible(NULL))
        }
        delay <- if (identical(result, TRUE)) {
          min(0.25, agent_run_interrupt_confirmation_seconds())
        } else {
          agent_run_interrupt_retry_delay(interrupt_attempts)
        }
        schedule_drain_heartbeat(delay = delay)
        invisible(NULL)
      }
      rill_register_question_drain(
        run$run_id,
        on_result = handle_interrupt_result,
        cancel = cancel_drain
      )
      request_interrupt <- function(reason) {
        if (!session$isClosed()) {
          draining_agent_run_id(run$run_id)
        }
        rill_signal_question_interrupt(
          run$run_id,
          reason,
          fallback = \(reason) agent$interrupt(reason)
        )
      }
      schedule_state_poll <- function(delay = 0.25) {
        if (is.function(state_poll_cancel)) {
          return(invisible(NULL))
        }
        state_poll_cancel <<- later::later(
          function() {
            state_poll_cancel <<- NULL
            current <- tryCatch(
              store_get_agent_run(store, actor_id, run$run_id),
              error = function(error) {
                telemetry_log(
                  "warn",
                  "agent_run.state_poll_failed",
                  list("error.type" = class(error)[[1L]])
                )
                NULL
              }
            )
            if (is.null(current)) {
              schedule_state_poll()
              return(NULL)
            }
            if (current$status %in% terminal_agent_run_statuses) {
              clear_run_control()
              return(NULL)
            }
            if (identical(current$status, "cancelling")) {
              if (is.null(agent_run_terminal_intents[[run$run_id]])) {
                agent_run_terminal_intents[[run$run_id]] <-
                  "reader_cancelled"
                update_visible_agent_run(current)
                request_interrupt("reader_cancelled")
              }
              return(NULL)
            }
            schedule_state_poll()
            NULL
          },
          delay = delay
        )
        invisible(NULL)
      }
      schedule_state_poll()

      delay <- max(
        0,
        as.numeric(difftime(deadline, Sys.time(), units = "secs"))
      )
      agent_run_deadlines[[run$run_id]] <- later::later(
        function() {
          agent_run_deadlines[[run$run_id]] <- NULL
          terminal_intent <- agent_run_terminal_intents[[run$run_id]]
          if (!is.null(terminal_intent)) {
            if (is.null(interrupt_outcome)) {
              request_interrupt(terminal_intent)
            } else {
              schedule_drain_heartbeat(delay = 0)
            }
            return(NULL)
          }
          agent_run_terminal_intents[[run$run_id]] <- "wall_time_limit"
          current <- tryCatch(
            store_request_agent_run_cancel(
              store,
              reader_id = actor_id,
              run_id = run$run_id
            ),
            error = function(error) {
              telemetry_log(
                "warn",
                "agent_run.deadline_write_failed",
                list("error.type" = class(error)[[1]])
              )
              NULL
            }
          )
          if (!is.null(current)) {
            update_visible_agent_run(current)
          }
          request_interrupt("wall_time_limit")
          NULL
        },
        delay = delay
      )
      invisible(deadline)
    }

    record_agent_run_partials <- function(run, deadline) {
      last_saved_at <- as.POSIXct(NA, tz = "UTC")
      function(partial) {
        now <- Sys.time()
        if (
          nzchar(partial) &&
            !is.na(last_saved_at) &&
            as.numeric(difftime(now, last_saved_at, units = "secs")) < 1
        ) {
          return(invisible(NULL))
        }
        updated <- tryCatch(
          store_record_agent_run_partial(
            store,
            reader_id = actor_id,
            run_id = run$run_id,
            worker_id = session_id,
            partial_response = partial,
            updated_at = now,
            lease_expires_at = deadline
          ),
          error = function(error) {
            telemetry_log(
              "warn",
              "agent_run.partial_write_failed",
              list("error.type" = class(error)[[1]])
            )
            NULL
          }
        )
        if (!is.null(updated)) {
          last_saved_at <<- now
          update_visible_agent_run(updated)
        }
        invisible(updated)
      }
    }

    record_agent_run_response <- function(run) {
      function(response) {
        updated <- tryCatch(
          store_record_agent_run_response(
            store,
            reader_id = actor_id,
            run_id = run$run_id,
            worker_id = session_id,
            response_text = response
          ),
          error = function(error) {
            telemetry_log(
              "warn",
              "agent_run.response_write_failed",
              list("error.type" = class(error)[[1L]])
            )
            NULL
          }
        )
        if (!is.null(updated)) {
          update_visible_agent_run(updated)
        }
        invisible(updated)
      }
    }

    reader_agent_for <- function(document) {
      agent <- reader_agent()
      if (
        is.null(agent) ||
          !identical(reader_agent_document_id(), document$document_id)
      ) {
        agent <- rill_reader_agent(
          document = document,
          reader_id = actor_id,
          session_id = session_id,
          model = config$agent_model,
          base_url = config$agent_base_url %||% "",
          on_stop = finish_agent_run
        )
        reader_agent(agent)
        reader_agent_document_id(document$document_id)
      }
      agent
    }

    next_agent_request_key <- function(
      prefix = "ask-rill",
      request_token = NULL
    ) {
      if (!is.null(request_token) && length(request_token)) {
        return(rill_id(prefix, session_id, as.character(request_token)[[1]]))
      }
      index <- agent_request_index() + 1L
      agent_request_index(index)
      rill_id(prefix, session_id, index)
    }

    run_reader_question <- function(
      question,
      document,
      retry_of = NULL,
      request_token = NULL,
      request_key = NULL,
      pinned_inputs = NULL,
      requested_at = NULL,
      transition_at = NULL
    ) {
      if (!is.null(draining_agent_run_id())) {
        cli::cli_abort(
          "The previous response is still stopping. Try again in a moment.",
          class = "rill_agent_run_draining"
        )
      }
      request_key <- request_key %||%
        next_agent_request_key(
          if (is.null(retry_of)) "ask-rill" else "ask-rill-retry",
          request_token = request_token
        )
      run_id <- rill_id("agent-run", actor_id, request_key)
      agent <- tryCatch(
        reader_agent_for(document),
        error = \(error) error
      )
      configured_destination <- rill_agent_data_destination_details(
        config$agent_model,
        base_url = config$agent_base_url %||% ""
      )
      runtime_identity <- if (inherits(agent, "error")) {
        list(
          model = config$agent_model,
          data_destination = configured_destination$label,
          data_destination_id = configured_destination$id
        )
      } else {
        rill_agent_runtime_identity(
          agent,
          config$agent_model,
          configured_destination = configured_destination$label,
          configured_destination_id = configured_destination$id
        )
      }
      preserved_inputs <- pinned_inputs %||% retry_of$pinned_inputs %||% NULL
      identity_error <- tryCatch(
        {
          rill_assert_question_runtime_identity(
            preserved_inputs,
            runtime_identity
          )
          NULL
        },
        error = \(error) error
      )
      if (inherits(identity_error, "error")) {
        store_delete_deferred_reader_question(store, actor_id, request_key)
        stop(identity_error)
      }
      requested_at <- requested_at %||% Sys.time()
      pinned_inputs <- pinned_inputs %||%
        if (is.null(retry_of)) {
          list(
            submission_id = request_key,
            entry_id = document$entry_id,
            document_id = document$document_id,
            document_content_hash = document$content_hash,
            document_record_hash = document$record_hash,
            orientation_selection = selected_orientation_provenance(),
            research_scope = list(
              kind = "selected_document",
              document_ids = document$document_id
            ),
            data_destination = runtime_identity$data_destination,
            data_destination_id = runtime_identity$data_destination_id,
            question = question,
            model = runtime_identity$model,
            policy_version = "ask-rill-v1",
            limits = rill_agent_run_limits()
          )
        } else {
          NULL
        }
      run <- tryCatch(
        store_start_prioritized_reader_question(
          store,
          reader_id = actor_id,
          request_key = request_key,
          pinned_inputs = pinned_inputs,
          retry_of = retry_of,
          requested_at = requested_at,
          worker_id = session_id,
          transition_at = transition_at %||% requested_at
        ),
        error = function(error) {
          fail_unstarted_agent_run(
            run_id,
            "start",
            paste0("start_error:", class(error)[[1]])
          )
          stop(error)
        }
      )
      if (!is.null(run$preempted)) {
        control <- orientation_control()
        if (
          !isTRUE(run$orientation_signalled) &&
            !is.null(control) &&
            identical(control$status, "running")
        ) {
          signal_interrupt <- control$signal_interrupt %||% control$interrupt
          try(signal_interrupt("reader_question"), silent = TRUE)
        }
        bump_refresh()
      }
      preempted <- run$preempted
      run <- run$run

      if (is.null(run)) {
        return(invisible(list(
          waiting = TRUE,
          preempted = preempted,
          question = question,
          document = document,
          retry_of = retry_of,
          request_token = request_token,
          request_key = request_key,
          pinned_inputs = pinned_inputs,
          requested_at = requested_at
        )))
      }
      if (!identical(run$status, "pending")) {
        active_agent_run(run)
        if (!run$status %in% terminal_agent_run_statuses) {
          schedule_visible_agent_run_poll(run$run_id)
        }
        return(invisible(run))
      }

      started_at <- Sys.time()
      deadline <- started_at + rill_agent_wall_time_seconds()
      run <- tryCatch(
        store_claim_agent_run(
          store,
          reader_id = actor_id,
          run_id = run$run_id,
          worker_id = session_id,
          started_at = started_at,
          lease_expires_at = deadline
        ),
        error = function(error) {
          fail_unstarted_agent_run(
            run_id,
            "claim",
            paste0("claim_error:", class(error)[[1]])
          )
          stop(error)
        }
      )
      if (is.null(run)) {
        fail_unstarted_agent_run(run_id, "claim", "claim_failed")
        cli::cli_abort(
          "The Agent Run could not be claimed.",
          class = "rill_agent_run_claim_failed"
        )
      }
      active_agent_run(run)
      if (!inherits(agent, "error")) {
        rill_register_question_interrupt(
          run$run_id,
          \(reason) agent$interrupt(reason)
        )
      }

      result <- if (inherits(agent, "error")) {
        agent
      } else {
        tryCatch(
          {
            response <- rill_agent_shiny_stream(
              agent,
              question,
              run_context = list(rill_agent_run_id = run$run_id)
            )
            response <- track_reader_agent_stream(
              response,
              record_agent_run_partials(run, deadline),
              on_complete = record_agent_run_response(run)
            )
            schedule_agent_run_deadline(run, agent, deadline)
            appended <- append_reader_chat(response, session)
            promises::then(appended, function(value) {
              if (is.character(value) && length(value) == 1L) {
                record_agent_run_response(run)(value)
              }
              value
            })
          },
          error = \(error) error
        )
      }
      if (inherits(result, "error")) {
        fail_agent_run(
          run,
          paste0("setup_error:", class(result)[[1]])
        )
        shiny::showNotification(
          conditionMessage(result),
          type = "error",
          duration = 8
        )
        append_reader_chat(
          "Rill couldn't start that response. You can retry from this panel.",
          session
        )
      }
      invisible(result)
    }

    run_prioritized_reader_question <- function(
      question,
      document,
      retry_of = NULL,
      request_token = NULL,
      request_key = NULL,
      pinned_inputs = NULL,
      requested_at = NULL,
      transition_at = NULL
    ) {
      request_key <- request_key %||%
        next_agent_request_key(
          if (is.null(retry_of)) "ask-rill" else "ask-rill-retry",
          request_token = request_token
        )
      pending <- shiny::isolate(pending_reader_question())
      if (!is.null(pending)) {
        if (identical(pending$request_key, request_key)) {
          return(invisible(pending))
        }
        cli::cli_abort(
          "Rill is still stopping Orientation before answering.",
          class = "rill_agent_run_draining"
        )
      }

      result <- run_reader_question(
        question,
        document,
        retry_of = retry_of,
        request_token = request_token,
        request_key = request_key,
        pinned_inputs = pinned_inputs,
        requested_at = requested_at,
        transition_at = transition_at
      )
      if (!is.list(result) || !isTRUE(result$waiting)) {
        return(invisible(result))
      }

      pending_reader_question(result)
      poll <- NULL
      poll <- function() {
        pending_reader_question_cancel <<- later::later(
          function() {
            pending_reader_question_cancel <<- NULL
            pending <- shiny::isolate(pending_reader_question())
            if (is.null(pending)) {
              return(NULL)
            }
            durable_pending_read <- tryCatch(
              list(value = store_get_deferred_reader_question(store, actor_id)),
              error = function(error) {
                telemetry_log(
                  "warn",
                  "agent_run.deferred_poll_failed",
                  list(
                    "read.target" = "deferred_question",
                    "error.type" = class(error)[[1L]]
                  )
                )
                NULL
              }
            )
            if (is.null(durable_pending_read)) {
              poll()
              return(NULL)
            }
            durable_pending <- durable_pending_read$value
            if (is.null(durable_pending)) {
              adopted_read <- tryCatch(
                list(
                  value = store_get_agent_run_by_request_key(
                    store,
                    actor_id,
                    pending$request_key
                  )
                ),
                error = function(error) {
                  telemetry_log(
                    "warn",
                    "agent_run.deferred_poll_failed",
                    list(
                      "read.target" = "question_agent_run",
                      "error.type" = class(error)[[1L]]
                    )
                  )
                  NULL
                }
              )
              if (is.null(adopted_read)) {
                poll()
                return(NULL)
              }
              pending_reader_question(NULL)
              adopted <- adopted_read$value
              if (!is.null(adopted)) {
                active_agent_run(adopted)
                if (
                  !adopted$status %in% terminal_agent_run_statuses ||
                    identical(adopted$status, "completed")
                ) {
                  schedule_visible_agent_run_poll(adopted$run_id)
                }
              }
              return(NULL)
            }
            current_read <- tryCatch(
              list(
                value = store_get_agent_run(
                  store,
                  actor_id,
                  pending$preempted$run_id
                )
              ),
              error = function(error) {
                telemetry_log(
                  "warn",
                  "agent_run.deferred_poll_failed",
                  list(
                    "read.target" = "preempted_agent_run",
                    "error.type" = class(error)[[1L]]
                  )
                )
                NULL
              }
            )
            if (is.null(current_read)) {
              poll()
              return(NULL)
            }
            current <- current_read$value
            if (
              !is.null(current) &&
                !current$status %in% terminal_agent_run_statuses
            ) {
              poll()
              return(NULL)
            }

            pending_reader_question(NULL)
            orientation_attempted_boundary(NULL)
            bump_refresh()
            restarted <- tryCatch(
              run_reader_question(
                pending$question,
                pending$document,
                retry_of = pending$retry_of,
                request_token = pending$request_token,
                request_key = pending$request_key,
                pinned_inputs = pending$pinned_inputs,
                requested_at = pending$requested_at,
                transition_at = Sys.time()
              ),
              error = function(error) {
                error
              }
            )
            if (inherits(restarted, "error")) {
              if (!session$isClosed()) {
                shiny::showNotification(
                  conditionMessage(restarted),
                  type = "error",
                  duration = 8
                )
                append_reader_chat(
                  paste(
                    "Rill couldn't start that response after Orientation",
                    "stopped."
                  ),
                  session
                )
              }
            } else if (is.list(restarted) && isTRUE(restarted$waiting)) {
              pending_reader_question(restarted)
              poll()
            }
            NULL
          },
          delay = 0.05
        )
        invisible(NULL)
      }
      poll()
      invisible(result)
    }

    read_deferred_resume_value <- function(target, read) {
      tryCatch(
        list(value = read()),
        error = function(error) {
          telemetry_log(
            "warn",
            "agent_run.deferred_resume_failed",
            list(
              "read.target" = target,
              "error.type" = class(error)[[1L]]
            )
          )
          NULL
        }
      )
    }
    resume_deferred_reader_question <- NULL
    schedule_deferred_reader_question_resume <- function(delay = 0.25) {
      if (is.function(deferred_reader_question_resume_cancel)) {
        return(invisible(NULL))
      }
      deferred_reader_question_resume_cancel <<- later::later(
        function() {
          deferred_reader_question_resume_cancel <<- NULL
          if (!session$isClosed()) {
            resume_deferred_reader_question()
          }
          NULL
        },
        delay = delay
      )
      invisible(NULL)
    }
    resume_deferred_reader_question <- function() {
      deferred_read <- read_deferred_resume_value(
        "deferred_question",
        \() store_get_deferred_reader_question(store, actor_id)
      )
      if (is.null(deferred_read)) {
        schedule_deferred_reader_question_resume()
        return(invisible(NULL))
      }
      deferred <- deferred_read$value
      if (is.null(deferred)) {
        return(invisible(NULL))
      }
      document_read <- read_deferred_resume_value(
        "document",
        \() {
          store_get_document_by_id(
            store,
            actor_id,
            deferred$pinned_inputs$document_id
          )
        }
      )
      if (is.null(document_read)) {
        schedule_deferred_reader_question_resume()
        return(invisible(NULL))
      }
      document <- document_read$value
      if (is.null(document)) {
        telemetry_log(
          "error",
          "agent_run.deferred_document_missing",
          list("agent.request_key" = deferred$request_key)
        )
        return(invisible(NULL))
      }
      retry_of_read <- if (is.null(deferred$retry_of_run_id)) {
        list(value = NULL)
      } else {
        read_deferred_resume_value(
          "retry_agent_run",
          \() {
            store_get_agent_run(
              store,
              actor_id,
              deferred$retry_of_run_id
            )
          }
        )
      }
      if (is.null(retry_of_read)) {
        schedule_deferred_reader_question_resume()
        return(invisible(NULL))
      }
      retry_of <- retry_of_read$value
      selected_id(document$entry_id)
      selected_document_id(document$document_id)
      resumed <- tryCatch(
        run_prioritized_reader_question(
          deferred$pinned_inputs$question,
          document,
          retry_of = retry_of,
          request_key = deferred$request_key,
          pinned_inputs = deferred$pinned_inputs,
          requested_at = deferred$requested_at,
          transition_at = Sys.time()
        ),
        error = \(error) error
      )
      if (inherits(resumed, "error") && !session$isClosed()) {
        shiny::showNotification(
          conditionMessage(resumed),
          type = "error",
          duration = 8
        )
        append_reader_chat(
          paste(
            "Rill didn't send the preserved question because its configured",
            "model destination changed. Ask it again to confirm the current",
            "destination."
          ),
          session
        )
      }
      invisible(resumed)
    }

    session$onFlushed(resume_deferred_reader_question, once = TRUE)

    feeds <- shiny::reactive({
      refresh_tick()
      store_list_feeds(store, actor_id)
    })

    queue_entries <- shiny::reactive({
      refresh_tick()
      view <- input$view %||% "unread"
      if (view %in% c("today", "week", "month")) {
        shiny::invalidateLater(60 * 1000, session)
      }
      store_list_entries(
        store,
        actor_id,
        view = view,
        feed_id = selected_feed(),
        limit = 150L,
        sort = input$story_sort %||% "newest"
      )
    })

    orientation_destination_status <- shiny::reactive({
      orientation_destination_tick()
      state <- orientation_destination_state(store, actor_id, config)
      orientation_destination_poll_token <<-
        orientation_destination_state_token(state)
      state
    })

    focus_orientation_destination <- function() {
      session$onFlushed(
        \() {
          session$sendCustomMessage(
            "rill-focus-orientation-destination",
            list()
          )
        },
        once = TRUE
      )
    }

    cancel_active_orientation <- function(reason) {
      active <- store_get_active_agent_run(store, actor_id)
      if (is.null(active) || !identical(active$kind, "orientation")) {
        return(invisible(NULL))
      }
      cancelling <- store_request_agent_run_cancel(
        store,
        reader_id = actor_id,
        run_id = active$run_id
      )
      signalled <- if (
        !is.null(cancelling) &&
          identical(cancelling$status, "cancelling")
      ) {
        rill_signal_orientation_interrupt(cancelling$run_id, reason)
      } else {
        FALSE
      }
      control <- shiny::isolate(orientation_control())
      if (
        !isTRUE(signalled) &&
          !is.null(control) &&
          identical(control$run$run_id %||% NULL, active$run_id)
      ) {
        try(control$interrupt(reason), silent = TRUE)
      }
      invisible(cancelling)
    }

    shiny::observe({
      shiny::invalidateLater(1000, session)
      state <- tryCatch(
        orientation_destination_state(store, actor_id, config),
        error = \(error) NULL
      )
      if (is.null(state)) {
        return()
      }
      token <- orientation_destination_state_token(state)
      previous <- orientation_destination_poll_token
      orientation_destination_poll_token <<- token
      if (!is.null(previous) && !identical(token, previous)) {
        if (!isTRUE(state$enabled)) {
          cancel_active_orientation("orientation_disabled")
        }
        orientation_destination_tick(
          shiny::isolate(orientation_destination_tick()) + 1L
        )
        bump_refresh()
      }
    })

    orientation_state <- shiny::reactive({
      refresh_tick()
      state <- orientation_status(store, actor_id)
      orientation_poll_token <<- orientation_state_token(state)
      state
    })

    shiny::observe({
      shiny::invalidateLater(1000, session)
      state <- tryCatch(
        orientation_status(store, actor_id),
        error = \(error) NULL
      )
      if (is.null(state)) {
        return()
      }
      token <- orientation_state_token(state)
      previous <- orientation_poll_token
      orientation_poll_token <<- token
      if (!is.null(previous) && !identical(token, previous)) {
        bump_refresh()
      }
    })

    start_orientation_maintenance <- function() {
      destination <- tryCatch(
        orientation_destination_state(store, actor_id, config),
        error = \(error) NULL
      )
      if (is.null(destination) || !isTRUE(destination$enabled)) {
        return(invisible(NULL))
      }
      current_control <- shiny::isolate(orientation_control())
      if (
        !is.null(current_control) &&
          identical(current_control$status, "running")
      ) {
        return(invisible(current_control))
      }
      prepared <- tryCatch(
        prepare_orientation_documents(store, actor_id),
        error = \(error) error
      )
      if (inherits(prepared, "error")) {
        telemetry_log(
          "warn",
          "orientation.preparation_failed",
          list("error.type" = class(prepared)[[1L]])
        )
        return(invisible(NULL))
      }
      state <- orientation_status(store, actor_id)
      boundary_hash <- state$boundary$hash
      if (
        !isTRUE(state$due) ||
          identical(orientation_attempted_boundary(), boundary_hash)
      ) {
        return(invisible(NULL))
      }

      orientation_attempted_boundary(boundary_hash)
      orientation_preparing(TRUE)
      control <- tryCatch(
        maintain_orientation_async(
          store = store,
          reader_id = actor_id,
          worker_id = session_id,
          model = config$agent_model,
          base_url = config$agent_base_url %||% "",
          destination_check = \() {
            orientation_destination_state(store, actor_id, config)
          },
          retry_id = orientation_automatic_retry_id()
        ),
        error = \(error) error
      )
      if (inherits(control, "error")) {
        orientation_preparing(FALSE)
        telemetry_log(
          "warn",
          "orientation.start_failed",
          list("error.type" = class(control)[[1L]])
        )
        return(invisible(NULL))
      }
      control$session_token <- rill_id(
        "orientation-control",
        session_id,
        control$run$run_id %||% boundary_hash,
        stats::runif(1)
      )
      orientation_control(control)
      if (!identical(control$status, "running") || is.null(control$promise)) {
        orientation_preparing(FALSE)
        orientation_control(NULL)
        if (control$status %in% c("busy", "running")) {
          schedule_orientation_retry()
        } else if (identical(control$status, "boundary_changed")) {
          schedule_orientation_retry(delay = 0)
        }
        return(invisible(control))
      }

      settle <- function(value = NULL) {
        if (session$isClosed()) {
          return(value)
        }
        current_control <- shiny::isolate(orientation_control())
        if (
          is.null(current_control) ||
            !identical(
              current_control$session_token,
              control$session_token
            )
        ) {
          return(value)
        }
        orientation_preparing(FALSE)
        orientation_control(NULL)
        bump_refresh()
        value
      }
      promises::then(
        control$promise,
        onFulfilled = settle,
        onRejected = function(error) {
          telemetry_log(
            "warn",
            "orientation.maintenance_failed",
            list("error.type" = class(error)[[1L]])
          )
          settle(NULL)
        }
      )
      invisible(control)
    }

    if (!isTRUE(config$demo_mode)) {
      shiny::observe({
        destination <- orientation_destination_status()
        orientation_state()
        if (isTRUE(destination$enabled) && is.null(selected_id())) {
          start_orientation_maintenance()
        }
      })
    }

    shiny::observeEvent(
      input$orientation_enable,
      {
        state <- shiny::isolate(orientation_destination_status())
        if (!isTRUE(state$available)) {
          shiny::showNotification(
            "Automatic Orientation is unavailable in this installation.",
            type = "warning"
          )
          return()
        }
        if (isTRUE(state$needs_configuration)) {
          message <- if (isTRUE(state$needs_endpoint_configuration)) {
            paste(
              "Automatic Orientation needs an explicit model endpoint in",
              "RILL_AGENT_BASE_URL."
            )
          } else {
            paste(
              "Automatic Orientation needs an inspectable provider-policy",
              "link from this installation."
            )
          }
          shiny::showNotification(
            message,
            type = "warning"
          )
          return()
        }
        if (isTRUE(state$needs_confirmation)) {
          shiny::showModal(orientation_destination_confirmation_ui(state))
          return()
        }
        set_orientation_enabled(
          store,
          actor_id,
          enabled = TRUE,
          config = config
        )
        orientation_destination_tick(orientation_destination_tick() + 1L)
        bump_refresh()
        focus_orientation_destination()
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$orientation_confirm,
      {
        confirmed <- tryCatch(
          confirm_orientation_destination(store, actor_id, config),
          error = function(error) {
            shiny::showNotification(
              conditionMessage(error),
              type = "error",
              duration = 8
            )
            NULL
          }
        )
        if (is.null(confirmed)) {
          return()
        }
        shiny::removeModal()
        orientation_destination_tick(orientation_destination_tick() + 1L)
        orientation_attempted_boundary(NULL)
        bump_refresh()
        focus_orientation_destination()
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$orientation_disable,
      {
        set_orientation_enabled(
          store,
          actor_id,
          enabled = FALSE,
          config = config
        )
        orientation_destination_tick(orientation_destination_tick() + 1L)
        cancel_active_orientation("orientation_disabled")
        bump_refresh()
        focus_orientation_destination()
      },
      ignoreInit = TRUE
    )

    entries <- shiny::reactive({
      rows <- queue_entries()
      ids <- retained_ids()
      if (
        !length(ids) ||
          !identical(retained_context(), current_context())
      ) {
        return(rows)
      }

      all_rows <- store_list_entries(
        store,
        actor_id,
        view = "all",
        feed_id = selected_feed(),
        limit = 500L,
        sort = input$story_sort %||% "newest"
      )
      visible_ids <- unique(c(as.character(rows$entry_id), ids))
      all_rows[all_rows$entry_id %in% visible_ids, , drop = FALSE]
    })

    selected_feed_title <- shiny::reactive({
      feed_id <- selected_feed()
      if (is.null(feed_id)) {
        return(NULL)
      }
      feed_rows <- feeds()
      title <- feed_rows$title[feed_rows$feed_id == feed_id]
      if (length(title)) title[[1]] else NULL
    })

    selected_entry <- shiny::reactive({
      entry_id <- selected_id()
      shiny::req(entry_id)
      refresh_tick()
      entry <- store_get_entry(store, actor_id, entry_id)
      if (!is.null(entry)) {
        entry$library_access <- TRUE
        return(entry)
      }
      if (!is.null(selected_document_id())) {
        entry <- store_get_entry_for_document_pin(
          store,
          actor_id,
          selected_document_id()
        )
      }
      if (!is.null(entry)) {
        entry$library_access <- FALSE
      }
      entry
    })

    selected_document <- shiny::reactive({
      entry <- selected_entry()
      document_id <- selected_document_id()
      if (!is.null(document_id)) {
        document <- store_get_document_by_id(store, actor_id, document_id)
        if (
          is.null(document) ||
            !identical(document$entry_id, entry$entry_id)
        ) {
          cli::cli_abort(
            "The Orientation reading copy is no longer available.",
            class = "rill_orientation_document_missing"
          )
        }
        return(document)
      }
      shiny::withProgress(
        message = "Preparing a clean reading copy",
        value = 0.5,
        {
          get_or_extract_document(store, actor_id, entry, config)
        }
      )
    })

    refresh_feeds_now <- function() {
      result <- shiny::withProgress(message = "Refreshing feeds", value = 0, {
        feed_rows <- store_list_feeds(
          store,
          actor_id,
          source_kind = "subscription"
        )
        if (!nrow(feed_rows)) {
          return(list())
        }
        output <- vector("list", nrow(feed_rows))
        for (index in seq_len(nrow(feed_rows))) {
          shiny::incProgress(
            1 / nrow(feed_rows),
            detail = feed_rows$title[[index]]
          )
          output[[index]] <- tryCatch(
            refresh_feed(store, as.list(feed_rows[index, , drop = FALSE])),
            error = function(error) list(error = conditionMessage(error))
          )
        }
        output
      })
      failures <- sum(vapply(
        result,
        function(item) !is.null(item$error),
        logical(1)
      ))
      status_text(
        if (failures) {
          paste0(
            failures,
            " feed",
            if (failures == 1L) "" else "s",
            " failed to refresh"
          )
        } else {
          "Feeds are up to date"
        }
      )
      status_kind(if (failures) "error" else "success")
      record_event(
        "feeds_refreshed",
        surface = "sidebar",
        payload = list(failures = failures)
      )
      bump_refresh()
      invisible(result)
    }

    output$feed_nav <- shiny::renderUI({
      feed_rows <- feeds()
      all_unread <- sum(feed_rows$unread_count, na.rm = TRUE)
      all_active <- is.null(selected_feed())
      links <- list(shiny::tags$button(
        type = "button",
        class = paste("feed-link", if (all_active) "is-active"),
        `aria-current` = if (all_active) "true" else NULL,
        onclick = "rillSelectFeed(null)",
        shiny::tags$span("All feeds"),
        shiny::tags$small(all_unread)
      ))

      if (nrow(feed_rows)) {
        links <- c(
          links,
          lapply(seq_len(nrow(feed_rows)), function(index) {
            feed <- feed_rows[index, , drop = FALSE]
            onclick <- sprintf(
              "rillSelectFeed(%s)",
              jsonlite::toJSON(as.character(feed$feed_id), auto_unbox = TRUE)
            )
            shiny::tags$button(
              type = "button",
              class = paste(
                "feed-link",
                if (identical(selected_feed(), as.character(feed$feed_id))) {
                  "is-active"
                }
              ),
              `aria-current` = if (
                identical(selected_feed(), as.character(feed$feed_id))
              ) {
                "true"
              } else {
                NULL
              },
              onclick = onclick,
              title = feed$title,
              shiny::tags$span(feed$title),
              shiny::tags$small(feed$unread_count)
            )
          })
        )
      }
      shiny::tagList(links)
    })

    output$feed_organization_control <- shiny::renderUI({
      feed_id <- selected_feed()
      if (is.null(feed_id)) {
        return(feed_organization_control_ui())
      }
      feed_rows <- feeds()
      selected <- feed_rows[feed_rows$feed_id == feed_id, , drop = FALSE]
      if (!nrow(selected)) {
        return(feed_organization_control_ui())
      }
      feed_organization_control_ui(as.list(selected[1, , drop = FALSE]))
    })

    output$orientation_destination_settings <- shiny::renderUI({
      orientation_destination_settings_ui(orientation_destination_status())
    })

    output$list_title <- shiny::renderUI({
      label <- switch(
        input$view %||% "unread",
        unread = "Unread",
        all = "All stories",
        starred = "Starred",
        saved = "Saved",
        today = "Today",
        week = "This week",
        month = "This month"
      )
      feed_title <- selected_feed_title()
      if (!is.null(feed_title)) {
        label <- paste(label, "\u00b7", feed_title)
      }
      shiny::tags$h1(label)
    })

    output$story_count <- shiny::renderUI({
      count <- nrow(queue_entries())
      noun <- if (count == 1L) "story" else "stories"
      shiny::tags$span(
        class = "count-pill",
        title = paste(count, noun),
        `aria-label` = paste(count, noun),
        count
      )
    })

    output$orientation_queue_status <- shiny::renderUI({
      if (!is.null(selected_id())) {
        return(NULL)
      }
      state <- orientation_state()
      orientation_queue_status_ui(
        state$orientation,
        state$candidates,
        preparing = orientation_preparing(),
        processing_note = orientation_processing_note(
          store,
          state$orientation,
          config,
          destination_state = orientation_destination_status(),
          preparing = orientation_preparing()
        )
      )
    })
    shiny::outputOptions(
      output,
      "orientation_queue_status",
      suspendWhenHidden = FALSE
    )

    output$prepare_today_control <- shiny::renderUI({
      if (!identical(input$view, "today")) {
        return(NULL)
      }
      prepare_today_button()
    })

    output$read_actions <- shiny::renderUI({
      read_actions_ui(selected_feed_title())
    })

    output$story_list <- shiny::renderUI({
      rows <- entries()
      if (!nrow(rows)) {
        return(empty_story_list(
          input$view %||% "unread",
          selected_feed_title()
        ))
      }
      shiny::tagList(lapply(seq_len(nrow(rows)), function(index) {
        story_card(
          as.list(rows[index, , drop = FALSE]),
          index,
          selected = identical(
            selected_id(),
            as.character(rows$entry_id[[index]])
          )
        )
      }))
    })

    output$reader_header <- shiny::renderUI({
      if (is.null(selected_id())) {
        state <- orientation_state()
        return(orientation_ui(
          state$orientation,
          state$candidates,
          preparing = orientation_preparing(),
          processing_note = orientation_processing_note(
            store,
            state$orientation,
            config,
            destination_state = orientation_destination_status(),
            preparing = orientation_preparing()
          )
        ))
      }

      reader_article_header_ui(selected_entry(), selected_document())
    })

    output$reader_body <- shiny::renderUI({
      if (is.null(selected_id())) {
        return(NULL)
      }
      reader_document_ui(
        selected_document(),
        selected_id(),
        if (is.null(selected_orientation_provenance())) {
          "story_list"
        } else {
          "orientation"
        }
      )
    })

    output$reader_agent_context <- shiny::renderUI({
      if (is.null(selected_id())) {
        return(NULL)
      }
      reader_agent_context_ui(selected_entry(), selected_document())
    })
    shiny::outputOptions(
      output,
      "reader_agent_context",
      suspendWhenHidden = FALSE
    )

    output$reader_agent_status <- shiny::renderUI({
      pending <- pending_reader_question()
      if (!is.null(pending)) {
        return(shiny::tags$p(
          class = "reader-agent-run-status",
          role = "status",
          `aria-live` = "polite",
          bsicons::bs_icon("hourglass-split"),
          "Stopping Orientation before answering\u2026"
        ))
      }
      run <- active_agent_run()
      if (is.null(run) || identical(run$status, "completed")) {
        return(NULL)
      }
      if (run$status %in% c("pending", "running")) {
        return(shiny::tags$p(
          class = "reader-agent-run-status",
          role = "status",
          `aria-live` = "polite",
          bsicons::bs_icon("stars"),
          "Reading the selected source\u2026"
        ))
      }
      if (identical(run$status, "cancelling")) {
        return(shiny::tags$p(
          class = "reader-agent-run-status",
          role = "status",
          `aria-live` = "polite",
          bsicons::bs_icon("stop-circle"),
          "Stopping the response\u2026"
        ))
      }

      shiny::tags$div(
        class = "reader-agent-retry",
        role = "alert",
        shiny::tags$p("That response stopped before it completed."),
        shiny::actionButton(
          "retry_agent_run",
          "Retry",
          class = "btn-sm"
        )
      )
    })

    output$sidebar_status <- shiny::renderUI({
      text <- status_text()
      if (is.null(text)) {
        return(NULL)
      }
      shiny::tags$p(
        class = paste("sidebar-status", paste0("is-", status_kind())),
        role = if (identical(status_kind(), "error")) "alert" else "status",
        `aria-live` = if (identical(status_kind(), "error")) {
          "assertive"
        } else {
          "polite"
        },
        `aria-atomic` = "true",
        text
      )
    })

    output$export_opml <- shiny::downloadHandler(
      filename = function() {
        paste0("rill-subscriptions-", format(Sys.Date(), "%Y-%m-%d"), ".opml")
      },
      content = function(file) {
        feed_rows <- store_list_feeds(
          store,
          actor_id,
          source_kind = "subscription"
        )
        write_opml(
          feed_rows,
          file,
          title = paste(config$app_name, "subscriptions")
        )
        record_event(
          "opml_exported",
          surface = "sidebar",
          payload = list(feeds = nrow(feed_rows))
        )
      },
      contentType = "text/x-opml"
    )

    shiny::observeEvent(
      input$view,
      {
        clear_selection()
        if (
          isTRUE(browse_queue_pending()) &&
            identical(input$view, "unread")
        ) {
          browse_queue_pending(FALSE)
          acknowledge_orientation_queue()
        }
      },
      ignoreInit = TRUE,
      priority = 100
    )

    shiny::observeEvent(
      input$story_sort,
      {
        clear_selection()
      },
      ignoreInit = TRUE,
      priority = 100
    )

    shiny::observeEvent(
      input$select_feed,
      {
        clear_selection()
        selected_feed(input$select_feed$id %||% NULL)
        record_event(
          "feed_filter",
          surface = "sidebar",
          payload = list(feed_id = selected_feed())
        )
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$browse_orientation_queue,
      {
        clear_selection()
        selected_feed(NULL)
        browse_queue_pending(!identical(input$view %||% "unread", "unread"))
        shiny::updateRadioButtons(session, "view", selected = "unread")
        bump_refresh()
        if (!isTRUE(browse_queue_pending())) {
          acknowledge_orientation_queue()
        }
      },
      ignoreInit = TRUE,
      priority = 100
    )

    shiny::observeEvent(
      input$select_entry,
      {
        request <- input$select_entry
        entry_id <- request$id %||% NULL
        if (is.null(entry_id)) {
          return()
        }
        previous_id <- selected_id()
        previous_document_id <- selected_document_id()
        surface <- request$surface %||% "story_list"
        if (!surface %in% c("story_list", "orientation")) {
          surface <- "story_list"
        }
        position <- as.integer(request$position %||% NA_integer_)
        pinned_document_id <- NULL
        provenance <- NULL
        if (identical(surface, "orientation")) {
          pinned_document_id <- request$document_id %||% NULL
        }
        if (
          (!identical(previous_id, entry_id) ||
            !identical(previous_document_id, pinned_document_id)) &&
            reader_response_in_flight()
        ) {
          shiny::showNotification(
            "Wait for the current response to stop before changing stories.",
            type = "warning"
          )
          if (identical(surface, "orientation")) {
            session$sendCustomMessage(
              "rill-orientation-action-rejected",
              list(action = "select")
            )
          }
          return()
        }
        if (identical(surface, "orientation")) {
          selected_at <- utc_now()
          selection <- tryCatch(
            store_select_orientation_card(
              store,
              reader_id = actor_id,
              orientation_id = request$orientation_id %||% NULL,
              revision_id = request$revision_id %||% NULL,
              card_id = request$card_id %||% NULL,
              entry_id = entry_id,
              document_id = request$document_id %||% NULL,
              basis_hash = request$basis_hash %||% NULL,
              rationale_hash = request$rationale_hash %||% NULL,
              event_id = rill_id(
                "event",
                session_id,
                "entry_opened",
                selected_at,
                stats::runif(1)
              ),
              session_id = session_id,
              selected_at = selected_at
            ),
            error = \(error) error
          )
          if (inherits(selection, "error")) {
            stale <- inherits(selection, "rill_orientation_invalid")
            shiny::showNotification(
              if (stale) {
                "That Orientation selection is no longer current."
              } else {
                "Rill could not record this opening. Please try again."
              },
              type = "warning"
            )
            if (!stale) {
              telemetry_log(
                "warn",
                "orientation.selection_failed",
                list("error.type" = class(selection)[[1L]])
              )
            }
            session$sendCustomMessage(
              "rill-orientation-action-rejected",
              list(action = "select")
            )
            bump_refresh()
            return()
          }
          entry_id <- selection$entry_id
          pinned_document_id <- selection$document_id
          position <- selection$position
          provenance <- selection$provenance
        }
        if (
          !identical(previous_id, entry_id) ||
            !identical(previous_document_id, pinned_document_id)
        ) {
          reader_agent(NULL)
          reader_agent_document_id(NULL)
          active_agent_run(NULL)
          clear_reader_chat(session)
        }
        retain_entry(entry_id)
        selected_id(entry_id)
        selected_document_id(pinned_document_id)
        selected_orientation_provenance(provenance)
        selected_position(position)
        if (!identical(surface, "orientation")) {
          store_mark_opened(store, actor_id, entry_id)
          record_event(
            "entry_opened",
            entry_id = entry_id,
            surface = surface,
            position = selected_position(),
            payload = list()
          )
        }
        session$sendCustomMessage(
          "rill-selection-accepted",
          list(surface = surface)
        )
        bump_refresh()
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$dismiss_orientation_card,
      {
        request <- input$dismiss_orientation_card
        card_id <- request$card_id %||% request$id %||% NULL
        revision_id <- request$revision_id %||% NULL
        rationale_hash <- request$rationale_hash %||% NULL
        if (
          is.null(card_id) ||
            is.null(revision_id) ||
            is.null(rationale_hash)
        ) {
          return()
        }
        current_orientation <- orientation_status(store, actor_id)$orientation
        current_cards <- Filter(
          \(card) {
            identical(card$card_id, card_id) &&
              identical(card$rationale_hash, rationale_hash)
          },
          current_orientation$cards %||% list()
        )
        if (
          is.null(current_orientation) ||
            !identical(current_orientation$revision_id, revision_id) ||
            length(current_cards) != 1L
        ) {
          shiny::showNotification(
            "That Orientation selection is no longer current.",
            type = "warning"
          )
          session$sendCustomMessage(
            "rill-orientation-action-rejected",
            list(action = "dismiss")
          )
          bump_refresh()
          return()
        }
        position <- match(
          card_id,
          vapply(
            current_orientation$cards %||% list(),
            `[[`,
            character(1),
            "card_id"
          )
        )
        dismissal <- tryCatch(
          store_dismiss_orientation_card(
            store,
            actor_id,
            card_id,
            revision_id,
            rationale_hash,
            event = list(
              event_id = rill_id(
                "event",
                "orientation-dismissal",
                actor_id,
                revision_id,
                card_id,
                rationale_hash
              ),
              actor_id = actor_id,
              entry_id = NULL,
              session_id = session_id,
              event_type = "orientation_card_dismissed",
              happened_at = Sys.time(),
              surface = "orientation",
              position = as.integer(position),
              payload = list()
            )
          ),
          error = function(error) {
            shiny::showNotification(
              "That Orientation selection is no longer current.",
              type = "warning"
            )
            session$sendCustomMessage(
              "rill-orientation-action-rejected",
              list(action = "dismiss")
            )
            bump_refresh()
            NULL
          }
        )
        if (is.null(dismissal)) {
          return()
        }
        bump_refresh()
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$close_reader,
      {
        clear_selection(clear_retained = FALSE)
        bump_refresh()
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$reader_chat_user_input,
      {
        question <- trimws(input$reader_chat_user_input %||% "")
        if (!nzchar(question)) {
          return()
        }
        if (is.null(selected_id())) {
          append_reader_chat(
            "Choose a story first, then ask about its selected reading copy.",
            session
          )
          return()
        }

        document <- tryCatch(selected_document(), error = \(error) error)
        if (inherits(document, "error")) {
          shiny::showNotification(
            conditionMessage(document),
            type = "error",
            duration = 8
          )
          append_reader_chat(
            "Rill couldn't prepare this story's reading copy.",
            session
          )
          return()
        }

        tryCatch(
          run_prioritized_reader_question(
            question,
            document,
            request_token = input$reader_chat_submission_id %||% NULL
          ),
          error = function(error) {
            shiny::showNotification(
              conditionMessage(error),
              type = "error",
              duration = 8
            )
            append_reader_chat(
              "Rill couldn't start that response. Finish the current response or retry.",
              session
            )
          }
        )
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$reader_chat_cancel,
      {
        pending <- shiny::isolate(pending_reader_question()) %||%
          tryCatch(
            store_get_deferred_reader_question(store, actor_id),
            error = \(error) NULL
          )
        if (!is.null(pending)) {
          if (is.function(pending_reader_question_cancel)) {
            try(pending_reader_question_cancel(), silent = TRUE)
            pending_reader_question_cancel <<- NULL
          }
          store_delete_deferred_reader_question(
            store,
            actor_id,
            pending$request_key
          )
          pending_reader_question(NULL)
          return()
        }
        run <- active_agent_run()
        if (
          is.null(run) ||
            run$status %in% terminal_agent_run_statuses
        ) {
          return()
        }

        cancelling <- store_request_agent_run_cancel(
          store,
          reader_id = actor_id,
          run_id = run$run_id
        )
        if (is.null(cancelling)) {
          return()
        }
        active_agent_run(cancelling)
        existing_intent <- agent_run_terminal_intents[[cancelling$run_id]]
        first_request <- is.null(existing_intent)
        if (first_request) {
          agent_run_terminal_intents[[cancelling$run_id]] <- "reader_cancelled"
        }
        if (
          first_request &&
            identical(cancelling$status, "cancelling")
        ) {
          draining_agent_run_id(cancelling$run_id)
          agent <- reader_agent()
          interrupted <- rill_signal_question_interrupt(
            cancelling$run_id,
            "reader_cancelled",
            fallback = if (is.null(agent)) {
              NULL
            } else {
              \(reason) agent$interrupt(reason)
            }
          )
          if (length(interrupted) == 1L && is.na(interrupted)) {
            telemetry_log(
              "warn",
              "agent_run.cancel_failed",
              list("agent.run_id" = cancelling$run_id)
            )
          }
          current <- store_get_agent_run(store, actor_id, cancelling$run_id)
          if (identical(interrupted, FALSE)) {
            draining_agent_run_id(NULL)
            terminal <- if (
              !is.null(current) &&
                current$status %in% terminal_agent_run_statuses
            ) {
              current
            } else {
              store_finish_agent_run(
                store,
                reader_id = actor_id,
                run_id = cancelling$run_id,
                worker_id = cancelling$worker_id %||% session_id,
                status = "cancelled",
                terminal_reason = "reader_cancelled"
              )
            }
          } else if (
            !is.null(current) &&
              current$status %in% terminal_agent_run_statuses
          ) {
            terminal <- current
          } else {
            terminal <- current %||% cancelling
          }
          if (!is.null(terminal)) {
            if (terminal$status %in% terminal_agent_run_statuses) {
              cancel_agent_run_deadline(terminal$run_id)
            }
            active_agent_run(terminal)
          }
        }
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$retry_agent_run,
      {
        run <- active_agent_run()
        if (
          is.null(run) ||
            !run$status %in% c("failed", "cancelled", "interrupted")
        ) {
          return()
        }
        document <- store_get_document_by_id(
          store,
          actor_id,
          run$pinned_inputs$document_id
        )
        if (is.null(document)) {
          shiny::showNotification(
            "The pinned reading copy is no longer available.",
            type = "error"
          )
          return()
        }

        tryCatch(
          run_prioritized_reader_question(
            run$pinned_inputs$question,
            document,
            retry_of = run,
            request_token = input$retry_agent_run
          ),
          error = function(error) {
            shiny::showNotification(
              conditionMessage(error),
              type = "error",
              duration = 8
            )
          }
        )
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$toggle_star,
      {
        entry <- selected_entry()
        shiny::req(isTRUE(entry$library_access))
        value <- store_toggle_state(store, actor_id, entry$entry_id, "starred")
        record_event(
          "star_changed",
          entry$entry_id,
          payload = list(value = value)
        )
        bump_refresh()
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$mark_unread,
      {
        entry <- selected_entry()
        shiny::req(isTRUE(entry$library_access))
        changed <- store_mark_unread(store, actor_id, entry$entry_id)
        if (changed) {
          record_event(
            "read_state_changed",
            entry$entry_id,
            payload = list(read = FALSE, reason = "manual_unread")
          )
        }
        status_kind("success")
        status_text("Marked story as unread")
        bump_refresh()
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$toggle_save,
      {
        entry <- selected_entry()
        shiny::req(isTRUE(entry$library_access))
        value <- store_toggle_state(store, actor_id, entry$entry_id, "saved")
        record_event(
          "save_changed",
          entry$entry_id,
          payload = list(value = value)
        )
        bump_refresh()
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$client_event,
      {
        event <- input$client_event
        allowed <- c(
          "article_impression",
          "scroll_milestone",
          "dwell_heartbeat",
          "page_hidden",
          "open_original"
        )
        if (!event$type %in% allowed) {
          return()
        }

        payload <- list(
          scroll_percent = as.integer(event$scroll_percent %||% NA_integer_),
          dwell_seconds = as.integer(event$dwell_seconds %||% NA_integer_)
        )
        record_event(
          type = event$type,
          entry_id = event$entry_id %||% selected_id(),
          surface = "reader",
          position = selected_position(),
          payload = payload,
          event_id = event$event_id %||% NULL,
          happened_at = event$happened_at %||% NULL
        )
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$add_feed,
      {
        url <- trimws(input$new_feed_url %||% "")
        if (!nzchar(url)) {
          status_kind("error")
          status_text("Enter a feed or website URL.")
          session$sendCustomMessage(
            "rill-input-validity",
            list(
              id = "new_feed_url",
              invalid = TRUE,
              message = "Enter a feed or website URL."
            )
          )
          shiny::showNotification(
            "Enter a feed or website URL.",
            type = "warning"
          )
          return()
        }
        session$sendCustomMessage(
          "rill-input-validity",
          list(id = "new_feed_url", invalid = FALSE)
        )

        result <- tryCatch(
          shiny::withProgress(
            message = "Finding and reading the feed",
            value = 0.5,
            {
              ingest_feed_url(store, actor_id, url)
            }
          ),
          error = function(error) error
        )
        if (inherits(result, "error")) {
          telemetry_log(
            "warn",
            "feed.add_failed",
            list("error.type" = class(result)[[1]])
          )
          status_kind("error")
          status_text(conditionMessage(result))
          shiny::showNotification(
            conditionMessage(result),
            type = "error",
            duration = 8
          )
          return()
        }

        status_kind("success")
        status_text(paste(
          "Added",
          result$feed$title,
          "\u00b7",
          result$added,
          "stories"
        ))
        shiny::updateTextInput(session, "new_feed_url", value = "")
        record_event(
          "feed_added",
          surface = "sidebar",
          payload = list(feed_id = result$feed$feed_id)
        )
        bump_refresh()
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$rename_feed,
      {
        feed_id <- selected_feed()
        title <- trimws(input$feed_title %||% "")
        if (is.null(feed_id) || !nzchar(title)) {
          status_kind("error")
          status_text("Select a feed and enter a name.")
          session$sendCustomMessage(
            "rill-input-validity",
            list(
              id = "feed_title",
              invalid = TRUE,
              message = "Enter a name for the selected feed."
            )
          )
          shiny::showNotification(
            "Select a feed and enter a name.",
            type = "warning"
          )
          return()
        }
        session$sendCustomMessage(
          "rill-input-validity",
          list(id = "feed_title", invalid = FALSE)
        )

        result <- tryCatch(
          store_rename_feed(store, actor_id, feed_id, title),
          error = function(error) error
        )
        if (inherits(result, "error")) {
          status_kind("error")
          status_text(conditionMessage(result))
          shiny::showNotification(
            conditionMessage(result),
            type = "error"
          )
          return()
        }

        status_kind("success")
        status_text(paste("Renamed feed to", title))
        record_event(
          "feed_renamed",
          surface = "sidebar",
          payload = list(feed_id = feed_id, title = title)
        )
        bump_refresh()
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$move_feed,
      {
        feed_id <- selected_feed()
        folder <- trimws(input$feed_folder %||% "")
        if (is.null(feed_id) || !nzchar(folder)) {
          status_kind("error")
          status_text("Select a feed and enter a folder.")
          session$sendCustomMessage(
            "rill-input-validity",
            list(
              id = "feed_folder",
              invalid = TRUE,
              message = "Enter a folder for the selected feed."
            )
          )
          shiny::showNotification(
            "Select a feed and enter a folder.",
            type = "warning"
          )
          return()
        }
        session$sendCustomMessage(
          "rill-input-validity",
          list(id = "feed_folder", invalid = FALSE)
        )

        result <- tryCatch(
          store_move_feed(store, actor_id, feed_id, folder),
          error = function(error) error
        )
        if (inherits(result, "error")) {
          status_kind("error")
          status_text(conditionMessage(result))
          shiny::showNotification(
            conditionMessage(result),
            type = "error"
          )
          return()
        }

        status_kind("success")
        status_text(paste("Moved feed to", folder))
        record_event(
          "feed_moved",
          surface = "sidebar",
          payload = list(feed_id = feed_id, folder = folder)
        )
        bump_refresh()
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$unsubscribe_feed,
      {
        feed_id <- selected_feed()
        if (is.null(feed_id)) {
          return()
        }
        result <- tryCatch(
          store_unsubscribe_feed(store, actor_id, feed_id),
          error = function(error) error
        )
        if (inherits(result, "error")) {
          status_kind("error")
          status_text(conditionMessage(result))
          shiny::showNotification(
            conditionMessage(result),
            type = "error"
          )
          return()
        }

        selected_feed(NULL)
        clear_selection(force = TRUE)
        status_kind("success")
        status_text("Unsubscribed. Reading state was preserved.")
        record_event(
          "feed_unsubscribed",
          surface = "sidebar",
          payload = list(feed_id = feed_id)
        )
        bump_refresh()
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$import_opml,
      {
        upload <- input$import_opml
        if (is.null(upload) || !nrow(upload)) {
          return()
        }

        subscriptions <- tryCatch(
          read_opml(upload$datapath[[1]]),
          error = function(error) error
        )
        if (inherits(subscriptions, "error")) {
          status_kind("error")
          status_text(paste(
            "Couldn't import OPML:",
            conditionMessage(subscriptions)
          ))
          shiny::showNotification(
            "That file could not be read as OPML.",
            type = "error",
            duration = 8
          )
          session$sendCustomMessage("rill-reset-file", "import_opml")
          return()
        }

        result <- shiny::withProgress(
          message = "Importing subscriptions",
          value = 0,
          {
            import_opml_subscriptions(
              store,
              actor_id,
              subscriptions,
              refresh = FALSE,
              progress = function(index, total, title) {
                shiny::setProgress(
                  value = index / total,
                  detail = title
                )
              }
            )
          }
        )
        status_kind(
          if (result$total == 0L) {
            "warning"
          } else if (result$failed > 0L || result$refresh_failed > 0L) {
            if (result$imported > 0L) "warning" else "error"
          } else {
            "success"
          }
        )
        import_status <- format_opml_import_status(
          result,
          demo_mode = config$demo_mode,
          refresh_hint = TRUE
        )
        status_text(import_status)
        shiny::showNotification(
          import_status,
          type = switch(
            status_kind(),
            error = "error",
            warning = "warning",
            "message"
          ),
          duration = 8
        )
        record_event(
          "opml_imported",
          surface = "sidebar",
          payload = list(
            total = result$total,
            imported = result$imported,
            added = result$added,
            updated = result$updated,
            stories = result$stories,
            refresh_failed = result$refresh_failed,
            failed = result$failed
          )
        )
        session$sendCustomMessage("rill-reset-file", "import_opml")
        bump_refresh()
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$refresh_feeds,
      {
        refresh_feeds_now()
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$prepare_today,
      {
        shiny::req(identical(input$view, "today"))
        result <- tryCatch(
          shiny::withProgress(
            message = "Preparing today's reading copies",
            value = 0,
            {
              prepare_today_documents(
                store,
                config,
                reader_id = actor_id,
                progress = function(index, total, title) {
                  shiny::setProgress(
                    value = index / total,
                    detail = title
                  )
                }
              )
            }
          ),
          error = function(error) error
        )
        if (inherits(result, "error")) {
          status_kind("error")
          status_text("Today's reading copies couldn't be prepared")
          shiny::showNotification(
            conditionMessage(result),
            type = "error",
            duration = 8
          )
          return()
        }

        status <- format_prepare_today_status(result)
        status_kind(if (result$failed > 0L) "warning" else "success")
        status_text(status)
        shiny::showNotification(
          status,
          type = if (result$failed > 0L) "warning" else "message",
          duration = 8
        )
        record_event(
          "today_prepared",
          surface = "reading_queue",
          payload = result[c("total", "cached", "prepared", "failed")]
        )
      },
      ignoreInit = TRUE
    )

    mark_scope_read <- function(before = NULL, reason) {
      marked <- store_mark_entries_read(
        store,
        actor_id,
        feed_id = selected_feed(),
        before = before,
        reason = reason
      )
      count <- length(marked)
      if (count) {
        retained_ids(setdiff(retained_ids(), marked))
        if (!is.null(selected_id()) && selected_id() %in% marked) {
          clear_selection(clear_retained = FALSE)
        }
        payload <- list(
          count = count,
          feed_id = selected_feed(),
          reason = reason
        )
        if (!is.null(before)) {
          payload$before <- format(before, tz = "UTC", usetz = TRUE)
        }
        record_event(
          "read_state_bulk_changed",
          surface = "reading_queue",
          payload = payload
        )
      }

      status <- if (identical(reason, "bulk_older_than_day")) {
        if (count) {
          paste(
            "Marked",
            count,
            if (count == 1L) "story" else "stories",
            "older than a day as read"
          )
        } else {
          "No unread stories older than a day"
        }
      } else if (count) {
        paste(
          "Marked",
          count,
          if (count == 1L) "story" else "stories",
          "as read"
        )
      } else {
        "No unread stories to mark"
      }
      status_kind("success")
      status_text(status)
      shiny::showNotification(status, type = "message")
      bump_refresh()
      invisible(marked)
    }

    shiny::observeEvent(
      input$mark_all_read,
      {
        mark_scope_read(reason = "bulk_all")
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$mark_older_read,
      {
        mark_scope_read(
          before = Sys.time() - 24 * 60 * 60,
          reason = "bulk_older_than_day"
        )
      },
      ignoreInit = TRUE
    )

    session$onSessionEnded(function() {
      if (is.function(pending_reader_question_cancel)) {
        try(pending_reader_question_cancel(), silent = TRUE)
        pending_reader_question_cancel <<- NULL
      }
      if (is.function(deferred_reader_question_resume_cancel)) {
        try(deferred_reader_question_resume_cancel(), silent = TRUE)
        deferred_reader_question_resume_cancel <<- NULL
      }
      if (is.function(orientation_retry_cancel)) {
        try(orientation_retry_cancel(), silent = TRUE)
        orientation_retry_cancel <<- NULL
      }
      if (is.function(visible_agent_run_poll_cancel)) {
        try(visible_agent_run_poll_cancel(), silent = TRUE)
        visible_agent_run_poll_cancel <<- NULL
      }
    })

    if (isTRUE(config$refresh_on_start)) {
      session$onFlushed(function() refresh_feeds_now(), once = TRUE)
    }
  }
}
