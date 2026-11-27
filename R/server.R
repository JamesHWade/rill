# domain, isolated, and log failures instead.
rill_session_callback <- function(session, callback) {
  force(session)
  force(callback)
  function(...) {
    tryCatch(
      shiny::withReactiveDomain(session, shiny::isolate(callback(...))),
      error = function(error) {
        telemetry_log(
          "error",
          "session.callback_failed",
          list("error.type" = class(error)[[1L]])
        )
        invisible(NULL)
      }
    )
  }
}

rill_server <- function(
  config,
  store,
  preview_fetch = queue_preview_fetch_async
) {
  force(config)
  force(store)

  function(input, output, session, reader_id = config$actor_id) {
    actor_id <- reader_id
    preview_src <- queue_preview_server(
      store,
      actor_id,
      session,
      fetch = preview_fetch
    )
    session_id <- rill_id(
      "session",
      actor_id,
      session$token,
      utc_now(),
      stats::runif(1)
    )
    in_session <- \(callback) rill_session_callback(session, callback)
    # An observer that returns a promise makes Shiny hold every later input
    # from the session, including Stop, until the promise settles. Answers
    # stream in the background instead; shinychat already reports failures.
    release_answer_promise <- function(result) {
      if (promises::is.promise(result)) {
        promises::catch(result, \(error) NULL)
      }
      invisible(NULL)
    }
    selected_id <- shiny::reactiveVal(NULL)
    orientation_requested <- shiny::reactiveVal(FALSE)
    restored_question <- shiny::reactiveVal(NULL)
    selected_document_id <- shiny::reactiveVal(NULL)
    reading_copy <- shiny::reactiveVal(NULL)
    cached_reading_copy <- NULL
    opening_telemetry <- reading_telemetry(config$telemetry_enabled)
    queue_request_id <- shiny::reactiveVal(NULL)
    queue_telemetry <- view_telemetry(
      config$telemetry_enabled,
      "queue.view",
      "queue",
      "visible_ms"
    )
    shiny::observeEvent(
      input$queue_view_request,
      {
        request <- input$queue_view_request
        if (
          !is.list(request) ||
            !store_scalar_string(request$id) ||
            !grepl("^[a-f0-9]{32}$", request$id) ||
            !isTRUE(
              request$view %in%
                c("today", "week", "month", "unread", "all", "saved", "starred")
            )
        ) {
          return()
        }
        queue_request_id(request$id)
        queue_telemetry$begin(request$id, request$view)
      },
      priority = 2000
    )
    shiny::observeEvent(input$queue_view_visible, {
      request <- input$queue_view_visible
      if (!is.list(request)) {
        return()
      }
      queue_telemetry$complete(
        request$id,
        request$elapsed_ms,
        request$dom_ready_ms
      )
    })
    preparation_tick <- shiny::reactiveVal(0L)
    today_preparation_pending <- shiny::reactiveVal(FALSE)
    selected_orientation_provenance <- shiny::reactiveVal(NULL)
    selected_position <- shiny::reactiveVal(NA_integer_)
    selected_feed <- shiny::reactiveVal(NULL)
    selected_orientation_theme <- shiny::reactiveVal(NULL)
    selected_folder <- shiny::reactiveVal(NULL)
    selected_group_ids <- shiny::reactiveVal(character())
    selected_ungrouped <- shiny::reactiveVal(FALSE)
    selected_group_match <- shiny::reactiveVal("any")
    feed_groups <- shiny::reactive({
      refresh_tick()
      feed_management_tick()
      store_list_groups(store, actor_id)
    })
    orientation_control <- shiny::reactiveVal(NULL)
    orientation_attempted_boundary <- shiny::reactiveVal(NULL)
    browse_queue_pending <- shiny::reactiveVal(FALSE)
    retained_ids <- shiny::reactiveVal(character())
    retained_context <- shiny::reactiveVal(NULL)
    refresh_tick <- shiny::reactiveVal(0L)
    queue_state_tick <- shiny::reactiveVal(0L)
    selected_state_tick <- shiny::reactiveVal(0L)
    library_state_tick <- shiny::reactiveVal(0L)
    library_refresh_cancel <- NULL
    feed_management_tick <- shiny::reactiveVal(0L)
    status_text <- shiny::reactiveVal(NULL)
    status_kind <- shiny::reactiveVal("info")
    preparation_failures <- shiny::reactiveVal(list())
    memory_access <- if (isTRUE(config$reader_memory_enabled)) {
      authorize <- session$userData$rill_reader_authorize
      if (is.null(authorize) && identical(config$identity_mode, "local")) {
        authorize <- function() TRUE
      }
      reader_memory_access(store, actor_id, authorize)
    } else {
      NULL
    }
    reader_agent <- shiny::reactiveVal(NULL)
    reader_agent_document_id <- shiny::reactiveVal(NULL)
    reader_agent_memory_basis <- shiny::reactiveVal(NULL)
    reader_memory_context_notice <- shiny::reactiveVal(FALSE)
    active_agent_run <- shiny::reactiveVal(NULL)
    feedback_controller <- reader_feedback_server(
      store,
      actor_id,
      active_agent_run,
      session
    )
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
    completed_question <- tryCatch(
      store_get_latest_question_agent_run(
        store,
        actor_id,
        status = "completed"
      ),
      error = \(error) NULL
    )
    if (!is.null(completed_question)) {
      document <- tryCatch(
        store_get_document_by_id(
          store,
          actor_id,
          completed_question$pinned_inputs$document_id
        ),
        error = \(error) NULL
      )
      if (!is.null(document)) {
        restored_question(completed_question)
      }
    }
    if (
      !is.null(existing_agent_run) &&
        identical(existing_agent_run$kind, "question") &&
        (identical(existing_agent_run$status, "completed") ||
          question_run_reopens(existing_agent_run))
    ) {
      active_agent_run(existing_agent_run)
      if (!identical(existing_agent_run$status, "completed")) {
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
    }

    bump_refresh <- function(feeds_changed = FALSE) {
      refresh_tick(shiny::isolate(refresh_tick()) + 1L)
      if (feeds_changed) {
        feed_management_tick(shiny::isolate(feed_management_tick()) + 1L)
      }
    }

    # An error in an observer ends the session, so reading actions report
    # store failures instead. A story can leave the Library while its queue
    # is on screen, for example after unsubscribing on another device.
    attempt_reader_action <- function(action, failure) {
      tryCatch(action(), error = function(error) {
        if (inherits(error, "rill_entry_forbidden")) {
          shiny::showNotification(
            "That story is no longer in your Library.",
            type = "warning"
          )
        } else {
          telemetry_log(
            "warn",
            "reader.action_failed",
            list("error.type" = class(error)[[1L]])
          )
          shiny::showNotification(failure, type = "error")
        }
        bump_refresh()
        NULL
      })
    }

    bump_entry_refresh <- function(entry_id) {
      queue_state_tick(shiny::isolate(queue_state_tick()) + 1L)
      if (identical(entry_id, shiny::isolate(selected_id()))) {
        selected_state_tick(shiny::isolate(selected_state_tick()) + 1L)
      }
      session$onFlushed(
        function() {
          if (is.function(library_refresh_cancel)) {
            library_refresh_cancel()
          }
          library_refresh_cancel <<- later::later(
            function() {
              library_refresh_cancel <<- NULL
              if (!session$isClosed()) {
                shiny::withReactiveDomain(
                  session,
                  shiny::isolate({
                    library_state_tick(library_state_tick() + 1L)
                  })
                )
              }
            },
            0.15
          )
        },
        once = TRUE
      )
    }
    session$onSessionEnded(function() {
      if (is.function(library_refresh_cancel)) library_refresh_cancel()
    })

    acknowledge_orientation_queue <- function() {
      session$onFlushed(
        \() session$sendCustomMessage("rill-browse-queue-ready", list()),
        once = TRUE
      )
    }

    current_context <- function() {
      view <- input$view %||% "unread"
      list(
        view = view,
        feed_id = selected_feed(),
        folder = selected_folder(),
        group_ids = selected_group_ids(),
        group_match = selected_group_match(),
        ungrouped = selected_ungrouped(),
        theme_entry_ids = selected_orientation_theme()$entry_ids,
        sort = input$story_sort %||% "newest",
        calendar = if (view %in% c("today", "week", "month")) {
          calendar_window()$window
        }
      )
    }

    clear_selection <- function(clear_retained = TRUE, force = FALSE) {
      if (!isTRUE(force) && reader_response_in_flight()) {
        return(invisible(FALSE))
      }
      selected_id(NULL)
      reading_copy(NULL)
      cached_reading_copy <<- NULL
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
    schedule_visible_agent_run_poll <- NULL
    schedule_visible_agent_run_poll <- function(run_id, delay = 0.05) {
      if (is.function(visible_agent_run_poll_cancel)) {
        return(invisible(NULL))
      }
      visible_agent_run_poll_cancel <<- later::later(
        in_session(function() {
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
            if (identical(shiny::isolate(draining_agent_run_id()), run_id)) {
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
        }),
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

    shiny::observeEvent(active_agent_run(), {
      run <- active_agent_run()
      if (
        identical(run$status, "completed") &&
          store_scalar_string(run$pinned_inputs$document_id)
      ) {
        restored_question(run)
      }
    })

    update_visible_agent_run <- function(run) {
      if (session$isClosed()) {
        return(invisible(run))
      }
      visible <- shiny::isolate(active_agent_run())
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
          identical(shiny::isolate(draining_agent_run_id()), run_id)
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
          in_session(function() {
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
          }),
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
          in_session(function() {
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
          }),
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
          in_session(function() {
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
          }),
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
        in_session(function() {
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
        }),
        delay = delay
      )
      invisible(deadline)
    }

    record_agent_run_partials <- function(run, deadline) {
      last_saved_at <- as.POSIXct(NA, tz = "UTC")
      function(partial) {
        feedback_controller$remember_partial(run$run_id, partial)
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

    reset_reader_chat <- function() {
      reader_agent(NULL)
      reader_agent_document_id(NULL)
      active_agent_run(NULL)
      reader_memory_context_notice(FALSE)
      clear_reader_chat(session)
    }

    invalidate_reader_memory_context <- function() {
      had_agent <- !is.null(reader_agent())
      reader_agent(NULL)
      reader_agent_document_id(NULL)
      reader_agent_memory_basis(NULL)
      if (had_agent) {
        reader_memory_context_notice(TRUE)
        shiny::showNotification(
          "Reader Memory changed. This starts a new conversation.",
          type = "message"
        )
      }
      invisible(NULL)
    }

    reader_agent_for <- function(document, memory_basis = NULL) {
      if (!is.null(memory_access)) {
        memory_basis <- canonicalize_json_value(memory_basis %||% list())
        reader_memory_consult(memory_access, memory_basis)
      }
      agent <- reader_agent()
      if (
        !is.null(agent) && !identical(reader_agent_memory_basis(), memory_basis)
      ) {
        invalidate_reader_memory_context()
        agent <- NULL
      }
      if (
        is.null(agent) ||
          !identical(reader_agent_document_id(), document$document_id) ||
          !identical(reader_agent_memory_basis(), memory_basis)
      ) {
        agent <- rill_reader_agent(
          document = document,
          reader_id = actor_id,
          session_id = session_id,
          model = config$agent_model,
          base_url = config$agent_base_url %||% "",
          on_stop = finish_agent_run,
          memory_access = memory_access,
          memory_basis = memory_basis
        )
        reader_agent(agent)
        reader_agent_document_id(document$document_id)
        reader_agent_memory_basis(memory_basis)
        if (reader_memory_context_notice()) {
          append_reader_chat(
            paste0(
              "**New conversation**\n\n",
              "Reader Memory changed, so this question starts a new ",
              "conversation. Earlier messages stay here but aren't sent to ",
              "Rill."
            ),
            session
          )
          reader_memory_context_notice(FALSE)
        }
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
      index <- shiny::isolate(agent_request_index()) + 1L
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
      if (!is.null(shiny::isolate(draining_agent_run_id()))) {
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
      preserved_inputs <- pinned_inputs %||% retry_of$pinned_inputs %||% NULL
      if (
        !is.null(preserved_inputs) &&
          !identical(
            "reader_memory" %in% names(preserved_inputs),
            !is.null(memory_access)
          )
      ) {
        store_delete_deferred_reader_question(store, actor_id, request_key)
        cli::cli_abort(
          c(
            "Reader Memory settings changed before Rill could answer.",
            "i" = "Ask a new question to use the current memory setting."
          ),
          class = "rill_agent_memory_mode_changed"
        )
      }
      memory_basis <- if (!is.null(memory_access)) {
        if (!is.null(preserved_inputs)) {
          preserved_inputs$reader_memory %||% list()
        } else {
          lapply(
            reader_memory_list(memory_access, consult = TRUE),
            `[[`,
            "basis"
          )
        }
      } else {
        NULL
      }
      agent <- tryCatch(
        reader_agent_for(document, memory_basis),
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
            orientation_selection = shiny::isolate(selected_orientation_provenance()),
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
      if (
        !is.null(memory_access) &&
          is.null(preserved_inputs) &&
          !is.null(pinned_inputs)
      ) {
        pinned_inputs$reader_memory <- memory_basis
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
          in_session(function() {
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
          }),
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
        in_session(function() {
          deferred_reader_question_resume_cancel <<- NULL
          if (!session$isClosed()) {
            resume_deferred_reader_question()
          }
          NULL
        }),
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
          if (inherits(resumed, "rill_agent_runtime_identity_changed")) {
            paste(
              "Rill didn't send your held question because its model changed.",
              "Ask again to send it to the new model."
            )
          } else {
            "Rill couldn't send your held question. Ask it again."
          },
          session
        )
      }
      invisible(resumed)
    }

    session$onFlushed(in_session(resume_deferred_reader_question), once = TRUE)

    feeds <- shiny::reactive({
      refresh_tick()
      library_state_tick()
      store_list_feeds(store, actor_id)
    })

    calendar_window <- shiny::reactive({
      view <- input$view %||% "unread"
      if (view %in% c("today", "week", "month")) {
        shiny::invalidateLater(60 * 1000, session)
      }
      now <- reader_calendar_now()
      timezone <- normalize_reader_timezone(input$reader_timezone)
      list(
        now = now,
        timezone = timezone,
        window = entry_view_window(view, now, timezone)
      )
    })

    # The queue loads stories in pages. One extra row shows whether more
    # exist, and asking for more past the loaded page raises the limit.
    queue_page_size <- 150L
    queue_limit <- shiny::reactiveVal(queue_page_size)
    queue_query <- shiny::reactive({
      refresh_tick()
      queue_state_tick()
      calendar <- calendar_window()
      queue_telemetry$activate()
      telemetry_local_span("queue.entries")
      store_list_entries(
        store,
        actor_id,
        view = input$view %||% "unread",
        feed_id = selected_feed(),
        folder = selected_folder(),
        group_ids = selected_group_ids(),
        group_match = selected_group_match(),
        ungrouped = selected_ungrouped(),
        limit = queue_limit() + 1L,
        sort = input$story_sort %||% "newest",
        now = calendar$now,
        timezone = calendar$timezone,
        include_content = FALSE,
        entry_ids = selected_orientation_theme()$entry_ids
      )
    })
    queue_entries <- shiny::reactive({
      utils::head(queue_query(), queue_limit())
    })
    queue_has_more <- shiny::reactive({
      nrow(queue_query()) > queue_limit()
    })

    orientation_session <- orientation_server(
      input,
      session,
      config,
      store,
      actor_id,
      session_id,
      selected_id,
      refresh_tick,
      library_state_tick,
      bump_refresh,
      orientation_control,
      orientation_attempted_boundary
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

      missing_ids <- setdiff(ids, as.character(rows$entry_id))
      if (!length(missing_ids)) {
        return(rows)
      }
      telemetry_local_span("queue.retained_entries")
      retained <- store_list_entries(
        store,
        actor_id,
        view = "all",
        feed_id = selected_feed(),
        folder = selected_folder(),
        group_ids = selected_group_ids(),
        group_match = selected_group_match(),
        ungrouped = selected_ungrouped(),
        limit = 500L,
        sort = input$story_sort %||% "newest",
        include_content = FALSE,
        entry_ids = missing_ids
      )
      combined <- rbind(rows, retained)
      combined[
        entry_sort_index(combined, input$story_sort %||% "newest"),
        ,
        drop = FALSE
      ]
    })

    shiny::observe({
      ids <- selected_group_ids()
      valid <- intersect(ids, feed_groups()$group_id)
      if (!identical(ids, valid)) {
        clear_selection()
        selected_group_ids(valid)
      }
    })

    selected_feed_title <- shiny::reactive({
      if (selected_ungrouped()) {
        return("Ungrouped")
      }
      if (length(selected_group_ids())) {
        groups <- feed_groups()
        names <- groups$name[match(selected_group_ids(), groups$group_id)]
        return(paste(
          names,
          collapse = if (selected_group_match() == "all") " + " else " or "
        ))
      }
      if (!is.null(selected_folder())) {
        return(selected_folder())
      }
      feed_id <- selected_feed()
      if (is.null(feed_id)) {
        return(NULL)
      }
      feed_rows <- feeds()
      title <- feed_rows$title[feed_rows$feed_id == feed_id]
      if (length(title)) title[[1]] else NULL
    })

    selected_entry <- shiny::reactive({
      opening_telemetry$activate()
      entry_id <- selected_id()
      shiny::req(entry_id)
      refresh_tick()
      selected_state_tick()
      entry <- telemetry_span(
        "store.entry.lookup",
        store_get_entry(store, actor_id, entry_id)
      )
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
      opening_telemetry$activate()
      shiny::req(selected_id())
      # The copy depends on which story is selected, not on its read, star, or
      # save state. Refreshes therefore don't rebuild the article, which would
      # lose focus, text selection, and scroll position.
      entry <- shiny::isolate(selected_entry())
      shiny::req(!is.null(entry))
      document_id <- selected_document_id()
      if (!is.null(document_id)) {
        document <- telemetry_span(
          "store.pinned_copy.lookup",
          store_get_document_by_id(store, actor_id, document_id)
        )
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
      current <- reading_copy() %||% cached_reading_copy
      if (!is.null(current) && identical(current$entry_id, entry$entry_id)) {
        return(current)
      }
      document <- reading_document(store, actor_id, entry)
      cached_reading_copy <<- document
      document
    })

    if (!is.null(memory_access)) {
      reader_memory_server(
        "memory",
        memory_access,
        selected_document,
        changed = invalidate_reader_memory_context,
        demo = config$demo_mode
      )
    }

    article_preparer <- article_preparation_controller(
      store,
      config,
      actor_id,
      updated = function(id, result) {
        if (session$isClosed()) {
          return(invisible(NULL))
        }
        shiny::withReactiveDomain(
          session,
          shiny::isolate({
            if (!is.null(result)) {
              preparation_failures(Filter(
                function(failure) {
                  !identical(failure$entry_id, id)
                },
                preparation_failures()
              ))
            }
            if (!is.null(result$failure)) {
              result$failure$entry_id <- id
              preparation_failures(utils::tail(
                c(preparation_failures(), list(result$failure)),
                20L
              ))
              status_kind("warning")
              status_text(
                "Some full articles aren't ready. You can still read their feed copies."
              )
            } else if (!is.null(result) && !length(preparation_failures())) {
              status_kind("success")
              status_text("Full article prepared. Load it when you're ready.")
            }
            preparation_tick(preparation_tick() + 1L)
          })
        )
      },
      idle = function() {
        if (session$isClosed()) {
          return(invisible(NULL))
        }
        shiny::withReactiveDomain(
          session,
          shiny::isolate({
            if (today_preparation_pending()) {
              today_preparation_pending(FALSE)
              if (!length(preparation_failures())) {
                status_kind("success")
                status_text(
                  "Finished checking today's stories for full articles."
                )
              }
            }
          })
        )
      }
    )
    shiny::observeEvent(
      selected_id(),
      {
        opening_telemetry$activate()
        if (!is.null(selected_document_id())) {
          return(invisible(NULL))
        }
        if (full_reading_document(selected_document())) {
          return(invisible(NULL))
        }
        entry_id <- selected_id()
        context <- telemetry_context()
        session$onFlushed(
          function() {
            if (!session$isClosed()) {
              article_preparer$request(entry_id, context = context)
            }
          },
          once = TRUE
        )
      },
      ignoreNULL = TRUE
    )

    feed_refresh <- feed_refresh_server(
      input,
      session,
      config,
      store,
      actor_id,
      status_kind,
      status_text,
      record_event,
      bump_refresh,
      request_preparation = function() {
        article_preparer$request(preparation_candidates(
          store,
          actor_id,
          backend = config$defuddle_backend
        ))
      }
    )

    navigation_groups <- shiny::reactive({
      group_navigation_index(feeds(), feed_groups())
    })

    output$feed_nav <- shiny::renderUI({
      telemetry_local_span("navigation.render")
      feed_navigation_ui(
        feeds(),
        navigation_groups,
        selected_feed,
        selected_group_ids,
        selected_ungrouped,
        selected_folder,
        \() shiny::isolate(input$open_feed_groups)
      )
    })

    reading_group_choices <- NULL
    shiny::observeEvent(feed_groups(), {
      groups <- feed_groups()
      if (identical(groups, reading_group_choices)) {
        return()
      }
      reading_group_choices <<- groups
      shiny::updateSelectizeInput(
        session,
        "reading_groups",
        choices = stats::setNames(groups$group_id, groups$name),
        selected = intersect(
          input$reading_groups %||% character(),
          groups$group_id
        )
      )
    })
    shiny::observeEvent(
      selected_group_ids(),
      {
        shiny::updateSelectizeInput(
          session,
          "reading_groups",
          selected = selected_group_ids()
        )
      },
      ignoreNULL = FALSE
    )
    shiny::observeEvent(selected_group_match(), {
      shiny::updateRadioButtons(
        session,
        "reading_group_match",
        selected = selected_group_match()
      )
    })

    feed_management <- feed_management_server(
      input,
      output,
      session,
      store,
      actor_id,
      selected_feed,
      feed_groups,
      feed_management_tick,
      bump_refresh,
      record_event,
      status_kind,
      status_text,
      feed_refresh
    )

    output$orientation_destination_settings <- shiny::renderUI({
      orientation_destination_settings_ui(orientation_session$destination())
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
      theme <- selected_orientation_theme()
      if (!is.null(theme)) {
        label <- paste(label, "\u00b7", theme$name)
      }
      shiny::tags$h1(label)
    })

    output$calendar_context <- shiny::renderUI({
      calendar <- calendar_window()
      window <- calendar$window
      if (is.null(window$since)) {
        return(NULL)
      }
      timezone <- calendar$timezone
      start <- as.Date(format(window$since, "%Y-%m-%d", tz = timezone))
      end <- as.Date(format(window$before, "%Y-%m-%d", tz = timezone)) - 1L
      date_label <- function(date) sub("  ", " ", format(date, "%b %e, %Y"))
      label <- date_label(start)
      if (end != start) {
        label <- paste(label, "\u2013", date_label(end))
      }
      zone_label <- format(calendar$now, "%Z", tz = timezone)
      if (!identical(input$reader_timezone, timezone)) {
        zone_label <- "UTC (browser time zone unavailable)"
      }
      shiny::tags$p(
        class = "calendar-context",
        title = paste0(
          "Published from midnight on ",
          date_label(start),
          " to midnight after ",
          date_label(end),
          " in ",
          timezone,
          ". Uses the time added when no publication time is available."
        ),
        label,
        shiny::tags$span(class = "calendar-zone", paste("\u00b7", zone_label))
      )
    })

    output$story_count <- shiny::renderUI({
      count <- nrow(queue_entries())
      label <- if (queue_has_more()) {
        paste("More than", count, "stories")
      } else {
        paste(count, if (count == 1L) "story" else "stories")
      }
      shiny::tags$span(
        class = "count-pill",
        title = label,
        `aria-label` = label,
        if (queue_has_more()) paste0(count, "+") else count
      )
    })

    output$orientation_queue_status <- shiny::renderUI({
      if (!is.null(selected_id())) {
        return(NULL)
      }
      state <- orientation_session$state()
      orientation_queue_status_ui(
        state$orientation,
        state$candidates,
        preparing = orientation_session$preparing(),
        failure = orientation_session$display_failure(),
        processing_note = orientation_processing_note(
          store,
          state$orientation,
          config,
          destination_state = orientation_session$destination(),
          preparing = orientation_session$preparing()
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
      shiny::tagList(
        prepare_today_button(),
        if (length(preparation_failures())) {
          shiny::actionLink("preparation_details", "Preparation details")
        }
      )
    })

    output$read_actions <- shiny::renderUI({
      read_actions_ui(selected_feed_title())
    })

    output$queue_filters <- shiny::renderUI({
      queue_filters_ui(input$view %||% "unread")
    })

    queue_batch <- queue_batch_server(
      current_context,
      input,
      \() match(selected_id(), entries()$entry_id, nomatch = 0L)
    )
    queue_limit_context <- NULL
    shiny::observeEvent(current_context(), {
      context <- current_context()
      if (!identical(context, queue_limit_context)) {
        queue_limit_context <<- context
        queue_limit(queue_page_size)
      }
    })
    shiny::observeEvent(
      input$queue_more,
      {
        if (queue_has_more() && queue_batch() + 30L > nrow(queue_entries())) {
          queue_limit(queue_limit() + queue_page_size)
        }
      },
      ignoreInit = TRUE,
      priority = 10
    )
    render_queue_cards <- queue_card_renderer()
    output$story_list <- shiny::renderUI({
      queue_telemetry$activate()
      span <- telemetry_local_span("queue.render")
      rows <- entries()
      total <- nrow(rows)
      rows <- utils::head(rows, queue_batch())
      rendered <- render_queue_cards(rows, selected_id(), preview_src)
      telemetry_attributes(
        span,
        list(
          "queue.view" = input$view %||% "unread",
          "queue.rows" = nrow(rows),
          "queue.available_rows" = total,
          "queue.cards_built" = rendered$built
        )
      )
      queue_telemetry$annotate(list(
        "queue.view" = input$view %||% "unread",
        "queue.rows" = nrow(rows),
        "queue.available_rows" = total,
        "queue.timezone" = calendar_window()$timezone,
        "queue.since" = as.character(calendar_window()$window$since),
        "queue.before" = as.character(calendar_window()$window$before)
      ))
      request_id <- queue_request_id()
      session$onFlushed(\() queue_telemetry$flushed(request_id), once = TRUE)
      more <- min(30L, total - nrow(rows))
      shiny::tags$div(
        class = "queue-batch",
        `data-queue-view` = input$view %||% "unread",
        `data-queue-request` = queue_request_id() %||% "",
        `data-queue-context` = digest::digest(
          current_context(),
          algo = "xxhash64"
        ),
        if (!total) {
          empty_story_list(
            input$view %||% "unread",
            selected_feed_title()
          )
        },
        rendered$cards,
        if (nrow(rows) < total || queue_has_more()) {
          shiny::tags$button(
            id = "queue_more",
            type = "button",
            class = "btn btn-outline-secondary queue-more",
            if (more > 0L) {
              sprintf(
                "Show %d more %s",
                more,
                if (more == 1L) "story" else "stories"
              )
            } else {
              "Show more stories"
            }
          )
        }
      )
    })

    output$reader_header <- shiny::renderUI({
      telemetry_local_span("article.header.render")
      if (is.null(selected_id())) {
        state <- orientation_session$state()
        feedback_token <- feedback_controller$set_orientation(
          state$orientation,
          state$candidates
        )
        header <- orientation_ui(
          state$orientation,
          state$candidates,
          feedback_token = feedback_token,
          preparing = orientation_session$preparing(),
          failure = orientation_session$display_failure(),
          processing_note = orientation_processing_note(
            store,
            state$orientation,
            config,
            destination_state = orientation_session$destination(),
            preparing = orientation_session$preparing()
          )
        )
        if (isTRUE(orientation_requested())) {
          header$attribs$id <- "rill-orientation"
        }
        return(header)
      }

      document <- selected_document()
      preparation_tick()
      state <- if (
        !full_reading_document(document) && is.null(selected_document_id())
      ) {
        shiny::invalidateLater(30000, session)
        article_preparation_status(
          store,
          selected_id(),
          article_preparer$busy(selected_id()),
          backend = config$defuddle_backend
        )
      } else {
        "missing"
      }
      reader_article_header_ui(
        selected_entry(),
        document,
        can_prepare = is.null(selected_document_id()),
        preparation = state
      )
    })

    output$reader_body <- shiny::renderUI({
      opening_telemetry$activate()
      if (is.null(selected_id())) {
        return(NULL)
      }
      document <- selected_document()
      opening_telemetry$annotate(list(
        "copy.kind" = if (full_reading_document(document)) "full" else "feed"
      ))
      telemetry_span(
        "article.render",
        reader_document_ui(
          document,
          selected_id(),
          if (is.null(selected_orientation_provenance())) {
            "story_list"
          } else {
            "orientation"
          },
          open_id = opening_telemetry$id()
        )
      )
    })

    shiny::observeEvent(
      input$article_visible,
      {
        report <- input$article_visible
        if (!is.list(report)) {
          return(invisible(NULL))
        }
        opening_telemetry$complete(
          report$id,
          report$elapsed_ms,
          report$dom_ready_ms
        )
      },
      ignoreInit = TRUE
    )

    output$reader_agent_context <- shiny::renderUI({
      telemetry_local_span("article.context.render")
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
      reader_agent_status_ui(active_agent_run(), pending_reader_question())
    })
    shiny::outputOptions(
      output,
      "reader_agent_status",
      suspendWhenHidden = FALSE
    )

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
          title = paste(config$app_name, "subscriptions"),
          groups = store_list_groups(store, actor_id)$name
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
        } else {
          selected_orientation_theme(NULL)
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
        selected_folder(NULL)
        selected_group_ids(character())
        selected_ungrouped(FALSE)
        selected_feed(input$select_feed$id %||% NULL)
        selected_orientation_theme(NULL)
        record_event(
          "feed_filter",
          surface = "sidebar",
          payload = list(feed_id = selected_feed())
        )
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$select_group,
      {
        id <- input$select_group$id
        shiny::req(
          is.character(id),
          length(id) == 1L,
          id == "" || id %in% feed_groups()$group_id
        )
        clear_selection()
        selected_orientation_theme(NULL)
        selected_feed(NULL)
        selected_folder(NULL)
        selected_group_ids(if (nzchar(id)) id else character())
        selected_ungrouped(!nzchar(id))
        selected_group_match("any")
      },
      ignoreInit = TRUE
    )
    shiny::observeEvent(
      input$apply_reading_groups,
      {
        ids <- input$reading_groups %||% character()
        if (!length(ids)) {
          shiny::showNotification(
            "Choose at least one Group to read.",
            type = "warning"
          )
          return()
        }
        shiny::req(all(ids %in% feed_groups()$group_id))
        clear_selection()
        selected_orientation_theme(NULL)
        selected_feed(NULL)
        selected_folder(NULL)
        selected_group_ids(ids)
        selected_ungrouped(FALSE)
        selected_group_match(match.arg(
          input$reading_group_match %||% "any",
          c("any", "all")
        ))
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$select_folder,
      {
        folder <- input$select_folder$id
        shiny::req(
          is.character(folder),
          length(folder) == 1L,
          folder %in% feeds()$folder
        )
        clear_selection()
        selected_orientation_theme(NULL)
        selected_feed(NULL)
        selected_folder(folder)
        record_event("folder_filter", surface = "sidebar")
      },
      ignoreInit = TRUE
    )

    browse_orientation_queue <- function(theme = NULL) {
      clear_selection()
      selected_feed(NULL)
      selected_folder(NULL)
      selected_group_ids(character())
      selected_ungrouped(FALSE)
      selected_orientation_theme(theme)
      browse_queue_pending(!identical(input$view %||% "unread", "unread"))
      shiny::updateRadioButtons(session, "view", selected = "unread")
      bump_refresh()
      if (!isTRUE(browse_queue_pending())) {
        acknowledge_orientation_queue()
      }
    }

    shiny::observeEvent(
      input$browse_orientation_queue,
      {
        browse_orientation_queue()
      },
      ignoreInit = TRUE,
      priority = 100
    )

    shiny::observeEvent(
      input$browse_orientation_theme,
      {
        theme_id <- input$browse_orientation_theme$theme_id %||% NULL
        state <- shiny::isolate(orientation_session$state())
        themes <- Filter(
          \(theme) identical(theme$theme_id, theme_id),
          state$orientation$themes %||% list()
        )
        if (is.null(theme_id) || length(themes) != 1L) {
          shiny::showNotification(
            "That Orientation theme is no longer current.",
            type = "warning"
          )
          bump_refresh()
          return()
        }
        theme <- themes[[1L]]
        browse_orientation_queue(theme)
        record_event(
          "orientation_theme_opened",
          surface = "orientation",
          payload = list(
            theme_id = theme$theme_id,
            orientation_id = state$orientation$orientation_id,
            revision_id = state$orientation$revision_id,
            entry_count = length(theme$entry_ids)
          )
        )
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
        opening_telemetry$begin(request$telemetry_id, surface)
        opening_id <- opening_telemetry$id()
        session$onFlushed(
          \() opening_telemetry$flushed(opening_id),
          once = TRUE
        )
        opening_telemetry$activate()
        telemetry_local_span("article.selection")
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
                "Rill couldn't open that story. Try again."
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
        if (!identical(surface, "orientation")) {
          opened <- attempt_reader_action(
            function() {
              telemetry_span(
                "store.mark_opened",
                store_mark_opened(store, actor_id, entry_id)
              )
              TRUE
            },
            "Rill couldn't open that story. Try again."
          )
          if (is.null(opened)) {
            return()
          }
        }
        if (
          !identical(previous_id, entry_id) ||
            !identical(previous_document_id, pinned_document_id)
        ) {
          reset_reader_chat()
          reading_copy(NULL)
          cached_reading_copy <<- NULL
        }
        retain_entry(entry_id)
        selected_id(entry_id)
        selected_document_id(pinned_document_id)
        selected_orientation_provenance(provenance)
        selected_position(position)
        if (!identical(surface, "orientation")) {
          telemetry_span(
            "store.open_event",
            record_event(
              "entry_opened",
              entry_id = entry_id,
              surface = surface,
              position = selected_position(),
              payload = list()
            )
          )
        }
        session$sendCustomMessage(
          "rill-selection-accepted",
          list(surface = surface)
        )
        bump_entry_refresh(entry_id)
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
      input$show_orientation,
      {
        request <- input$show_orientation
        if (!is.list(request) || !store_scalar_string(request$request_id)) {
          return()
        }
        request_id <- request$request_id
        opened <- clear_selection(clear_retained = FALSE)
        if (isTRUE(opened)) {
          orientation_requested(TRUE)
          library_state_tick(shiny::isolate(library_state_tick()) + 1L)
        } else {
          shiny::showNotification(
            "Finish or stop the current response before opening Orientation.",
            type = "message",
            duration = 5
          )
        }
        session$onFlushed(
          function() {
            session$sendCustomMessage(
              "rill-reader-destination",
              list(
                destination = "orientation",
                request_id = request_id,
                ok = isTRUE(opened)
              )
            )
          },
          once = TRUE
        )
      },
      ignoreInit = TRUE
    )

    output$recent_answer_control <- shiny::renderUI({
      if (is.null(restored_question())) {
        return(NULL)
      }
      shiny::tags$button(
        type = "button",
        class = "btn-manage-feeds recent-answer-control",
        onclick = "rillReopenLastAnswer()",
        bsicons::bs_icon("chat-left-text"),
        "Reopen last answer"
      )
    })

    shiny::observeEvent(
      input$reopen_last_answer,
      {
        request <- input$reopen_last_answer
        if (!is.list(request) || !store_scalar_string(request$request_id)) {
          return()
        }
        request_id <- request$request_id
        saved <- restored_question()
        if (is.null(saved)) {
          session$sendCustomMessage(
            "rill-reader-destination",
            list(
              destination = "last_answer",
              request_id = request_id,
              ok = FALSE
            )
          )
          return()
        }
        if (reader_response_in_flight()) {
          shiny::showNotification(
            "Finish or stop the current response before reopening an answer.",
            type = "message",
            duration = 5
          )
          session$sendCustomMessage(
            "rill-reader-destination",
            list(
              destination = "last_answer",
              request_id = request_id,
              ok = FALSE
            )
          )
          return()
        }
        run <- store_get_agent_run(store, actor_id, saved$run_id)
        document <- if (!is.null(run)) {
          store_get_document_by_id(
            store,
            actor_id,
            run$pinned_inputs$document_id
          )
        }
        if (is.null(document)) {
          restored_question(NULL)
          shiny::showNotification(
            "That answer's reading copy is no longer available.",
            type = "message",
            duration = 5
          )
          session$sendCustomMessage(
            "rill-reader-destination",
            list(
              destination = "last_answer",
              request_id = request_id,
              ok = FALSE
            )
          )
          return()
        }
        if (!identical(active_agent_run()$run_id, run$run_id)) {
          reset_reader_chat()
          active_agent_run(run)
          if (nzchar(run$response_text %||% "")) {
            append_reader_chat(run$response_text, session)
          }
        }
        reading_copy(NULL)
        cached_reading_copy <<- NULL
        selected_orientation_provenance(NULL)
        selected_id(document$entry_id)
        selected_document_id(document$document_id)
        selected_position(NA_integer_)
        session$onFlushed(
          function() {
            session$sendCustomMessage(
              "rill-reader-destination",
              list(
                destination = "last_answer",
                request_id = request_id,
                ok = TRUE
              )
            )
          },
          once = TRUE
        )
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
            "Open a story first, then ask about it.",
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

        started <- tryCatch(
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
            NULL
          }
        )
        release_answer_promise(started)
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
          # shinychat keeps its composer disabled until a message arrives.
          append_reader_chat("Stopped before Rill started answering.", session)
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
            "The story copy this question used is no longer available.",
            type = "error"
          )
          return()
        }

        started <- tryCatch(
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
            NULL
          }
        )
        release_answer_promise(started)
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$toggle_star,
      {
        entry <- selected_entry()
        shiny::req(isTRUE(entry$library_access))
        value <- attempt_reader_action(
          \() store_toggle_state(store, actor_id, entry$entry_id, "starred"),
          "Rill couldn't star that story. Try again."
        )
        if (is.null(value)) {
          return()
        }
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
        changed <- attempt_reader_action(
          \() store_mark_unread(store, actor_id, entry$entry_id),
          "Rill couldn't mark that story unread. Try again."
        )
        if (is.null(changed)) {
          return()
        }
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

    reader_queue_server(
      store,
      actor_id,
      bump_entry_refresh,
      record_event,
      session,
      telemetry_enabled = config$telemetry_enabled
    )

    shiny::observeEvent(
      input$toggle_save,
      {
        entry <- selected_entry()
        shiny::req(isTRUE(entry$library_access))
        value <- attempt_reader_action(
          \() store_toggle_state(store, actor_id, entry$entry_id, "saved"),
          "Rill couldn't save that story. Try again."
        )
        if (is.null(value)) {
          return()
        }
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

    register_feed_actions(
      input,
      session,
      config,
      store,
      actor_id,
      selected_feed,
      selected_entry,
      clear_selection,
      status_kind,
      status_text,
      record_event,
      bump_refresh,
      feed_management,
      feed_refresh
    )

    shiny::observeEvent(
      input$preparation_details,
      {
        shiny::req(length(preparation_failures()))
        shiny::showModal(preparation_failures_ui(preparation_failures()))
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$prepare_article,
      {
        shiny::req(!is.null(selected_id()), is.null(selected_document_id()))
        entry <- selected_entry()
        shiny::req(isTRUE(entry$library_access))
        article_preparer$request(entry$entry_id, retry = TRUE)
        preparation_tick(preparation_tick() + 1L)
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$use_prepared_article,
      {
        shiny::req(!is.null(selected_id()), is.null(selected_document_id()))
        if (reader_response_in_flight()) {
          shiny::showNotification(
            "Wait for the current answer before loading a new copy.",
            type = "message"
          )
          return(invisible(NULL))
        }
        entry <- selected_entry()
        shiny::req(isTRUE(entry$library_access))
        previous <- selected_document()
        shiny::req(identical(previous$acquisition_method, "feed_fallback"))
        document <- public_reading_document(store, entry$entry_id)
        shiny::req(full_reading_document(document))
        current <- store_get_document(store, actor_id, entry$entry_id)
        if (
          !is.null(current) &&
            !identical(current$document_id, previous$document_id) &&
            !identical(current$document_id, document$document_id)
        ) {
          shiny::showNotification(
            "Your selected reading copy changed. Reopen the story to load it.",
            type = "message"
          )
          return(invisible(NULL))
        }
        store_replace_selected_document(
          store,
          actor_id,
          previous$document_id,
          document$document_id
        )
        current <- store_get_document(store, actor_id, entry$entry_id)
        shiny::req(
          !is.null(current),
          identical(current$document_id, document$document_id)
        )
        reading_copy(document)
        reset_reader_chat()
        bump_refresh()
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$prepare_today,
      {
        shiny::req(identical(input$view, "today"))
        entries <- tryCatch(
          store_list_entries(
            store,
            actor_id,
            view = "today",
            limit = 500L,
            now = calendar_window()$now,
            timezone = calendar_window()$timezone
          ),
          error = function(error) error
        )
        if (inherits(entries, "error")) {
          failure <- preparation_failure(entries, "library", config)
          preparation_failures(list(failure))
          status_kind("error")
          status_text("Rill couldn't start preparing today's full articles.")
          return(invisible(NULL))
        }
        preparation_failures(list())
        today_preparation_pending(TRUE)
        article_preparer$request(entries$entry_id, retry = TRUE)
        preparation_tick(preparation_tick() + 1L)
        status_kind("info")
        status_text(
          "Preparing today's articles in the background. Keep reading."
        )
        record_event(
          "today_preparation_requested",
          surface = "reading_queue",
          payload = list(total = nrow(entries))
        )
      },
      ignoreInit = TRUE
    )

    mark_scope_read <- function(before = NULL, reason) {
      marked <- attempt_reader_action(
        \() {
          store_mark_entries_read(
            store,
            actor_id,
            feed_id = selected_feed(),
            folder = selected_folder(),
            group_ids = selected_group_ids(),
            group_match = selected_group_match(),
            ungrouped = selected_ungrouped(),
            before = before,
            reason = reason
          )
        },
        "Rill couldn't mark those stories as read. Try again."
      )
      if (is.null(marked)) {
        return(invisible(NULL))
      }
      count <- length(marked)
      if (count) {
        retained_ids(setdiff(retained_ids(), marked))
        if (!is.null(selected_id()) && selected_id() %in% marked) {
          clear_selection(clear_retained = FALSE)
        }
        payload <- list(
          count = count,
          feed_id = selected_feed(),
          folder = selected_folder(),
          group_ids = selected_group_ids(),
          group_match = selected_group_match(),
          ungrouped = selected_ungrouped(),
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
      opening_telemetry$finish("disconnected")
      queue_telemetry$finish("disconnected")
      article_preparer$close()
      if (is.function(pending_reader_question_cancel)) {
        try(pending_reader_question_cancel(), silent = TRUE)
        pending_reader_question_cancel <<- NULL
      }
      if (is.function(deferred_reader_question_resume_cancel)) {
        try(deferred_reader_question_resume_cancel(), silent = TRUE)
        deferred_reader_question_resume_cancel <<- NULL
      }
      orientation_session$close()
      if (is.function(visible_agent_run_poll_cancel)) {
        try(visible_agent_run_poll_cancel(), silent = TRUE)
        visible_agent_run_poll_cancel <<- NULL
      }
    })

    if (isTRUE(config$refresh_on_start)) {
      session$onFlushed(function() feed_refresh$start(), once = TRUE)
    }
  }
}
