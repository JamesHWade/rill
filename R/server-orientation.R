orientation_server <- function(
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
) {
  orientation_preparing <- shiny::reactiveVal(FALSE)
  orientation_failure <- shiny::reactiveVal(NULL)
  orientation_destination_tick <- shiny::reactiveVal(0L)
  orientation_destination_poll_token <- NULL
  orientation_poll_token <- NULL
  orientation_retry_cancel <- NULL

  schedule_orientation_retry <- function(delay = 2) {
    if (is.function(orientation_retry_cancel)) {
      return(invisible(NULL))
    }
    orientation_retry_cancel <<- later::later(
      rill_session_callback(session, function() {
        orientation_retry_cancel <<- NULL
        if (!session$isClosed()) {
          orientation_attempted_boundary(NULL)
          bump_refresh()
        }
        NULL
      }),
      delay = delay
    )
    invisible(NULL)
  }

  orientation_destination_status <- shiny::reactive({
    orientation_destination_tick()
    state <- orientation_destination_state(store, actor_id, config)
    orientation_destination_poll_token <<-
      orientation_destination_state_token(state)
    state
  })

  orientation_display_failure <- shiny::reactive({
    if (isTRUE(orientation_destination_status()$enabled)) {
      orientation_failure()
    }
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
    shiny::invalidateLater(rill_session_poll_interval_ms, session)
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
    library_state_tick()
    polled <- store_orientation_polled_state(store, actor_id)
    orientation_poll_token <<- polled$token
    polled$state
  })

  shiny::observe({
    shiny::invalidateLater(rill_session_poll_interval_ms, session)
    token <- tryCatch(
      store_orientation_poll_token(store, actor_id),
      error = \(error) NULL
    )
    if (is.null(token)) {
      return()
    }
    previous <- orientation_poll_token
    orientation_poll_token <<- token
    if (!is.null(previous) && !identical(token, previous)) {
      bump_refresh()
    }
  })

  start_orientation_maintenance <- function(
    retry_id = orientation_automatic_retry_id()
  ) {
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
      orientation_failure(prepared)
      telemetry_log(
        "warn",
        "orientation.preparation_failed",
        list("error.type" = class(prepared)[[1L]])
      )
      return(invisible(NULL))
    }
    state <- orientation_status(store, actor_id)
    boundary_hash <- state$boundary$hash
    if (!isTRUE(state$due)) {
      orientation_failure(NULL)
      return(invisible(NULL))
    }
    if (identical(orientation_attempted_boundary(), boundary_hash)) {
      return(invisible(NULL))
    }

    orientation_attempted_boundary(boundary_hash)
    orientation_preparing(TRUE)
    orientation_failure(NULL)
    span <- telemetry_start(
      "orientation.maintain",
      list(
        "orientation.candidate_count" = state$boundary$candidate_count,
        "orientation.explicit_retry" = !identical(
          retry_id,
          orientation_automatic_retry_id()
        )
      )
    )
    finish_trace <- telemetry_finalizer(span)
    diagnostics <- orientation_diagnostics()
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
        retry_id = retry_id,
        diagnostics = diagnostics
      ),
      error = \(error) error
    )
    if (inherits(control, "error")) {
      orientation_failure(diagnostics$error() %||% control)
      finish_trace(
        "error",
        c(
          list("orientation.outcome" = "start_failed"),
          diagnostics$attributes(control)
        )
      )
      orientation_preparing(FALSE)
      telemetry_log(
        "warn",
        "orientation.start_failed",
        diagnostics$attributes(control)
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
      if (control$status %in% c("failed", "cancelled", "interrupted")) {
        orientation_failure(orientation_failure_message(
          control$run$terminal_reason %||% control$status
        ))
      }
      finish_trace(
        attributes = list(
          "orientation.outcome" = control$status,
          "orientation.execution_started" = FALSE
        )
      )
      orientation_preparing(FALSE)
      orientation_control(NULL)
      if (control$status %in% c("busy", "running")) {
        schedule_orientation_retry()
      } else if (identical(control$status, "boundary_changed")) {
        schedule_orientation_retry(delay = 0)
      }
      return(invisible(control))
    }

    settle <- function(value = NULL, error = NULL) {
      run_status <- value$run$status %||% "stopped"
      outcome <- if (!is.null(error)) {
        "failed"
      } else if (run_status %in% c("failed", "cancelled", "interrupted")) {
        run_status
      } else if (
        identical(run_status, "completed") &&
          identical(value$run$run_id, control$run$run_id) &&
          identical(value$orientation$agent_run_id, control$run$run_id)
      ) {
        "published"
      } else {
        "stopped"
      }
      finish_trace(
        status = if (identical(outcome, "failed")) "error" else "ok",
        attributes = c(
          list(
            "orientation.outcome" = outcome,
            "orientation.execution_started" = TRUE,
            "orientation.card_count" = if (identical(outcome, "published")) {
              length(value$orientation$cards)
            }
          ),
          diagnostics$attributes(error)
        )
      )
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
      if (outcome %in% c("failed", "cancelled", "interrupted")) {
        orientation_failure(
          diagnostics$error() %||%
            error %||%
            orientation_failure_message(
              value$run$terminal_reason %||% outcome
            )
        )
      } else {
        orientation_failure(NULL)
      }
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
          diagnostics$attributes(error)
        )
        settle(NULL, error = error)
      }
    )
    invisible(control)
  }

  retry_orientation <- function() {
    if (is.null(shiny::isolate(orientation_failure()))) {
      return(invisible(NULL))
    }
    orientation_attempted_boundary(NULL)
    start_orientation_maintenance(
      retry_id = rill_id(
        "orientation-reader-retry",
        session_id,
        utc_now(),
        stats::runif(1)
      )
    )
  }
  shiny::observeEvent(
    input$retry_orientation,
    retry_orientation(),
    ignoreInit = TRUE
  )
  shiny::observeEvent(
    input$retry_orientation_queue,
    retry_orientation(),
    ignoreInit = TRUE
  )

  if (!isTRUE(config$demo_mode)) {
    shiny::observe({
      if (!is.null(selected_id())) {
        return()
      }
      destination <- orientation_destination_status()
      orientation_state()
      if (isTRUE(destination$enabled)) {
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
          "Orientation is off for this installation.",
          type = "warning"
        )
        return()
      }
      if (isTRUE(state$needs_configuration)) {
        message <- if (isTRUE(state$needs_endpoint_configuration)) {
          paste(
            "Set RILL_AGENT_BASE_URL to the model's endpoint before turning",
            "on Orientation."
          )
        } else {
          paste(
            "Set RILL_AGENT_POLICY_URL to the provider's data policy before",
            "turning on Orientation."
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

  list(
    state = orientation_state,
    destination = orientation_destination_status,
    preparing = orientation_preparing,
    display_failure = orientation_display_failure,
    failure = orientation_failure,
    close = function() {
      if (is.function(orientation_retry_cancel)) {
        try(orientation_retry_cancel(), silent = TRUE)
        orientation_retry_cancel <<- NULL
      }
    }
  )
}
