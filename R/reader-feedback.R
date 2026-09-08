feedback_require_reader <- function(store, reader_id) {
  if (
    !store_scalar_string(reader_id) ||
      !identical(store_resolve_reader(store, reader_id)$status, "active")
  ) {
    cli::cli_abort(
      "Feedback requires an active Reader.",
      class = "rill_feedback_invalid"
    )
  }
}

feedback_reasons <- function() {
  c(
    "Relevance" = "relevance",
    "Usefulness" = "usefulness",
    "Source faithfulness" = "source_faithfulness",
    "Clarity" = "clarity",
    "Missing context" = "missing_context",
    "Unwanted actions" = "unwanted_actions",
    "Following my request" = "instruction_following",
    "Slow or failed execution" = "execution"
  )
}

feedback_question_runs <- function(store, reader_id) {
  feedback_require_reader(store, reader_id)
  if (identical(store$mode, "postgres")) {
    rows <- DBI::dbGetQuery(
      store$pool,
      paste(
        "SELECT * FROM agent_runs WHERE reader_id = $1 AND kind = 'question'",
        "AND status IN ('completed', 'failed', 'interrupted', 'cancelled')",
        "ORDER BY requested_at DESC, run_id LIMIT 50"
      ),
      params = list(reader_id)
    )
    return(lapply(seq_len(nrow(rows)), function(i) {
      agent_run_from_row(rows[i, , drop = FALSE])
    }))
  }
  runs <- Filter(
    function(run) {
      identical(run$reader_id, reader_id) &&
        identical(run$kind, "question") &&
        run$status %in% c("completed", "failed", "interrupted", "cancelled")
    },
    store$memory$agent_runs
  )
  requested <- vapply(
    runs,
    function(run) {
      as.numeric(as.POSIXct(run$requested_at, tz = "UTC"))
    },
    numeric(1)
  )
  ids <- vapply(runs, `[[`, character(1), "run_id")
  utils::head(runs[order(-requested, ids)], 50L)
}

feedback_target <- function(store, reader_id, kind, source) {
  feedback_require_reader(store, reader_id)
  if (is.null(source) || !identical(source$reader_id, reader_id)) {
    cli::cli_abort(
      "This output is unavailable.",
      class = "rill_feedback_invalid"
    )
  }
  if (identical(kind, "orientation")) {
    run <- store_get_agent_run(store, reader_id, source$agent_run_id)
    source_id <- source$revision_id
    output <- list(
      question = source$question,
      introduction = source$introduction,
      status = source$status,
      cards = source$cards
    )
  } else {
    run <- source
    if (
      !identical(run$kind, "question") ||
        !run$status %in% c("completed", "failed", "interrupted", "cancelled")
    ) {
      cli::cli_abort(
        "Wait until this response finishes.",
        class = "rill_feedback_invalid"
      )
    }
    source_id <- run$run_id
    output <- list(
      question = run$pinned_inputs$question,
      response = if (store_scalar_string(run$response_text)) {
        run$response_text
      } else if (store_scalar_string(run$partial_response)) {
        run$partial_response
      },
      response_state = if (store_scalar_string(run$response_text)) {
        "complete"
      } else if (store_scalar_string(run$partial_response)) {
        "partial"
      } else {
        "unavailable"
      },
      status = run$status
    )
  }
  inputs <- run$pinned_inputs %||% list()
  provenance <- inputs[intersect(
    names(inputs),
    c(
      "model",
      "policy_version",
      "tool_versions",
      "document_content_hash",
      "document_record_hash",
      "candidate_document_ids",
      "candidates",
      "document_id",
      "document_ids",
      "entry_id",
      "entry_ids",
      "content_hash",
      "content_hashes",
      "record_hash",
      "record_hashes"
    )
  )]
  snapshot <- list(
    reader_id = reader_id,
    kind = kind,
    source_id = source_id,
    run_id = run$run_id %||% source$agent_run_id,
    output = output,
    provenance = provenance
  )
  snapshot <- canonicalize_json_value(jsonlite::fromJSON(
    orientation_json(snapshot),
    simplifyVector = FALSE
  ))
  list(
    target_id = rill_id("feedback", canonical_json(snapshot)),
    snapshot = snapshot
  )
}

feedback_save <- function(
  store,
  reader_id,
  target,
  rating,
  reasons = character(),
  comment = ""
) {
  feedback_require_reader(store, reader_id)
  if (
    !identical(target$snapshot$reader_id, reader_id) ||
      !identical(
        target$target_id,
        rill_id("feedback", canonical_json(target$snapshot))
      )
  ) {
    cli::cli_abort(
      "This feedback belongs to a different output.",
      class = "rill_feedback_invalid"
    )
  }
  rating <- rlang::arg_match(rating, c("helpful", "not_helpful"))
  if (
    !is.character(reasons) ||
      anyNA(reasons) ||
      !all(reasons %in% unname(feedback_reasons())) ||
      !is.character(comment) ||
      length(comment) != 1L ||
      is.na(comment) ||
      nchar(comment) > 2000L
  ) {
    cli::cli_abort(
      "Choose listed reasons and keep comments within 2,000 characters.",
      class = "rill_feedback_invalid"
    )
  }
  existing <- store_list_reader_feedback(store, reader_id)[[target$target_id]]
  record <- c(
    target,
    list(
      rating = rating,
      reasons = unique(reasons),
      comment = comment,
      created_at = existing$created_at %||% utc_now(),
      updated_at = utc_now()
    )
  )
  if (identical(store$mode, "postgres")) {
    DBI::dbExecute(
      store$pool,
      paste(
        "INSERT INTO reader_feedback (reader_id, target_id, record) VALUES ($1, $2, $3::jsonb)",
        "ON CONFLICT (reader_id, target_id) DO UPDATE SET record =",
        "jsonb_set(EXCLUDED.record, '{created_at}', reader_feedback.record->'created_at')"
      ),
      params = list(reader_id, target$target_id, orientation_json(record))
    )
  } else {
    store$memory$reader_feedback[[reader_id]][[target$target_id]] <- record
  }
  invisible(record)
}

#' List a Reader's private output feedback
#'
#' Returns explicit ratings and their retained output snapshots for review or
#' export by an authorized operator. Set `DATABASE_URL` to the Library's
#' PostgreSQL connection string. This privileged operation does not authenticate
#' a Reader; obtain their permission before sharing their private feedback.
#' It does not aggregate across Readers or change agent behavior. Treat exported
#' text as private, untrusted content.
#'
#' @param reader_id The owning Reader's identifier.
#' @return A named list of feedback records, keyed by output target identifier.
#' @export
list_reader_feedback <- function(reader_id) {
  if (!store_scalar_string(reader_id)) {
    cli::cli_abort(
      "Choose one Reader identifier.",
      class = "rill_feedback_invalid"
    )
  }
  config <- rill_config()
  if (isTRUE(config$demo_mode)) {
    cli::cli_abort(
      "Set DATABASE_URL to the Reader's durable Library before reviewing feedback.",
      class = "rill_feedback_store_required"
    )
  }
  store <- rill_store(config)
  on.exit(rill_store_close(store), add = TRUE)
  store_list_reader_feedback(store, reader_id)
}

store_list_reader_feedback <- function(store, reader_id) {
  feedback_require_reader(store, reader_id)
  if (identical(store$mode, "postgres")) {
    rows <- DBI::dbGetQuery(
      store$pool,
      "SELECT target_id, record FROM reader_feedback WHERE reader_id = $1 ORDER BY target_id",
      params = list(reader_id)
    )
    return(stats::setNames(
      lapply(rows$record, function(record) {
        canonicalize_json_value(jsonlite::fromJSON(
          record,
          simplifyVector = FALSE
        ))
      }),
      rows$target_id
    ))
  }
  store$memory$reader_feedback[[reader_id]] %||% list()
}

feedback_withdraw <- function(store, reader_id, target_id) {
  feedback_require_reader(store, reader_id)
  if (!store_scalar_string(target_id)) {
    cli::cli_abort(
      "Choose feedback to withdraw.",
      class = "rill_feedback_invalid"
    )
  }
  if (identical(store$mode, "postgres")) {
    DBI::dbExecute(
      store$pool,
      "DELETE FROM reader_feedback WHERE reader_id = $1 AND target_id = $2",
      params = list(reader_id, target_id)
    )
  } else {
    store$memory$reader_feedback[[reader_id]][[target_id]] <- NULL
  }
  invisible(NULL)
}

feedback_output_ui <- function(output) {
  shiny::tagList(
    if (store_scalar_string(output$question)) {
      shiny::tags$p(shiny::tags$strong(output$question))
    },
    if (identical(output$response_state, "partial")) {
      shiny::tags$p(
        "This attempt did not finish. The retained partial response is shown below."
      )
    },
    if (identical(output$response_state, "unavailable")) {
      shiny::tags$p(
        "No answer text was retained for this attempt. You can rate its execution."
      )
    },
    if (store_scalar_string(output$introduction)) {
      shiny::tags$p(output$introduction)
    },
    if (store_scalar_string(output$response)) {
      shiny::tags$pre(
        style = "white-space:pre-wrap",
        output$response
      )
    },
    lapply(output$cards, function(card) {
      shiny::tags$div(
        if (store_scalar_string(card$interpretation)) {
          shiny::tags$p(card$interpretation)
        },
        if (store_scalar_string(card$why_now)) shiny::tags$p(card$why_now),
        if (store_scalar_string(card$evidence)) {
          shiny::tags$blockquote(card$evidence)
        }
      )
    }),
    if (store_scalar_string(output$status)) shiny::tags$p(output$status)
  )
}

feedback_dialog <- function(target, existing = NULL, return_focus = NULL) {
  modal <- shiny::modalDialog(
    title = "Rate this Rill output",
    easyClose = TRUE,
    shiny::tags$p(
      "Your rating is private. Saving retains this output for review; it does not change Rill's behavior."
    ),
    shiny::tags$details(
      shiny::tags$summary("Review the exact output being rated"),
      shiny::tags$div(
        style = "max-height:16rem;overflow:auto",
        feedback_output_ui(target$snapshot$output)
      )
    ),
    shiny::radioButtons(
      "feedback_rating",
      "Was this helpful?",
      c("Helpful" = "helpful", "Not helpful" = "not_helpful"),
      selected = existing$rating %||% character()
    ),
    shiny::tags$details(
      open = if (length(existing$reasons) || nzchar(existing$comment %||% "")) {
        NA
      } else {
        NULL
      },
      shiny::tags$summary("Add reasons or a comment (optional)"),
      shiny::checkboxGroupInput(
        "feedback_reasons",
        "Optional reasons",
        feedback_reasons(),
        selected = unlist(existing$reasons)
      ),
      shiny::textAreaInput(
        "feedback_comment",
        "Optional comment (up to 2,000 characters)",
        value = existing$comment %||% "",
        width = "100%"
      )
    ),
    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      if (!is.null(existing)) {
        shiny::actionButton("feedback_withdraw", "Withdraw rating")
      },
      shiny::actionButton("feedback_save", "Save rating", class = "btn-primary")
    )
  )
  htmltools::tagAppendAttributes(modal, `data-rill-return-focus` = return_focus)
}

reader_feedback_server <- function(store, reader_id, active_run, session) {
  input <- session$input
  output <- session$output
  feedback_pending <- shiny::reactiveVal(NULL)
  feedback_visible_orientation <- NULL
  feedback_return_focus <- NULL
  feedback_open <- function(target) {
    existing <- store_list_reader_feedback(store, reader_id)[[target$target_id]]
    feedback_pending(target)
    shiny::showModal(feedback_dialog(target, existing, feedback_return_focus))
  }
  feedback_attempt <- function(action) {
    tryCatch(action(), error = function(error) {
      shiny::showNotification(
        if (inherits(error, "rill_feedback_invalid")) {
          conditionMessage(error)
        } else {
          "Could not update feedback. Please try again."
        },
        type = "error"
      )
    })
  }
  output$reader_feedback_actions <- shiny::renderUI({
    run <- active_run()
    shiny::tagList(
      if (
        !is.null(run) &&
          run$status %in% c("completed", "failed", "interrupted", "cancelled")
      ) {
        shiny::actionButton("rate_response", "Rate this response")
      },
      shiny::actionLink(
        "choose_feedback_response",
        "Rate an earlier response"
      ),
      shiny::actionButton("review_feedback", "My saved ratings")
    )
  })
  shiny::observeEvent(input$rate_response, {
    feedback_return_focus <<- "rate_response"
    feedback_attempt(function() {
      feedback_open(feedback_target(
        store,
        reader_id,
        "question",
        active_run()
      ))
    })
  })
  shiny::observeEvent(input$choose_feedback_response, {
    feedback_return_focus <<- "choose_feedback_response"
    feedback_attempt(function() {
      runs <- feedback_question_runs(store, reader_id)
      labels <- vapply(
        runs,
        function(run) {
          paste(
            substr(run$pinned_inputs$question %||% "Response", 1L, 100L),
            run$status,
            format(run$requested_at),
            sep = " \u00b7 "
          )
        },
        character(1)
      )
      ids <- vapply(runs, `[[`, character(1), "run_id")
      shiny::showModal(shiny::modalDialog(
        title = "Rate an earlier response",
        easyClose = TRUE,
        if (length(runs)) {
          shiny::selectInput(
            "feedback_response_id",
            "Choose from your last 50 finished attempts",
            stats::setNames(ids, labels)
          )
        } else {
          shiny::tags$p("There are no finished responses to rate yet.")
        },
        footer = shiny::tagList(
          shiny::modalButton("Cancel"),
          if (length(runs)) {
            shiny::actionButton(
              "open_feedback_response",
              "Review this response"
            )
          }
        )
      ))
    })
  })
  shiny::observeEvent(input$open_feedback_response, {
    feedback_attempt(function() {
      run <- store_get_agent_run(store, reader_id, input$feedback_response_id)
      feedback_open(feedback_target(store, reader_id, "question", run))
    })
  })
  shiny::observeEvent(input$rate_orientation, {
    shiny::req(feedback_visible_orientation)
    feedback_return_focus <<- "rate_orientation"
    feedback_attempt(function() {
      feedback_open(feedback_target(
        store,
        reader_id,
        "orientation",
        feedback_visible_orientation
      ))
    })
  })
  shiny::observeEvent(input$feedback_save, {
    shiny::req(feedback_pending())
    if (!store_scalar_string(input$feedback_rating)) {
      shiny::showNotification(
        "Choose Helpful or Not helpful before saving.",
        type = "warning"
      )
      return()
    }
    feedback_attempt(function() {
      feedback_save(
        store,
        reader_id,
        feedback_pending(),
        input$feedback_rating,
        input$feedback_reasons %||% character(),
        input$feedback_comment %||% ""
      )
      feedback_pending(NULL)
      shiny::removeModal()
      shiny::showNotification("Rating saved privately.", type = "message")
    })
  })
  shiny::observeEvent(input$feedback_withdraw, {
    shiny::req(feedback_pending())
    feedback_attempt(function() {
      feedback_withdraw(store, reader_id, feedback_pending()$target_id)
      feedback_pending(NULL)
      shiny::removeModal()
      shiny::showNotification(
        "Rating and retained output removed.",
        type = "message"
      )
    })
  })
  output$download_feedback <- shiny::downloadHandler(
    filename = function() "rill-private-feedback.json",
    content = function(file) {
      writeLines(
        orientation_json(store_list_reader_feedback(store, reader_id)),
        file,
        useBytes = TRUE
      )
    },
    contentType = "application/json"
  )
  shiny::observeEvent(input$review_feedback, {
    feedback_return_focus <<- "review_feedback"
    feedback_attempt(function() {
      records <- store_list_reader_feedback(store, reader_id)
      labels <- vapply(
        records,
        function(record) {
          paste(
            if (identical(record$snapshot$kind, "orientation")) {
              "Orientation"
            } else {
              "Ask Rill"
            },
            if (identical(record$rating, "helpful")) {
              "Helpful"
            } else {
              "Not helpful"
            },
            record$created_at,
            substr(record$snapshot$output$question %||% "", 1L, 80L),
            sep = " \u00b7 "
          )
        },
        character(1)
      )
      shiny::showModal(shiny::modalDialog(
        title = "My saved ratings",
        easyClose = TRUE,
        shiny::downloadButton("download_feedback", "Download my ratings"),
        if (length(records)) {
          shiny::selectInput(
            "saved_feedback_id",
            "Choose an output to review",
            stats::setNames(names(records), labels)
          )
        } else {
          shiny::tags$p("You have no saved ratings.")
        },
        footer = shiny::tagList(
          shiny::modalButton("Close"),
          if (length(records)) {
            shiny::actionButton("edit_feedback", "Review rating")
          }
        )
      ))
    })
  })
  shiny::observeEvent(input$edit_feedback, {
    feedback_attempt(function() {
      record <- store_list_reader_feedback(store, reader_id)[[
        input$saved_feedback_id
      ]]
      shiny::req(record)
      feedback_open(record[c("target_id", "snapshot")])
    })
  })
  list(
    pending = feedback_pending,
    set_orientation = function(orientation, candidates) {
      feedback_visible_orientation <<- orientation
      if (!is.null(feedback_visible_orientation)) {
        visible_ids <- vapply(
          candidates,
          function(candidate) {
            candidate$document$document_id %||% ""
          },
          character(1)
        )
        feedback_visible_orientation$cards <<- Filter(
          function(card) {
            card$document_id %in% visible_ids
          },
          feedback_visible_orientation$cards
        )
        if (!length(feedback_visible_orientation$cards)) {
          feedback_visible_orientation <<- NULL
        }
      }
      invisible(NULL)
    }
  )
}
