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

feedback_question_ready <- function(run) {
  !is.null(run) &&
    run$status %in% c("completed", "failed", "interrupted", "cancelled") &&
    !completed_response_may_arrive(run)
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
    return(Filter(
      feedback_question_ready,
      lapply(seq_len(nrow(rows)), function(i) {
        agent_run_from_row(rows[i, , drop = FALSE])
      })
    ))
  }
  runs <- Filter(
    function(run) {
      identical(run$reader_id, reader_id) &&
        identical(run$kind, "question") &&
        feedback_question_ready(run)
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
      cards = source$cards,
      themes = source$themes %||% list()
    )
  } else {
    run <- source
    if (
      !identical(run$kind, "question") ||
        !feedback_question_ready(run)
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

feedback_action_button <- function(input_id, label, target) {
  shiny::tags$button(
    id = input_id,
    type = "button",
    class = "btn btn-default",
    onclick = sprintf(
      "Shiny.setInputValue(%s, %s, {priority: 'event'})",
      jsonlite::toJSON(input_id, auto_unbox = TRUE),
      jsonlite::toJSON(target, auto_unbox = TRUE)
    ),
    label
  )
}

feedback_source_metadata <- function(candidate) {
  document <- candidate$document
  entry <- candidate$entry
  list(
    document_id = document$document_id,
    title = document$title %||% entry$title,
    site = document$site %||% entry$feed_title,
    published_at = entry$published_at,
    original_url = rill_document_original_source_url(document),
    acquisition_method = document$acquisition_method,
    producer = document$producer,
    captured_at = document$captured_at,
    limitations = rill_document_limitations(document),
    content_hash = document$content_hash,
    record_hash = document$record_hash
  )
}

feedback_source_ui <- function(source) {
  if (is.null(source)) {
    return(NULL)
  }
  labels <- c(
    title = "Title",
    site = "Site",
    published_at = "Published at",
    original_url = "Original Source",
    document_id = "Document",
    acquisition_method = "Acquisition",
    producer = "Prepared by",
    captured_at = "Captured at",
    limitations = "Limitations"
  )
  shiny::tags$div(
    style = "overflow-wrap:anywhere",
    shiny::tags$p(shiny::tags$strong("Source Document")),
    shiny::tags$dl(lapply(names(labels), function(field) {
      if (store_scalar_string(source[[field]])) {
        shiny::tagList(
          shiny::tags$dt(labels[[field]]),
          shiny::tags$dd(source[[field]])
        )
      }
    }))
  )
}

feedback_answer_ui <- function(response) {
  rendered <- sanitize_rendered_html(commonmark::markdown_html(
    response,
    extensions = c("table", "strikethrough", "autolink", "tagfilter")
  ))
  parsed <- xml2::read_html(paste0(
    "<div id='feedback-answer'>",
    rendered,
    "</div>"
  ))
  root <- xml2::xml_find_first(parsed, "//*[@id='feedback-answer']")
  xml2::xml_remove(xml2::xml_find_all(
    root,
    ".//img | .//iframe | .//video | .//audio"
  ))
  prose_tags <- c(
    "p",
    "h1",
    "h2",
    "h3",
    "h4",
    "h5",
    "h6",
    "em",
    "strong",
    "code",
    "pre",
    "blockquote",
    "ul",
    "ol",
    "li",
    "a",
    "hr",
    "br",
    "table",
    "thead",
    "tbody",
    "tr",
    "th",
    "td",
    "del",
    "span"
  )
  for (node in xml2::xml_find_all(root, ".//*")) {
    tag <- xml2::xml_name(node)
    if (!tag %in% prose_tags) {
      xml2::xml_set_name(node, "span")
    }
    keep <- if (identical(tag, "a")) {
      c("href", "title")
    } else if (identical(tag, "ol")) {
      "start"
    } else {
      character()
    }
    for (attribute in setdiff(names(xml2::xml_attrs(node)), keep)) {
      xml2::xml_set_attr(node, attribute, NULL)
    }
  }
  xml2::xml_set_attr(root, "id", NULL)
  for (table in xml2::xml_find_all(root, ".//table")) {
    xml2::xml_set_attr(table, "tabindex", "0")
    xml2::xml_set_attr(table, "aria-label", "Retained answer table")
  }
  shiny::tagList(
    shiny::tags$div(
      class = "feedback-answer",
      htmltools::HTML(as.character(root))
    ),
    shiny::tags$details(
      shiny::tags$summary("Original answer text"),
      rill_copy_text_ui("Copy original answer"),
      shiny::tags$pre(
        class = "feedback-original",
        shiny::tags$code(response),
        .noWS = "inside"
      )
    )
  )
}

feedback_saved_records <- function(records) {
  if (!length(records)) {
    return(records)
  }
  dates <- vapply(
    records,
    function(record) as.character(record$created_at),
    character(1)
  )
  records[order(dates, names(records), decreasing = TRUE)]
}

feedback_saved_choices <- function(records) {
  questions <- vapply(
    records,
    function(record) {
      record$snapshot$output$question %||% record$snapshot$kind
    },
    character(1)
  )
  repeated <- duplicated(questions) | duplicated(questions, fromLast = TRUE)
  lapply(seq_along(records), function(index) {
    record <- records[[index]]
    output <- record$snapshot$output
    kind <- if (identical(record$snapshot$kind, "orientation")) {
      "Orientation"
    } else {
      "Ask Rill"
    }
    rating <- if (identical(record$rating, "helpful")) {
      "Helpful"
    } else {
      "Not helpful"
    }
    shiny::tags$span(
      class = "feedback-saved-choice",
      shiny::tags$strong(output$question %||% kind),
      shiny::tags$span(
        class = "feedback-saved-meta",
        paste(kind, rating, record$created_at, sep = " \u00b7 "),
        if (repeated[[index]]) paste0(" \u00b7 Saved output ", index)
      )
    )
  })
}

feedback_output_ui <- function(output) {
  shiny::tagList(
    if (store_scalar_string(output$question)) {
      shiny::tags$p(shiny::tags$strong("Question: "), output$question)
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
      shiny::tags$p(
        shiny::tags$strong("Rill introduction: "),
        output$introduction
      )
    },
    if (store_scalar_string(output$response)) {
      feedback_answer_ui(output$response)
    },
    lapply(output$cards, function(card) {
      shiny::tags$div(
        feedback_source_ui(card$source),
        if (store_scalar_string(card$interpretation)) {
          shiny::tags$p(
            shiny::tags$strong("Rill interpretation: "),
            card$interpretation
          )
        },
        if (store_scalar_string(card$why_now)) {
          shiny::tags$p(shiny::tags$strong("Why now (Rill): "), card$why_now)
        },
        if (store_scalar_string(card$evidence)) {
          shiny::tagList(
            shiny::tags$p(shiny::tags$strong("Source evidence")),
            shiny::tags$blockquote(card$evidence)
          )
        }
      )
    }),
    lapply(output$themes, function(theme) {
      shiny::tags$div(
        shiny::tags$p(shiny::tags$strong("Theme (Rill): "), theme$name),
        shiny::tags$p(theme$note),
        shiny::tags$p(paste(
          length(theme$entry_ids),
          if (length(theme$entry_ids) == 1L) {
            "unread story"
          } else {
            "unread stories"
          }
        )),
        lapply(theme$sources, feedback_source_ui)
      )
    }),
    if (store_scalar_string(output$status)) shiny::tags$p(output$status)
  )
}

feedback_dialog <- function(target, existing = NULL, return_focus = NULL) {
  modal <- shiny::modalDialog(
    title = "Rate this Rill output",
    size = "l",
    easyClose = TRUE,
    shiny::tags$p(
      "Your rating is private. Saving retains this output for review; it does not change Rill's behavior."
    ),
    shiny::radioButtons(
      "feedback_rating",
      "Was this helpful?",
      c("Helpful" = "helpful", "Not helpful" = "not_helpful"),
      selected = existing$rating %||% character()
    ),
    shiny::tags$details(
      open = NA,
      shiny::tags$summary("Review the exact output being rated"),
      shiny::tags$div(
        class = "feedback-output",
        tabindex = "0",
        role = "region",
        `aria-label` = "Output being rated",
        feedback_output_ui(target$snapshot$output)
      )
    ),
    shiny::tags$details(
      shiny::tags$summary(
        if (length(existing$reasons) || nzchar(existing$comment %||% "")) {
          "Edit saved reasons or comment (optional)"
        } else {
          "Add reasons or a comment (optional)"
        }
      ),
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
  feedback_visible_token <- NULL
  feedback_partials <- list()
  question_target <- function(run) {
    if (
      !is.null(run) &&
        !store_scalar_string(run$response_text) &&
        !store_scalar_string(run$partial_response)
    ) {
      run$partial_response <- feedback_partials[[run$run_id]]
    }
    feedback_target(store, reader_id, "question", run)
  }
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
    if (!is.null(run) && completed_response_may_arrive(run)) {
      shiny::invalidateLater(250, session)
    }
    shiny::tagList(
      if (feedback_question_ready(run)) {
        feedback_action_button(
          "rate_response",
          "Rate this response",
          run$run_id
        )
      },
      shiny::actionLink(
        "choose_feedback_response",
        "Rate an earlier response"
      ),
      shiny::actionButton("review_feedback", "My saved ratings")
    )
  })
  shiny::observeEvent(input$rate_response, {
    run <- active_run()
    if (is.null(run) || !identical(input$rate_response, run$run_id)) {
      feedback_pending(NULL)
      shiny::showNotification(
        "The response changed. Review the current response or choose an earlier attempt.",
        type = "message"
      )
      return(invisible(NULL))
    }
    feedback_return_focus <<- "rate_response"
    feedback_attempt(function() {
      feedback_open(question_target(run))
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
      feedback_open(question_target(run))
    })
  })
  shiny::observeEvent(input$rate_orientation, {
    shiny::req(feedback_visible_orientation)
    if (!identical(input$rate_orientation, feedback_visible_token)) {
      feedback_pending(NULL)
      shiny::showNotification(
        "Orientation changed. Review the current selection and try again.",
        type = "message"
      )
      return(invisible(NULL))
    }
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
      records <- feedback_saved_records(records)
      selected <- shiny::isolate(input$saved_feedback_id)
      if (!length(selected) || !selected %in% names(records)) {
        selected <- names(records)[1L]
      }
      shiny::showModal(shiny::modalDialog(
        title = "My saved ratings",
        size = "l",
        easyClose = TRUE,
        shiny::downloadButton("download_feedback", "Download my ratings"),
        if (length(records)) {
          shiny::radioButtons(
            "saved_feedback_id",
            "Choose an output to review",
            choiceNames = feedback_saved_choices(records),
            choiceValues = names(records),
            selected = selected,
            width = "100%"
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
    remember_partial = function(run_id, partial) {
      feedback_partials[[run_id]] <<- NULL
      if (store_scalar_string(partial)) {
        feedback_partials[[run_id]] <<- partial
        feedback_partials <<- utils::tail(feedback_partials, 50L)
      }
      invisible(NULL)
    },
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
        feedback_visible_orientation$cards <<- lapply(
          feedback_visible_orientation$cards,
          function(card) {
            candidate <- candidates[[match(card$document_id, visible_ids)]]
            card$source <- feedback_source_metadata(candidate)
            card
          }
        )
        feedback_visible_orientation$introduction <<- NULL
        if (!length(feedback_visible_orientation$cards)) {
          feedback_visible_orientation$question <<- NULL
        }
        feedback_visible_orientation$themes <<- orientation_live_themes(
          orientation$themes %||% list(),
          candidates,
          feedback_visible_orientation$cards
        )
        entry_ids <- vapply(
          candidates,
          \(candidate) candidate$entry$entry_id %||% "",
          character(1)
        )
        feedback_visible_orientation$themes <<- lapply(
          feedback_visible_orientation$themes,
          function(theme) {
            theme$sources <- lapply(theme$entry_ids, function(entry_id) {
              feedback_source_metadata(candidates[[match(entry_id, entry_ids)]])
            })
            theme
          }
        )
        if (
          !length(feedback_visible_orientation$cards) &&
            !length(feedback_visible_orientation$themes)
        ) {
          feedback_visible_orientation <<- NULL
        }
      }
      feedback_visible_token <<- if (!is.null(feedback_visible_orientation)) {
        rill_id("feedback-view", canonical_json(feedback_visible_orientation))
      }
      invisible(feedback_visible_token)
    }
  )
}
