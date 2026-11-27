orientation_destination_settings_ui <- function(state) {
  destination <- state$destination
  status <- if (isTRUE(state$demo_mode)) {
    "Demo sample"
  } else if (!isTRUE(state$available)) {
    "Orientation off"
  } else if (isTRUE(state$needs_endpoint_configuration)) {
    "Endpoint needed"
  } else if (isTRUE(state$needs_configuration)) {
    "Policy link needed"
  } else if (isTRUE(state$enabled)) {
    "Orientation on"
  } else if (isTRUE(state$needs_confirmation)) {
    "Confirmation needed"
  } else {
    "Orientation off"
  }
  locality <- if (identical(destination$kind, "installation")) {
    "Runs inside this Rill installation."
  } else {
    paste0(
      "Sends story text to ",
      destination$label,
      ", outside this installation."
    )
  }

  shiny::tags$details(
    class = "orientation-destination-settings",
    shiny::tags$summary(
      shiny::tags$span(
        class = "orientation-destination-name",
        destination$name
      ),
      shiny::tags$span(" \u00b7 "),
      shiny::tags$span(
        class = "orientation-destination-status",
        role = "status",
        `aria-live` = "polite",
        status
      )
    ),
    shiny::tags$div(
      class = "orientation-destination-body",
      shiny::tags$p(
        if (isTRUE(state$demo_mode)) {
          "The demo shows a fixed sample. No story text is sent to a model."
        } else if (!isTRUE(state$available)) {
          paste(
            "This installation hasn't turned on Orientation.",
            "Reading and Ask Rill still work."
          )
        } else if (isTRUE(state$needs_endpoint_configuration)) {
          paste(
            "Set RILL_AGENT_BASE_URL to the model's endpoint before turning",
            "on Orientation."
          )
        } else if (isTRUE(state$needs_configuration)) {
          paste(
            "Set RILL_AGENT_POLICY_URL to the provider's data policy before",
            "turning on Orientation."
          )
        } else {
          locality
        }
      ),
      if (!is.na(destination$endpoint)) {
        shiny::tags$p(
          class = "orientation-destination-endpoint",
          "Endpoint ",
          shiny::tags$code(destination$endpoint)
        )
      },
      if (
        identical(destination$kind, "external") &&
          !isTRUE(state$demo_mode)
      ) {
        shiny::tags$p(
          class = "orientation-destination-caveat",
          paste(
            "Your agreement with the provider decides whether it keeps or",
            "trains on that text."
          )
        )
      },
      if (!is.na(destination$policy_url)) {
        shiny::tags$a(
          href = destination$policy_url,
          target = "_blank",
          rel = "noopener noreferrer",
          `aria-label` = paste0(
            "Read ",
            destination$name,
            "'s data policy (opens in a new tab)"
          ),
          "Read the provider's data policy"
        )
      },
      if (!isTRUE(state$available) || isTRUE(state$demo_mode)) {
        NULL
      } else if (isTRUE(state$needs_configuration)) {
        NULL
      } else if (isTRUE(state$enabled)) {
        shiny::actionButton(
          "orientation_disable",
          "Turn off Orientation",
          class = "btn-sm btn-outline-secondary"
        )
      } else {
        shiny::actionButton(
          "orientation_enable",
          "Turn on Orientation",
          class = "btn-sm btn-outline-secondary"
        )
      }
    )
  )
}

orientation_destination_confirmation_ui <- function(state) {
  destination <- state$destination
  if (!isTRUE(state$policy_ready) || is.na(destination$policy_url)) {
    cli::cli_abort(
      "Orientation needs a link to the provider's data policy.",
      class = "rill_orientation_policy_required"
    )
  }
  title_id <- "orientation-confirmation-title"
  modal <- shiny::modalDialog(
    title = shiny::tags$span(
      id = title_id,
      "Turn on Orientation?"
    ),
    easyClose = TRUE,
    size = "m",
    shiny::tags$p(
      paste0(
        "Each time Orientation updates, Rill sends ",
        destination$label,
        " the text of up to ",
        orientation_candidate_limit(),
        " of your newest unread stories."
      )
    ),
    shiny::tags$p(
      paste(
        "That tells the provider which of those stories you haven't read.",
        "Rill doesn't send the rest of your Library, your reading history,",
        "Reader Memory, or any credentials."
      )
    ),
    shiny::tags$p(
      shiny::tags$a(
        href = destination$policy_url,
        target = "_blank",
        rel = "noopener noreferrer",
        `aria-label` = paste0(
          "Read ",
          destination$name,
          "'s data policy (opens in a new tab)"
        ),
        "Read the provider's data policy (opens in a new tab)"
      )
    ),
    shiny::tags$p(
      paste0(
        destination$name,
        " runs outside this Rill installation, so Rill can't control whether ",
        "it keeps or trains on this text. Your agreement with ",
        destination$name,
        " does."
      )
    ),
    footer = shiny::tagList(
      shiny::modalButton("Not now"),
      shiny::actionButton(
        "orientation_confirm",
        paste("Turn on with", destination$name),
        class = "btn-primary"
      )
    )
  )
  htmltools::tagQuery(modal)$addAttrs(
    role = "dialog",
    `aria-labelledby` = title_id,
    `data-rill-orientation-confirmation` = ""
  )$allTags()
}

orientation_open_button <- function(class = "") {
  shiny::tags$button(
    type = "button",
    class = paste("orientation-return", class),
    onclick = "rillShowOrientation()",
    `aria-label` = "Open Orientation",
    bsicons::bs_icon("compass"),
    "Orientation"
  )
}

orientation_ui <- function(
  orientation,
  candidates,
  preparing = FALSE,
  processing_note = NULL,
  failure = NULL,
  feedback_token = NULL,
  unread_total = NULL
) {
  unread_total <- as.integer(
    unread_total %||% orientation_unread_total(candidates)
  )
  if (is.null(orientation)) {
    return(shiny::tags$section(
      class = "orientation-canvas orientation-quiet",
      rill_reading_otter("welcome-otter"),
      shiny::tags$p(class = "eyebrow", "Orientation"),
      shiny::tags$h1("Choose something worth reading"),
      orientation_failure_ui(
        if (preparing) {
          "Checking your unread stories\u2026"
        } else {
          failure %||% "No picks yet."
        }
      ),
      orientation_retry_button(failure, "retry_orientation"),
      orientation_processing_ui(processing_note),
      orientation_browse_button("Browse unread stories")
    ))
  }

  cards <- lapply(orientation$cards, function(card) {
    matches <- Filter(
      function(candidate) {
        !is.null(candidate$document) &&
          identical(candidate$document$document_id, card$document_id)
      },
      candidates
    )
    if (!length(matches)) NULL else list(card = card, candidate = matches[[1L]])
  })
  cards <- Filter(Negate(is.null), cards)
  themes <- orientation_live_themes(
    orientation$themes %||% list(),
    candidates,
    lapply(cards, `[[`, "card")
  )

  if (!length(cards)) {
    return(shiny::tags$section(
      id = if (length(themes)) "rill-orientation",
      class = "orientation-canvas orientation-quiet",
      rill_reading_otter("welcome-otter"),
      shiny::tags$p(class = "eyebrow", "Orientation"),
      shiny::tags$h1("No picks right now"),
      shiny::tags$p(
        class = "orientation-status",
        role = "status",
        `aria-live` = "polite",
        orientation$status
      ),
      orientation_evaluated_basis(
        orientation,
        orientation_boundary(candidates),
        preparing
      ),
      orientation_failure_ui(failure),
      orientation_retry_button(failure, "retry_orientation"),
      orientation_processing_ui(processing_note),
      orientation_browse_button("Browse unread stories"),
      orientation_themes_ui(themes, candidates),
      if (length(themes) && store_scalar_string(feedback_token)) {
        shiny::tags$div(
          class = "orientation-meta",
          feedback_action_button(
            "rate_orientation",
            "Rate this Orientation",
            feedback_token
          )
        )
      },
      orientation_totals_ui(
        unread_total,
        picked = 0L,
        themes = themes,
        evaluated = length(candidates)
      )
    ))
  }

  shiny::tags$section(
    id = "rill-orientation",
    class = "orientation-canvas",
    `aria-labelledby` = "orientation-title",
    shiny::tags$p(
      class = "visually-hidden orientation-live-status",
      role = "status",
      `aria-live` = "polite",
      orientation$status
    ),
    shiny::tags$header(
      class = "orientation-header",
      shiny::tags$p(
        class = "eyebrow",
        "Orientation"
      ),
      shiny::tags$h1(
        id = "orientation-title",
        orientation$question
      ),
      shiny::tags$div(
        class = "orientation-meta",
        orientation_evaluated_basis(
          orientation,
          orientation_boundary(candidates),
          preparing
        ),
        orientation_failure_ui(failure),
        orientation_retry_button(failure, "retry_orientation"),
        orientation_processing_ui(processing_note),
        orientation_browse_button("Browse unread stories"),
        if (store_scalar_string(feedback_token)) {
          feedback_action_button(
            "rate_orientation",
            "Rate this Orientation",
            feedback_token
          )
        }
      )
    ),
    shiny::tags$div(
      class = "orientation-picks",
      lapply(seq_along(cards), function(index) {
        pair <- cards[[index]]
        orientation_card_ui(
          pair$card,
          pair$candidate,
          index,
          orientation
        )
      })
    ),
    orientation_themes_ui(themes, candidates),
    orientation_totals_ui(
      unread_total,
      picked = length(cards),
      themes = themes,
      evaluated = length(candidates)
    )
  )
}

orientation_themes_ui <- function(themes, candidates) {
  if (!length(themes)) {
    return(NULL)
  }
  feed_titles <- stats::setNames(
    vapply(
      candidates,
      \(candidate) {
        as.character(
          candidate$document$site %||% candidate$entry$feed_title %||% ""
        )
      },
      character(1)
    ),
    vapply(
      candidates,
      \(candidate) as.character(candidate$entry$entry_id),
      character(1)
    )
  )

  shiny::tags$section(
    class = "orientation-themes",
    `aria-labelledby` = "orientation-themes-title",
    shiny::tags$h2(id = "orientation-themes-title", "More in your unread"),
    shiny::tags$p(
      class = "orientation-themes-note",
      "Rill grouped the rest by their titles and opening lines. The topic",
      "names and notes are its interpretation."
    ),
    lapply(themes, function(theme) {
      count <- length(theme$entry_ids)
      sources <- unique(feed_titles[theme$entry_ids])
      sources <- sources[!is.na(sources) & nzchar(sources)]
      shiny::tags$article(
        class = "orientation-theme",
        `data-theme-id` = theme$theme_id,
        shiny::tags$span(class = "orientation-theme-count", count),
        shiny::tags$button(
          type = "button",
          class = "orientation-theme-name",
          onclick = sprintf(
            "rillBrowseOrientationTheme(%s)",
            jsonlite::toJSON(theme$theme_id, auto_unbox = TRUE)
          ),
          `aria-label` = paste0(
            theme$name,
            " (",
            count,
            if (count == 1L) " unread story)" else " unread stories)"
          ),
          theme$name
        ),
        shiny::tags$p(class = "orientation-theme-note", theme$note),
        if (length(sources)) {
          shiny::tags$span(
            class = "orientation-theme-sources",
            paste(utils::head(sources, 3L), collapse = " \u00b7 ")
          )
        }
      )
    })
  )
}

orientation_totals_ui <- function(unread_total, picked, themes, evaluated) {
  unread_total <- as.integer(unread_total %||% 0L)
  if (!unread_total) {
    return(NULL)
  }
  themed <- sum(vapply(themes, \(theme) length(theme$entry_ids), integer(1)))
  outside <- max(0L, unread_total - as.integer(evaluated))
  parts <- c(
    paste(unread_total, "unread"),
    paste(picked, "picked"),
    paste(themed, "in topics"),
    if (outside) paste(outside, "not checked")
  )
  shiny::tags$p(
    class = "orientation-totals",
    paste(parts, collapse = " \u00b7 "),
    shiny::tags$button(
      type = "button",
      class = "orientation-totals-link",
      onclick = "rillBrowseQueue()",
      "Browse unread stories"
    )
  )
}

orientation_evidence_sentence <- function(evidence) {
  parts <- strsplit(evidence, "(?<=[.!?])\\s+", perl = TRUE)[[1L]]
  trimws(parts[[1L]] %||% evidence)
}

orientation_evidence_lead <- function(evidence) {
  lead <- orientation_evidence_sentence(evidence)
  if (nchar(lead) > 220L) {
    lead <- sub("\\s+\\S*$", "", substr(lead, 1L, 200L))
  }
  lead
}

orientation_processing_ui <- function(processing_note) {
  if (is.null(processing_note) || !nzchar(processing_note)) {
    return(NULL)
  }
  shiny::tags$small(
    class = "orientation-destination",
    processing_note
  )
}

orientation_queue_status_ui <- function(
  orientation,
  candidates,
  preparing = FALSE,
  processing_note = NULL,
  failure = NULL
) {
  if (!is.null(orientation)) {
    current_ids <- vapply(
      Filter(\(candidate) !is.null(candidate$document), candidates),
      \(candidate) candidate$document$document_id,
      character(1)
    )
    card_ids <- vapply(
      orientation$cards %||% list(),
      `[[`,
      character(1),
      "document_id"
    )
    if (any(card_ids %in% current_ids)) {
      return(NULL)
    }
  }

  status <- if (is.null(orientation)) {
    if (preparing) {
      "Checking your unread stories\u2026"
    } else {
      failure %||% "No picks yet."
    }
  } else {
    orientation$status
  }

  shiny::tags$aside(
    class = "orientation-queue-status",
    `aria-label` = "Orientation status",
    shiny::tags$strong("Orientation"),
    orientation_failure_ui(status),
    if (!is.null(orientation)) {
      orientation_evaluated_basis(
        orientation,
        orientation_boundary(candidates),
        preparing
      )
    },
    if (!is.null(orientation)) orientation_failure_ui(failure),
    orientation_retry_button(failure, "retry_orientation_queue"),
    orientation_processing_ui(processing_note)
  )
}

orientation_failure_ui <- function(failure) {
  if (is.null(failure)) {
    return(NULL)
  }
  if (inherits(failure, "condition")) {
    return(shiny::tags$div(
      class = "orientation-status",
      shiny::tags$p(
        role = "status",
        `aria-live` = "polite",
        conditionMessage(failure)
      ),
      shiny::tags$details(
        shiny::tags$summary("Error details"),
        shiny::tags$pre(paste(format(failure), collapse = "\n"))
      )
    ))
  }
  shiny::tags$p(
    class = "orientation-status",
    role = "status",
    `aria-live` = "polite",
    failure
  )
}

orientation_retry_button <- function(failure, input_id) {
  if (is.null(failure)) {
    return(NULL)
  }
  shiny::actionButton(
    input_id,
    "Retry Orientation",
    class = "btn-outline-secondary"
  )
}

orientation_browse_button <- function(label) {
  shiny::tags$button(
    type = "button",
    class = "orientation-browse",
    onclick = "rillBrowseQueue()",
    bsicons::bs_icon("list-ul"),
    label
  )
}

orientation_boundary_entry_ids <- function(boundary) {
  candidates <- boundary$candidates %||% list()
  if (!length(candidates) && isTRUE(boundary$candidate_count > 0L)) {
    return(NULL)
  }
  vapply(
    candidates,
    \(candidate) as.character(candidate$entry_id %||% NA_character_),
    character(1)
  )
}

orientation_boundary_change <- function(evaluated, current) {
  if (identical(evaluated$hash, current$hash)) {
    return("Nothing has changed since then.")
  }

  # Compare stories, not copies: a story that gets a new reading copy keeps
  # its entry ID but changes its document ID.
  evaluated_ids <- orientation_boundary_entry_ids(evaluated)
  current_ids <- orientation_boundary_entry_ids(current)
  if (is.null(evaluated_ids) || is.null(current_ids)) {
    return("Since then: The stories it looked at have changed.")
  }
  added <- length(setdiff(current_ids, evaluated_ids))
  removed <- length(setdiff(evaluated_ids, current_ids))
  changes <- c(
    if (added) {
      paste(added, if (added == 1L) "story added" else "stories added")
    },
    if (removed) paste(removed, "removed")
  )
  if (!length(changes)) {
    changes <- "The stories it looked at have changed"
  }
  paste0("Since then: ", paste(changes, collapse = "; "), ".")
}

orientation_evaluated_basis <- function(
  orientation,
  current_boundary,
  preparing = FALSE
) {
  count <- as.integer(orientation$boundary$candidate_count %||% 0L)
  noun <- if (count == 1L) "unread story" else "unread stories"
  evaluated <- gsub(
    " +",
    " ",
    trimws(format(
      orientation$evaluated_at,
      "%b %e at %l:%M %p UTC",
      tz = "UTC"
    ))
  )
  evaluated_at <- format(
    orientation$evaluated_at,
    "%Y-%m-%dT%H:%M:%SZ",
    tz = "UTC"
  )
  prefix <- if (preparing) {
    "Updating \u00b7 last checked "
  } else if (!identical(orientation$boundary$hash, current_boundary$hash)) {
    "Out of date \u00b7 checked "
  } else {
    "Checked "
  }
  shiny::tags$details(
    class = "orientation-evaluated",
    shiny::tags$summary(
      prefix,
      shiny::tags$time(
        datetime = evaluated_at,
        evaluated
      ),
      paste0(" \u00b7 ", count, " ", noun)
    ),
    shiny::tags$p(
      orientation_boundary_change(orientation$boundary, current_boundary)
    )
  )
}

orientation_card_ui <- function(card, candidate, index, orientation) {
  document <- candidate$document
  entry <- candidate$entry
  original_source_url <- rill_document_original_source_url(document)
  copy <- reading_copy_label(document)
  title <- document$title %||% entry$title
  select <- sprintf(
    "rillSelectEntry(%s, %d, 'orientation', %s)",
    jsonlite::toJSON(document$entry_id, auto_unbox = TRUE),
    as.integer(index),
    jsonlite::toJSON(
      list(
        orientation_id = orientation$orientation_id,
        revision_id = orientation$revision_id,
        card_id = card$card_id,
        document_id = card$document_id,
        basis_hash = card$basis_hash,
        rationale_hash = card$rationale_hash
      ),
      auto_unbox = TRUE
    )
  )
  dismiss <- sprintf(
    "rillDismissOrientation(%s, %s, %s)",
    jsonlite::toJSON(card$card_id, auto_unbox = TRUE),
    jsonlite::toJSON(orientation$revision_id, auto_unbox = TRUE),
    jsonlite::toJSON(card$rationale_hash, auto_unbox = TRUE)
  )
  lead <- orientation_evidence_lead(card$evidence)

  shiny::tags$article(
    class = "orientation-step",
    `data-card-id` = card$card_id,
    `data-revision-id` = orientation$revision_id,
    `data-rationale-hash` = card$rationale_hash,
    shiny::tags$div(
      class = "orientation-step-body",
      shiny::tags$p(
        class = "orientation-source",
        shiny::tags$span(document$site %||% entry$feed_title),
        shiny::tags$span(format_story_time(entry$published_at))
      ),
      shiny::tags$button(
        type = "button",
        class = "orientation-read",
        onclick = select,
        title
      ),
      shiny::tags$p(
        class = "orientation-interpretation",
        shiny::tags$span(
          class = "orientation-interpretation-label",
          "Rill's interpretation"
        ),
        card$interpretation
      ),
      shiny::tags$p(
        class = "orientation-why",
        shiny::tags$strong("Why now \u00b7 "),
        card$why_now
      ),
      shiny::tags$blockquote(
        class = c(
          "orientation-evidence-lead",
          if (
            nchar(lead) < nchar(orientation_evidence_sentence(card$evidence))
          ) {
            "is-truncated"
          }
        ),
        lead
      ),
      shiny::tags$details(
        class = "orientation-evidence",
        shiny::tags$summary("Source passage and details"),
        shiny::tags$blockquote(card$evidence),
        shiny::tags$dl(
          class = "orientation-evidence-metadata",
          shiny::tags$dt("Copy ID"),
          shiny::tags$dd(shiny::tags$code(document$document_id)),
          shiny::tags$dt("Original"),
          shiny::tags$dd(
            shiny::tags$a(
              href = original_source_url,
              target = "_blank",
              rel = "noopener noreferrer",
              original_source_url
            )
          ),
          shiny::tags$dt("Copy"),
          shiny::tags$dd(
            paste(
              copy$label,
              "\u00b7",
              copy$detail,
              "\u00b7 saved",
              format(
                as.POSIXct(document$captured_at, tz = "UTC"),
                "%Y-%m-%d %H:%M UTC",
                tz = "UTC"
              )
            )
          ),
          shiny::tags$dt("Limitations"),
          shiny::tags$dd(rill_document_limitations(document))
        )
      )
    ),
    shiny::tags$button(
      type = "button",
      class = "orientation-dismiss",
      onclick = dismiss,
      `aria-label` = paste("Not for me:", title),
      bsicons::bs_icon("x-lg"),
      "Not for me"
    )
  )
}
