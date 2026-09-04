rill_ui <- function(config) {
  bslib::page_fillable(
    if (identical(config$identity_mode, "auth0")) {
      shinyOAuth::use_shinyOAuth()
    },
    theme = bslib::bs_theme(
      version = 5,
      brand = brand.yml::read_brand_yml(
        rill_package_file("app", "_brand.yml")
      )
    ),
    fillable_mobile = TRUE,
    shiny::tags$head(
      shiny::tags$meta(
        name = "viewport",
        content = "width=device-width, initial-scale=1"
      ),
      shiny::tags$link(
        rel = "icon",
        type = "image/png",
        href = "rill-assets/rill-duck.png"
      ),
      shiny::tags$title("Rill \u2014 personal reader"),
      shiny::includeScript(rill_package_file("app", "www", "app.js")),
      shiny::includeCSS(rill_package_file("app", "www", "styles.css"))
    ),
    bslib::as_fill_carrier(
      shiny::tags$main(
        class = "app-shell",
        bslib::layout_sidebar(
          bslib::layout_sidebar(
            reader_pane_ui(config),
            sidebar = story_sidebar_ui(),
            fillable = TRUE,
            fill = TRUE,
            border = FALSE,
            border_radius = FALSE,
            padding = 0,
            gap = 0,
            height = "100%",
            class = "reader-pane"
          ),
          sidebar = navigation_sidebar_ui(config),
          fillable = TRUE,
          fill = TRUE,
          border = FALSE,
          border_radius = FALSE,
          padding = 0,
          gap = 0,
          height = "100%",
          class = "reading-shell"
        )
      )
    )
  )
}

navigation_sidebar_ui <- function(config) {
  bslib::sidebar(
    shiny::tags$div(
      class = "brand",
      rill_duck_mark("brand-mark"),
      shiny::tags$div(
        shiny::tags$strong(config$app_name),
        shiny::tags$small("Personal reader")
      )
    ),
    if (config$demo_mode) {
      shiny::tags$div(
        class = "mode-badge",
        "Demo data \u00b7 resets on restart"
      )
    } else {
      NULL
    },
    shiny::radioButtons(
      "view",
      label = NULL,
      choiceNames = list(
        shiny::tags$span(
          class = "nav-choice",
          bsicons::bs_icon("inbox"),
          "Unread"
        ),
        shiny::tags$span(
          class = "nav-choice",
          bsicons::bs_icon("archive"),
          "All stories"
        ),
        shiny::tags$span(
          class = "nav-choice",
          bsicons::bs_icon("star"),
          "Starred"
        ),
        shiny::tags$span(
          class = "nav-choice",
          bsicons::bs_icon("bookmark"),
          "Saved"
        ),
        shiny::tags$span(
          class = "nav-choice nav-choice-temporal-first",
          bsicons::bs_icon("calendar-day"),
          "Today"
        ),
        shiny::tags$span(
          class = "nav-choice",
          bsicons::bs_icon("calendar-week"),
          "This week"
        ),
        shiny::tags$span(
          class = "nav-choice",
          bsicons::bs_icon("calendar-month"),
          "This month"
        )
      ),
      choiceValues = c(
        "unread",
        "all",
        "starred",
        "saved",
        "today",
        "week",
        "month"
      ),
      selected = "unread"
    ),
    shiny::tags$div(
      class = "feed-heading",
      shiny::tags$span("Feeds"),
      shiny::actionButton(
        "refresh_feeds",
        label = bsicons::bs_icon("arrow-repeat"),
        class = "icon-button",
        title = "Refresh feeds",
        `aria-label` = "Refresh feeds"
      )
    ),
    shiny::uiOutput("feed_nav", container = shiny::tags$nav),
    shiny::tags$div(
      class = "sidebar-footer",
      shiny::uiOutput("orientation_destination_settings"),
      appearance_control_ui(),
      feed_tools_ui(),
      identity_sign_out_ui(config),
      shiny::uiOutput(
        "sidebar_status",
        container = shiny::tags$div,
        `aria-live` = "polite",
        `aria-atomic` = "true"
      )
    ),
    id = "navigation_sidebar",
    class = "nav-sidebar",
    width = "230px",
    open = list(desktop = "open", mobile = "always-above"),
    resizable = TRUE,
    fillable = TRUE,
    padding = 0,
    gap = 0
  )
}

orientation_destination_settings_ui <- function(state) {
  destination <- state$destination
  status <- if (isTRUE(state$demo_mode)) {
    "Demo Orientation"
  } else if (!isTRUE(state$available)) {
    "Orientation unavailable"
  } else if (isTRUE(state$needs_endpoint_configuration)) {
    "Endpoint required"
  } else if (isTRUE(state$needs_configuration)) {
    "Provider terms required"
  } else if (isTRUE(state$enabled)) {
    "Orientation on"
  } else if (isTRUE(state$needs_confirmation)) {
    "Confirmation needed"
  } else {
    "Orientation off"
  }
  locality <- if (identical(destination$kind, "installation")) {
    "Runs within this Rill installation."
  } else {
    paste(destination$label, "is external to this Rill installation.")
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
          paste(
            "Bundled demo Orientation uses local sample content; no reading",
            "copies are sent to a model."
          )
        } else if (!isTRUE(state$available)) {
          paste(
            "This installation has automatic Orientation turned off.",
            "Reading and Ask Rill remain available."
          )
        } else if (isTRUE(state$needs_endpoint_configuration)) {
          paste(
            "Automatic Orientation needs an explicit model endpoint in",
            "RILL_AGENT_BASE_URL before it can be enabled."
          )
        } else if (isTRUE(state$needs_configuration)) {
          paste(
            "Automatic Orientation needs an inspectable provider-policy",
            "link before it can be enabled."
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
            "Your provider agreement controls retention, deletion, and",
            "training practices."
          )
        )
      },
      if (!is.na(destination$policy_url)) {
        shiny::tags$a(
          href = destination$policy_url,
          target = "_blank",
          rel = "noopener noreferrer",
          `aria-label` = paste(
            "Review provider terms for",
            destination$name,
            "(opens in a new tab)"
          ),
          "Review provider terms"
        )
      },
      if (!isTRUE(state$available) || isTRUE(state$demo_mode)) {
        NULL
      } else if (isTRUE(state$needs_configuration)) {
        NULL
      } else if (isTRUE(state$enabled)) {
        shiny::actionButton(
          "orientation_disable",
          "Disable automatic Orientation",
          class = "btn-sm btn-outline-secondary"
        )
      } else {
        shiny::actionButton(
          "orientation_enable",
          "Enable automatic Orientation",
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
      "Automatic Orientation requires an inspectable provider-policy link.",
      class = "rill_orientation_policy_required"
    )
  }
  title_id <- "orientation-confirmation-title"
  modal <- shiny::modalDialog(
    title = shiny::tags$span(
      id = title_id,
      "Enable automatic Orientation?"
    ),
    easyClose = TRUE,
    size = "m",
    shiny::tags$p(
      paste(
        "Rill will send only bounded unread Document reading copies needed",
        "for each Orientation to",
        paste0(destination$label, ".")
      )
    ),
    shiny::tags$p(
      paste(
        "The selected copies disclose that those Documents are currently",
        "unread. Rill will not send the rest of your Library, the Reading",
        "History event log, Reader Memory, or credentials."
      )
    ),
    shiny::tags$p(
      shiny::tags$a(
        href = destination$policy_url,
        target = "_blank",
        rel = "noopener noreferrer",
        `aria-label` = paste(
          "Review provider terms for",
          destination$name,
          "(opens in a new tab)"
        ),
        "Review the governing provider terms (opens in a new tab)"
      )
    ),
    shiny::tags$p(
      paste0(
        destination$name,
        " is external to this Rill installation. Rill cannot enforce its ",
        "retention, deletion, or training practices; your provider ",
        "agreement controls them."
      )
    ),
    footer = shiny::tagList(
      shiny::modalButton("Not now"),
      shiny::actionButton(
        "orientation_confirm",
        paste("Confirm", destination$name, "and enable"),
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

story_sidebar_ui <- function() {
  bslib::sidebar(
    shiny::tags$header(
      class = "pane-header",
      shiny::tags$div(
        shiny::tags$p(class = "eyebrow", "Reading queue"),
        shiny::uiOutput("list_title", container = shiny::tags$div)
      ),
      shiny::tags$div(
        class = "queue-controls",
        shiny::uiOutput(
          "prepare_today_control",
          container = shiny::tags$div,
          class = "prepare-today-control"
        ),
        shiny::uiOutput(
          "read_actions",
          container = shiny::tags$div,
          class = "read-actions"
        ),
        shiny::tags$div(
          class = "story-sort",
          shiny::selectInput(
            "story_sort",
            label = shiny::tags$span(
              class = "visually-hidden",
              "Sort stories"
            ),
            choices = c(
              "Newest" = "newest",
              "Oldest" = "oldest",
              "Recently added" = "recently_added",
              "Feed A\u2013Z" = "feed",
              "Title A\u2013Z" = "title"
            ),
            selected = "newest",
            selectize = FALSE,
            width = "126px"
          )
        ),
        shiny::uiOutput("story_count", container = shiny::tags$div)
      )
    ),
    shiny::uiOutput(
      "orientation_queue_status",
      container = shiny::tags$div,
      class = "orientation-queue-status-slot"
    ),
    shiny::uiOutput(
      "story_list",
      container = shiny::tags$div,
      class = "story-list",
      `aria-keyshortcuts` = "j k"
    ),
    id = "story_sidebar",
    class = "story-pane",
    width = "385px",
    open = list(desktop = "open", mobile = "always-above"),
    resizable = TRUE,
    fillable = TRUE,
    padding = 0,
    gap = 0
  )
}

prepare_today_button <- function() {
  shiny::actionButton(
    "prepare_today",
    "Prepare",
    icon = bsicons::bs_icon("cloud-arrow-down"),
    class = "btn-prepare-today",
    title = "Prepare today\u2019s reading copies",
    `aria-label` = "Prepare today\u2019s reading copies"
  )
}

read_actions_ui <- function(feed_title = NULL) {
  scope <- if (is.null(feed_title)) "all feeds" else feed_title

  bslib::popover(
    shiny::tags$button(
      type = "button",
      class = "read-actions-trigger",
      title = "Manage reading status",
      `aria-label` = "Manage reading status",
      bsicons::bs_icon("check2-all")
    ),
    title = "Reading status",
    placement = "bottom",
    shiny::tags$div(
      class = "read-actions-menu",
      shiny::tags$p(
        class = "read-actions-scope",
        paste("Applies to", scope, "across all views.")
      ),
      shiny::actionButton(
        "mark_all_read",
        "Mark all as read",
        class = "btn-read-action"
      ),
      shiny::actionButton(
        "mark_older_read",
        "Mark older than a day as read",
        class = "btn-read-action"
      ),
      shiny::tags$p(
        class = "read-actions-help",
        "Open history stays unchanged."
      )
    )
  )
}

mark_unread_button <- function() {
  shiny::actionButton(
    "mark_unread",
    shiny::tagList(bsicons::bs_icon("circle"), "Mark unread"),
    class = "reader-action"
  )
}

reader_pane_ui <- function(config) {
  bslib::layout_sidebar(
    shiny::tags$div(
      class = "reader-scroll",
      shiny::uiOutput("reader_header", container = shiny::tags$div),
      shiny::uiOutput("reader_body", container = shiny::tags$div)
    ),
    sidebar = bslib::sidebar(
      shiny::tags$header(
        class = "reader-agent-header",
        shiny::tags$p(class = "eyebrow", "Ask Rill"),
        shiny::tags$h2("Ask Rill about this story")
      ),
      shiny::uiOutput(
        "reader_agent_status",
        container = shiny::tags$div,
        class = "reader-agent-status-slot"
      ),
      shinychat::chat_ui(
        "reader_chat",
        greeting = shinychat::chat_greeting(
          "Choose a story, then ask what it says, why it matters, or how it connects."
        ),
        placeholder = "Ask about the selected story\u2026",
        height = "100%",
        fill = TRUE,
        enable_cancel = TRUE,
        footer = shiny::tags$span(
          paste(
            "Sends your question and selected reading copy to",
            rill_agent_data_destination(
              config$agent_model,
              config$agent_base_url %||% ""
            ),
            "\u00b7 interpretation stays labeled"
          )
        )
      ),
      id = "reader_agent_sidebar",
      class = "reader-agent-sidebar",
      width = "380px",
      position = "right",
      open = list(desktop = "open", mobile = "closed"),
      resizable = TRUE,
      fillable = TRUE,
      padding = 0,
      gap = 0
    ),
    fillable = TRUE,
    fill = TRUE,
    border = FALSE,
    border_radius = FALSE,
    padding = 0,
    gap = 0,
    height = "100%"
  )
}

orientation_ui <- function(
  orientation,
  candidates,
  preparing = FALSE,
  processing_note = NULL
) {
  if (is.null(orientation)) {
    return(shiny::tags$section(
      class = "orientation-canvas orientation-quiet",
      shiny::tags$p(class = "eyebrow", "Orientation"),
      shiny::tags$h1("Choose something worth reading"),
      shiny::tags$p(
        class = "orientation-status",
        role = "status",
        `aria-live` = "polite",
        if (preparing) {
          "Evaluating the current unread Documents\u2026"
        } else {
          "Orientation will appear after Rill evaluates your unread Documents."
        }
      ),
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

  if (!length(cards)) {
    return(shiny::tags$section(
      class = "orientation-canvas orientation-quiet",
      shiny::tags$p(class = "eyebrow", "Orientation"),
      shiny::tags$h1("No current Orientation selection"),
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
      orientation_processing_ui(processing_note),
      orientation_browse_button("Browse unread stories")
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
      shiny::tags$div(
        shiny::tags$p(class = "eyebrow", "Orientation"),
        shiny::tags$h1(
          id = "orientation-title",
          "One question is worth carrying into this reading"
        ),
        shiny::tags$p(
          class = "orientation-question",
          orientation$question
        )
      ),
      shiny::tags$div(
        class = "orientation-maintenance",
        shiny::tags$span("Maintained from current unread Documents"),
        orientation_evaluated_basis(
          orientation,
          orientation_boundary(candidates),
          preparing
        ),
        orientation_processing_ui(processing_note),
        orientation_browse_button("Browse the full unread queue")
      )
    ),
    shiny::tags$p(
      class = "orientation-introduction",
      orientation$introduction
    ),
    shiny::tags$div(
      class = "orientation-path",
      lapply(seq_along(cards), function(index) {
        pair <- cards[[index]]
        shiny::tagList(
          if (index > 1L) {
            shiny::tags$div(
              class = "orientation-connector",
              shiny::tags$span(orientation_connector_label(pair$card)),
              bsicons::bs_icon("arrow-down")
            )
          },
          orientation_card_ui(
            pair$card,
            pair$candidate,
            index,
            orientation
          )
        )
      })
    )
  )
}

orientation_connector_label <- function(card) {
  switch(
    card$role,
    contrast = "Then test the counterpoint",
    extension = "Then make the connection",
    "Then continue the reading path"
  )
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
  processing_note = NULL
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
      "Evaluating the current unread Documents\u2026"
    } else {
      "Orientation will appear after Rill evaluates your unread Documents."
    }
  } else {
    orientation$status
  }

  shiny::tags$aside(
    class = "orientation-queue-status",
    `aria-label` = "Orientation status",
    shiny::tags$strong("Orientation"),
    shiny::tags$p(
      class = "orientation-status",
      role = "status",
      `aria-live` = "polite",
      status
    ),
    if (!is.null(orientation)) {
      orientation_evaluated_basis(
        orientation,
        orientation_boundary(candidates),
        preparing
      )
    },
    orientation_processing_ui(processing_note)
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

orientation_boundary_change <- function(evaluated, current) {
  if (identical(evaluated$hash, current$hash)) {
    return("No material Library changes since this evaluation.")
  }

  evaluated_ids <- as.character(evaluated$document_ids %||% character())
  current_ids <- as.character(current$document_ids %||% character())
  added <- length(setdiff(current_ids, evaluated_ids))
  removed <- length(setdiff(evaluated_ids, current_ids))
  changes <- character()
  if (added) {
    changes <- c(
      changes,
      paste(
        added,
        "new eligible",
        if (added == 1L) "Document" else "Documents"
      )
    )
  }
  if (removed) {
    changes <- c(changes, paste(removed, "no longer eligible"))
  }
  if (!length(changes)) {
    changes <- "The bounded evaluation inputs changed"
  }
  paste0("Since then: ", paste(changes, collapse = "; "), ".")
}

orientation_evaluated_basis <- function(
  orientation,
  current_boundary,
  preparing = FALSE
) {
  count <- as.integer(orientation$boundary$candidate_count %||% 0L)
  noun <- if (count == 1L) "unread Document" else "unread Documents"
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
    "Reevaluating \u00b7 last evaluated "
  } else if (!identical(orientation$boundary$hash, current_boundary$hash)) {
    "Update due \u00b7 evaluated "
  } else {
    "Evaluated "
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
  number <- sprintf("%02d", index)
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
  frame <- switch(
    card$frame,
    change = "Notice the change",
    connection = "Make the connection",
    counterpoint = "Test the counterpoint",
    unresolved_question = "Follow the question",
    "Read with a question"
  )

  shiny::tags$article(
    class = "orientation-step",
    `data-card-id` = card$card_id,
    `data-revision-id` = orientation$revision_id,
    `data-rationale-hash` = card$rationale_hash,
    shiny::tags$div(class = "orientation-step-number", number),
    shiny::tags$div(
      class = "orientation-step-body",
      shiny::tags$p(class = "orientation-card-kind", frame),
      shiny::tags$div(
        class = "orientation-source",
        shiny::tags$span(document$site %||% entry$feed_title),
        shiny::tags$strong(document$title %||% entry$title),
        shiny::tags$span(format_story_time(entry$published_at))
      ),
      shiny::tags$p(
        class = "orientation-interpretation-label",
        "Interpretation"
      ),
      shiny::tags$p(
        class = "orientation-interpretation",
        card$interpretation
      ),
      shiny::tags$p(
        class = "orientation-why",
        shiny::tags$strong("Why now \u00b7 "),
        card$why_now
      ),
      shiny::tags$details(
        class = "orientation-evidence",
        shiny::tags$summary(paste0("Inspect Source Evidence [", number, "]")),
        shiny::tags$blockquote(card$evidence),
        shiny::tags$dl(
          class = "orientation-evidence-metadata",
          shiny::tags$dt("Document"),
          shiny::tags$dd(shiny::tags$code(document$document_id)),
          shiny::tags$dt("Original Source"),
          shiny::tags$dd(
            shiny::tags$a(
              href = original_source_url,
              target = "_blank",
              rel = "noopener noreferrer",
              original_source_url
            )
          ),
          shiny::tags$dt("Acquisition"),
          shiny::tags$dd(
            paste(
              gsub("_", " ", document$acquisition_method, fixed = TRUE),
              "by",
              document$producer,
              "\u00b7 captured",
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
      ),
      shiny::tags$div(
        class = "orientation-step-actions",
        shiny::tags$button(
          type = "button",
          class = "orientation-read",
          onclick = select,
          if (identical(card$role, "anchor")) {
            "Read the anchor Document"
          } else {
            "Read this Document"
          },
          bsicons::bs_icon("arrow-right")
        ),
        shiny::tags$button(
          type = "button",
          class = "orientation-dismiss",
          onclick = dismiss,
          `aria-label` = paste("Not for me:", document$title %||% entry$title),
          bsicons::bs_icon("x-lg"),
          "Not for me"
        )
      )
    )
  )
}

rill_duck_mark <- function(class) {
  shiny::tags$span(
    class = class,
    `aria-hidden` = "true",
    shiny::tags$img(
      class = "theme-logo theme-logo-light",
      src = "rill-assets/rill-duck.png",
      alt = ""
    ),
    shiny::tags$img(
      class = "theme-logo theme-logo-dark",
      src = "rill-assets/rill-duck-dark.png",
      alt = ""
    )
  )
}

appearance_control_ui <- function() {
  options <- list(
    system = list(icon = "circle-half", label = "System"),
    light = list(icon = "sun", label = "Light"),
    dark = list(icon = "moon-stars", label = "Dark")
  )

  shiny::tags$fieldset(
    class = "appearance-control",
    shiny::tags$legend(class = "visually-hidden", "Appearance"),
    lapply(names(options), function(value) {
      option <- options[[value]]
      input_id <- paste0("rill-theme-", value)
      shiny::tags$label(
        class = "appearance-option",
        `for` = input_id,
        title = option$label,
        shiny::tags$input(
          id = input_id,
          type = "radio",
          name = "rill_theme_mode",
          value = value,
          checked = if (identical(value, "system")) NA else NULL
        ),
        bsicons::bs_icon(option$icon),
        shiny::tags$span(class = "visually-hidden", option$label)
      )
    })
  )
}

feed_tools_ui <- function() {
  shiny::tags$details(
    class = "add-feed feed-tools",
    shiny::tags$summary("Manage feeds"),
    shiny::tags$div(
      class = "feed-tool-section rename-feed-tools",
      shiny::tags$p(class = "feed-tool-label", "Organize selected feed"),
      shiny::uiOutput("feed_organization_control")
    ),
    shiny::tags$div(
      class = "feed-tool-section",
      shiny::tags$p(class = "feed-tool-label", "Add one feed"),
      shiny::tags$label(
        class = "visually-hidden",
        `for` = "new_feed_url",
        "Feed or website URL"
      ),
      shiny::textInput(
        "new_feed_url",
        label = NULL,
        placeholder = "Feed or website URL"
      ),
      shiny::actionButton("add_feed", "Add feed", class = "btn-add-feed")
    ),
    shiny::tags$div(
      class = "feed-tool-section opml-tools",
      shiny::tags$p(class = "feed-tool-label", "Move subscriptions"),
      shiny::tags$div(
        class = "opml-actions",
        shiny::tags$div(
          class = "opml-upload",
          shiny::fileInput(
            "import_opml",
            label = NULL,
            accept = c(".opml", ".xml", "text/x-opml", "application/xml"),
            buttonLabel = "Import OPML",
            placeholder = "No file selected"
          )
        ),
        shiny::downloadButton(
          "export_opml",
          "Export OPML",
          class = "btn-opml-export"
        )
      ),
      shiny::tags$p(
        class = "feed-tool-help",
        "Folders are preserved when moving between readers."
      )
    )
  )
}

feed_organization_control_ui <- function(feed = NULL) {
  if (is.null(feed)) {
    return(shiny::tags$p(
      class = "feed-tool-help",
      "Select a feed above to rename, move, or unsubscribe."
    ))
  }

  shiny::tagList(
    shiny::tags$label(
      class = "visually-hidden",
      `for` = "feed_title",
      "Feed name"
    ),
    shiny::textInput(
      "feed_title",
      label = NULL,
      value = feed$title
    ),
    shiny::actionButton(
      "rename_feed",
      "Rename feed",
      class = "btn-rename-feed"
    ),
    shiny::textInput(
      "feed_folder",
      label = "Folder",
      value = feed$folder,
      placeholder = "Folder"
    ),
    shiny::actionButton(
      "move_feed",
      "Move feed",
      class = "btn-move-feed"
    ),
    if (identical(feed$source_kind %||% "subscription", "subscription")) {
      shiny::actionButton(
        "unsubscribe_feed",
        "Unsubscribe",
        class = "btn-unsubscribe-feed"
      )
    },
    shiny::tags$p(
      class = "feed-tool-help",
      if (identical(feed$source_kind %||% "subscription", "subscription")) {
        paste(
          "The source stays shared. Unsubscribing hides it from this Library",
          "but preserves reading state for restoration."
        )
      } else {
        "Captured items are Reader-owned and cannot be unsubscribed as a Feed."
      }
    )
  )
}

format_story_time <- function(value) {
  parsed <- tryCatch(
    suppressWarnings(as.POSIXct(value, tz = "UTC")),
    error = function(error) as.POSIXct(NA)
  )
  if (length(parsed) == 0L || is.na(parsed)) {
    return("")
  }
  seconds <- as.numeric(difftime(Sys.time(), parsed, units = "secs"))
  if (seconds < 3600) {
    return(paste0(max(1L, floor(seconds / 60)), "m"))
  }
  if (seconds < 86400) {
    return(paste0(floor(seconds / 3600), "h"))
  }
  if (seconds < 604800) {
    return(paste0(floor(seconds / 86400), "d"))
  }
  format(parsed, "%b %e")
}

story_card <- function(entry, index, selected = FALSE) {
  entry_id <- as.character(entry$entry_id)
  is_read <- !is.na(entry$read_at) && nzchar(as.character(entry$read_at))
  onclick <- sprintf(
    "rillSelectEntry(%s, %d)",
    jsonlite::toJSON(entry_id, auto_unbox = TRUE),
    as.integer(index)
  )
  classes <- c(
    "story-card",
    if (is_read) "is-read",
    if (isTRUE(entry$starred)) "is-starred",
    if (selected) "is-selected"
  )

  shiny::tags$button(
    type = "button",
    class = paste(classes, collapse = " "),
    `data-entry-id` = entry_id,
    `aria-current` = if (selected) "true" else NULL,
    onclick = onclick,
    shiny::tags$span(
      class = "story-status-icon",
      `aria-hidden` = "true",
      bsicons::bs_icon("circle-fill")
    ),
    shiny::tags$div(
      class = "story-kicker",
      shiny::tags$span(class = "feed-name", entry$feed_title),
      shiny::tags$span(
        class = "story-time",
        if (is_read && selected) {
          shiny::tags$span(class = "story-state", "Read")
        } else {
          NULL
        },
        if (isTRUE(entry$starred)) {
          shiny::tags$span(class = "star-glyph", "\u2605")
        } else {
          NULL
        },
        format_story_time(entry$published_at)
      )
    ),
    shiny::tags$h3(entry$title),
    shiny::tags$p(entry$summary %||% "")
  )
}

empty_story_list <- function(view, feed_title = NULL) {
  scoped_feed <- !is.null(feed_title) && nzchar(feed_title)
  state <- switch(
    view,
    unread = list(
      title = "You're all caught up",
      body = if (scoped_feed) {
        paste0("No unread stories from ", feed_title, ".")
      } else {
        "New stories will land here after the next refresh."
      }
    ),
    starred = list(
      title = "No starred stories yet",
      body = "Press F while reading to keep favorites close."
    ),
    saved = list(
      title = "Nothing saved yet",
      body = "Press S while reading to build a return-later list."
    ),
    today = list(
      title = "Nothing published today",
      body = if (scoped_feed) {
        paste0("No stories from ", feed_title, " were published today.")
      } else {
        "Stories published today will appear here."
      }
    ),
    week = list(
      title = "Nothing published this week",
      body = if (scoped_feed) {
        paste0("No stories from ", feed_title, " were published this week.")
      } else {
        "Stories published since Monday will appear here."
      }
    ),
    month = list(
      title = "Nothing published this month",
      body = if (scoped_feed) {
        paste0("No stories from ", feed_title, " were published this month.")
      } else {
        "Stories published this calendar month will appear here."
      }
    ),
    list(
      title = "No stories yet",
      body = "Add a feed to start your reading queue."
    )
  )

  shiny::tags$div(
    class = "empty-list",
    shiny::tags$p(class = "empty-eyebrow", "Reading queue"),
    shiny::tags$h3(state$title),
    shiny::tags$p(state$body)
  )
}
