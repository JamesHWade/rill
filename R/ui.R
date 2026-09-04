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
        sizes = "32x32",
        href = "rill-assets/favicon-32.png"
      ),
      shiny::tags$link(
        rel = "apple-touch-icon",
        sizes = "180x180",
        href = "rill-assets/apple-touch-icon.png"
      ),
      shiny::tags$title("Rill \u2014 personal reader"),
      shiny::includeScript(rill_package_file("app", "www", "app.js")),
      shiny::includeCSS(rill_package_file("app", "www", "styles.css"))
    ),
    rill_skip_link_ui(),
    rill_system_status_ui(),
    bslib::as_fill_carrier(
      shiny::tags$main(
        id = "rill-app",
        class = "app-shell",
        `data-compact-surface` = "queue",
        `aria-busy` = "true",
        compact_app_bar_ui(config),
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

rill_skip_link_ui <- function() {
  shiny::tags$a(
    class = "rill-skip-link",
    href = "#rill-primary-surface",
    "Skip to primary content"
  )
}

rill_system_status_ui <- function() {
  shiny::tags$aside(
    id = "rill-system-status",
    class = "rill-system-status is-starting",
    `data-state` = "starting",
    role = "status",
    `aria-live` = "polite",
    `aria-atomic` = "true",
    shiny::tags$span(
      class = "rill-system-status-icon",
      `aria-hidden` = "true",
      bsicons::bs_icon("cloud")
    ),
    shiny::tags$span(
      class = "rill-system-status-copy",
      shiny::tags$strong(
        id = "rill-system-status-title",
        "Opening Rill"
      ),
      shiny::tags$span(
        id = "rill-system-status-detail",
        "Connecting to your Library\u2026"
      )
    ),
    shiny::tags$button(
      id = "rill-system-status-action",
      type = "button",
      class = "rill-system-status-action",
      onclick = "rillRecoverConnection()",
      hidden = TRUE,
      "Reload Rill"
    )
  )
}

compact_app_bar_ui <- function(config) {
  shiny::tags$header(
    class = "compact-app-bar",
    shiny::tags$button(
      type = "button",
      class = "compact-app-action compact-library-trigger",
      onclick = "rillOpenLibrary()",
      `aria-controls` = "navigation_sidebar",
      `aria-expanded` = "false",
      bsicons::bs_icon("collection"),
      shiny::tags$span("Library")
    ),
    shiny::tags$div(
      class = "compact-app-brand",
      rill_otter_mark("compact-brand-mark"),
      shiny::tags$strong(config$app_name)
    ),
    shiny::tags$button(
      type = "button",
      class = "compact-app-action compact-reader-return",
      onclick = "rillReturnToReading()",
      hidden = TRUE,
      bsicons::bs_icon("book"),
      shiny::tags$span("Reading")
    )
  )
}

navigation_sidebar_ui <- function(config) {
  bslib::sidebar(
    shiny::tags$header(
      class = "compact-library-header",
      shiny::tags$button(
        type = "button",
        class = "compact-surface-back",
        onclick = "rillCloseLibrary()",
        bsicons::bs_icon("arrow-left"),
        shiny::tags$span("Back")
      ),
      shiny::tags$h1("Library")
    ),
    shiny::tags$div(
      class = "brand",
      rill_otter_mark("brand-mark"),
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
    shiny::uiOutput(
      "feed_nav",
      container = shiny::tags$nav,
      class = "feed-list",
      `aria-label` = "Feeds"
    ),
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
    open = list(desktop = "open", mobile = "closed"),
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
        class = "queue-title",
        shiny::tags$p(class = "eyebrow", "Reading queue"),
        shiny::uiOutput("list_title", container = shiny::tags$div)
      ),
      htmltools::tagQuery(
        bslib::toolbar(
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
            htmltools::tagQuery(
              shiny::selectInput(
                "story_sort",
                label = NULL,
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
            )$find("select")$addAttrs(
              `aria-label` = "Sort stories"
            )$allTags()
          ),
          shiny::uiOutput("story_count", container = shiny::tags$div),
          align = "right",
          gap = "8px"
        )
      )$addClass("queue-controls")$allTags()
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
    open = list(desktop = "open", mobile = "closed"),
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
      id = "rill-primary-surface",
      class = "reader-scroll",
      tabindex = "-1",
      `aria-label` = "Reading surface",
      shiny::uiOutput("reader_header", container = shiny::tags$div),
      shiny::uiOutput("reader_body", container = shiny::tags$div)
    ),
    sidebar = bslib::sidebar(
      shiny::tags$header(
        class = "reader-agent-header",
        shiny::tags$div(
          class = "reader-agent-kicker",
          shiny::tags$p(class = "eyebrow", "Ask Rill"),
          shiny::tags$span(class = "reader-agent-badge", "Source-bound")
        ),
        shiny::tags$h2(
          id = "reader-agent-title",
          "Ask about the selected story"
        ),
        shiny::uiOutput(
          "reader_agent_context",
          container = shiny::tags$div,
          class = "reader-agent-context-slot"
        )
      ),
      shiny::uiOutput(
        "reader_agent_status",
        container = shiny::tags$div,
        class = "reader-agent-status-slot"
      ),
      shinychat::chat_ui(
        "reader_chat",
        greeting = shinychat::chat_greeting(
          paste(
            "I answer from the selected reading copy and label inference",
            "as interpretation. Ask for a summary, a key claim, or a",
            "connection."
          )
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
      open = list(desktop = "closed", mobile = "closed"),
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

reader_article_header_ui <- function(entry, document) {
  reading_minutes <- max(
    1L,
    ceiling(as.integer(document$word_count %||% 0L) / 225)
  )
  source_name <- document$site %||% entry$feed_title
  source_url <- rill_document_original_source_url(document)
  if (is.na(source_url)) {
    source_url <- entry$url
  }

  actions <- htmltools::tagQuery(
    bslib::toolbar(
      shiny::tags$button(
        type = "button",
        class = "reader-action mobile-back",
        `aria-keyshortcuts` = "Escape",
        onclick = "rillOpenQueue()",
        bsicons::bs_icon("arrow-left"),
        "Queue"
      ),
      if (isTRUE(entry$library_access)) {
        shiny::tagList(
          if (
            !is.na(entry$read_at %||% NA_character_) &&
              nzchar(as.character(entry$read_at %||% ""))
          ) {
            mark_unread_button()
          } else {
            NULL
          },
          shiny::actionButton(
            "toggle_star",
            shiny::tagList(
              bsicons::bs_icon(
                if (isTRUE(entry$starred)) "star-fill" else "star"
              ),
              if (isTRUE(entry$starred)) "Starred" else "Star"
            ),
            `aria-keyshortcuts` = "f",
            `aria-pressed` = if (isTRUE(entry$starred)) "true" else "false",
            class = paste(
              "reader-action",
              if (isTRUE(entry$starred)) "is-active"
            )
          ),
          shiny::actionButton(
            "toggle_save",
            shiny::tagList(
              bsicons::bs_icon(
                if (isTRUE(entry$saved)) "bookmark-fill" else "bookmark"
              ),
              if (isTRUE(entry$saved)) "Saved" else "Save"
            ),
            `aria-keyshortcuts` = "s",
            `aria-pressed` = if (isTRUE(entry$saved)) "true" else "false",
            class = paste(
              "reader-action",
              if (isTRUE(entry$saved)) "is-active"
            )
          )
        )
      } else {
        NULL
      },
      shiny::tags$button(
        type = "button",
        class = "reader-action reader-agent-trigger",
        `aria-controls` = "reader_agent_sidebar",
        `aria-expanded` = "false",
        onclick = "rillOpenAskRill(this)",
        bsicons::bs_icon("chat-left-text"),
        "Ask Rill"
      ),
      shiny::tags$a(
        class = "reader-action original-link",
        href = source_url,
        target = "_blank",
        rel = "noopener noreferrer",
        `aria-keyshortcuts` = "o",
        onclick = "rillTrack('open_original')",
        bsicons::bs_icon("box-arrow-up-right"),
        "Original"
      ),
      shiny::tags$span(
        class = "shortcut-hint",
        shiny::tags$kbd("J"),
        "/",
        shiny::tags$kbd("K"),
        " navigate \u00b7 ",
        shiny::tags$kbd("O"),
        " original \u00b7 ",
        shiny::tags$kbd("S"),
        " save \u00b7 ",
        shiny::tags$kbd("F"),
        " star"
      ),
      align = "left",
      gap = "6px"
    )
  )$addClass("article-actions")$allTags()

  shiny::tags$header(
    class = "article-header",
    actions,
    shiny::tags$p(
      class = "article-source",
      shiny::tags$span("Source"),
      shiny::tags$a(
        href = source_url,
        target = "_blank",
        rel = "noopener noreferrer",
        source_name
      )
    ),
    shiny::tags$h1(document$title %||% entry$title),
    shiny::tags$div(
      class = "article-byline",
      if (
        !is.na(document$author %||% NA_character_) &&
          nzchar(document$author %||% "")
      ) {
        shiny::tags$span(document$author)
      },
      shiny::tags$span(paste(reading_minutes, "min read")),
      shiny::tags$span(format_story_time(entry$published_at))
    ),
    shiny::tags$div(
      class = "article-copy-status",
      `aria-label` = paste(
        "Stored reading copy prepared by",
        document$producer %||% "feed fallback",
        "with details below"
      ),
      bsicons::bs_icon("file-earmark-text"),
      shiny::tags$strong("Stored reading copy"),
      shiny::tags$span(
        class = "article-copy-producer",
        paste("Prepared by", document$producer %||% "feed fallback")
      )
    )
  )
}

reader_document_ui <- function(document, entry_id, selection_surface) {
  source_url <- rill_document_original_source_url(document)
  captured_at <- format(
    as.POSIXct(document$captured_at, tz = "UTC"),
    "%Y-%m-%d %H:%M UTC",
    tz = "UTC"
  )
  acquisition <- gsub(
    "_",
    " ",
    document$acquisition_method %||% "unknown",
    fixed = TRUE
  )

  provenance <- bslib::accordion(
    bslib::accordion_panel(
      shiny::tagList(
        bsicons::bs_icon("info-circle"),
        "About this reading copy"
      ),
      shiny::tags$p(
        class = "reading-copy-boundary",
        paste(
          "This stored source copy remains separate from Ask Rill's",
          "interpretation."
        )
      ),
      shiny::tags$p(
        class = "reading-copy-limitations",
        rill_document_limitations(document)
      ),
      shiny::tags$dl(
        class = "reading-copy-metadata",
        shiny::tags$dt("Original source"),
        shiny::tags$dd(
          if (!is.na(source_url)) {
            shiny::tags$a(
              href = source_url,
              target = "_blank",
              rel = "noopener noreferrer",
              source_url
            )
          } else {
            "Unavailable"
          }
        ),
        shiny::tags$dt("Reading copy"),
        shiny::tags$dd(shiny::tags$code(document$document_id)),
        shiny::tags$dt("Prepared"),
        shiny::tags$dd(
          paste(document$producer %||% "feed fallback", "via", acquisition)
        ),
        shiny::tags$dt("Captured"),
        shiny::tags$dd(captured_at)
      ),
      value = "reading-copy-provenance"
    ),
    open = FALSE,
    class = "reading-provenance"
  )

  shiny::tags$article(
    id = "reader-document",
    class = "reader-document",
    `data-entry-id` = entry_id,
    `data-document-id` = document$document_id,
    `data-selection-surface` = selection_surface,
    render_document(document),
    shiny::tags$footer(
      class = "article-footer",
      provenance
    )
  )
}

reader_agent_context_ui <- function(entry, document) {
  shiny::tags$div(
    id = "reader-agent-context",
    class = "reader-agent-context",
    shiny::tags$span("Grounded in this reading copy"),
    shiny::tags$strong(document$title %||% entry$title),
    shiny::tags$small(document$site %||% entry$feed_title)
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
      rill_reading_otter("welcome-otter"),
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
      rill_reading_otter("welcome-otter"),
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
        shiny::tags$p(
          class = "eyebrow",
          "Orientation \u00b7 Rill-guided reading"
        ),
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
    shiny::tags$aside(
      class = "orientation-disclosure",
      `aria-label` = "How Orientation uses sources",
      bsicons::bs_icon("signpost-split"),
      shiny::tags$div(
        shiny::tags$strong("A guided path, not a source"),
        shiny::tags$p(
          paste(
            "Rill generates the question, path, and interpretation.",
            "Source Documents and quoted evidence remain separate and",
            "inspectable."
          )
        )
      )
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
      shiny::tags$p(
        class = "orientation-card-kind",
        paste("Reading step", number, "\u00b7", frame)
      ),
      shiny::tags$p(class = "orientation-source-label", "Source Document"),
      shiny::tags$div(
        class = "orientation-source",
        shiny::tags$span(document$site %||% entry$feed_title),
        shiny::tags$strong(document$title %||% entry$title),
        shiny::tags$span(format_story_time(entry$published_at))
      ),
      shiny::tags$p(
        class = "orientation-interpretation-label",
        "Rill interpretation"
      ),
      shiny::tags$p(
        class = "orientation-interpretation",
        card$interpretation
      ),
      shiny::tags$p(
        class = "orientation-why",
        shiny::tags$strong("Path rationale \u00b7 "),
        card$why_now
      ),
      shiny::tags$details(
        class = "orientation-evidence",
        shiny::tags$summary(
          paste0("Source evidence and provenance [", number, "]")
        ),
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
            "Read the anchor source"
          } else {
            "Read this source"
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

rill_otter_mark <- function(class) {
  shiny::tags$span(
    class = class,
    `aria-hidden` = "true",
    shiny::tags$img(
      class = "brand-logo",
      src = "rill-assets/rill-otter-mark.png",
      alt = "",
      width = 48,
      height = 48
    )
  )
}

rill_reading_otter <- function(class) {
  shiny::tags$img(
    class = class,
    src = "rill-assets/rill-otter-reading.png",
    alt = "",
    `aria-hidden` = "true",
    width = 200,
    height = 200
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
    rill_reading_otter("empty-otter"),
    shiny::tags$p(class = "empty-eyebrow", "Reading queue"),
    shiny::tags$h3(state$title),
    shiny::tags$p(state$body)
  )
}
