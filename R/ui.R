rill_ui <- function(config) {
  bslib::page_fillable(
    theme = bslib::bs_theme(
      version = 5,
      brand = rill_package_file("app", "_brand.yml")
    ),
    fillable_mobile = TRUE,
    shiny::tags$head(
      shiny::tags$meta(
        name = "viewport",
        content = "width=device-width, initial-scale=1"
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
            reader_pane_ui(),
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
        )
      ),
      choiceValues = c("unread", "all", "starred", "saved"),
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
      appearance_control_ui(),
      feed_tools_ui(),
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

story_sidebar_ui <- function() {
  bslib::sidebar(
    shiny::tags$header(
      class = "pane-header",
      shiny::tags$div(
        shiny::tags$p(class = "eyebrow", "Reading queue"),
        shiny::uiOutput("list_title", container = shiny::tags$div)
      ),
      shiny::uiOutput("story_count", container = shiny::tags$div)
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

reader_pane_ui <- function() {
  shiny::tags$div(
    class = "reader-scroll",
    shiny::uiOutput("reader_header", container = shiny::tags$div),
    shiny::uiOutput("reader_body", container = shiny::tags$div)
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
