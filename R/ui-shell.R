rill_modal_dialog <- function(title, ..., id = "rill-modal") {
  title_id <- paste0(id, "-title")
  modal <- shiny::modalDialog(
    title = shiny::tags$span(id = title_id, title),
    ...
  )
  modal <- htmltools::tagQuery(modal)$find(".modal-dialog")$addClass(
    "modal-dialog-scrollable"
  )$reset()$find(".modal-header")$append(
    shiny::tags$button(
      type = "button",
      class = "btn-close",
      `data-bs-dismiss` = "modal",
      `data-dismiss` = "modal",
      `aria-label` = "Close"
    )
  )$allTags()
  htmltools::tagAppendAttributes(
    modal,
    role = "dialog",
    `aria-modal` = "true",
    `aria-labelledby` = title_id
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
