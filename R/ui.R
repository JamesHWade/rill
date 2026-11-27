rill_ui <- function(config) {
  bslib::page_fillable(
    lang = "en",
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
      shiny::includeScript(rill_package_file("app", "www", "queue.js")),
      shiny::includeCSS(rill_package_file("app", "www", "styles.css"))
    ),
    rill_skip_link_ui(),
    rill_system_status_ui(),
    queue_notice_ui(),
    bslib::as_fill_carrier(
      shiny::tags$main(
        id = "rill-app",
        class = "app-shell",
        `data-compact-surface` = "queue",
        `data-operational-telemetry` = tolower(as.character(isTRUE(
          config$telemetry_enabled
        ))),
        `aria-busy` = "true",
        compact_app_bar_ui(config),
        compact_queue_navigation_ui(),
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
