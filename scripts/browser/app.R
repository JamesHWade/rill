# Start from the repository root: Rscript scripts/browser/app.R
Sys.unsetenv(c("DATABASE_URL", "OPENAI_API_KEY", "ANTHROPIC_API_KEY"))
Sys.setenv(RILL_IDENTITY_MODE = "local")
pkgload::load_all(helpers = FALSE)
config <- rill_config()
stopifnot(config$demo_mode)
shiny::addResourcePath("rill-assets", rill_package_file("app", "www"))
stress_store <- function(store) {
  feeds <- store$memory$feeds[rep(1L, 116L), ]
  feeds$feed_id <- paste0("audit-feed-", seq_len(116L))
  feeds$title <- paste(
    "A deliberately long publisher name for layout checks",
    seq_len(116L)
  )
  feeds$feed_url <- paste0("https://example.com/feed/", seq_len(116L))
  store$memory$feeds <- feeds
  subscriptions <- store$memory$subscriptions[rep(1L, 116L), ]
  subscriptions$feed_id <- feeds$feed_id
  store$memory$subscriptions <- subscriptions
  entries <- store$memory$entries[rep(1L, 150L), ]
  entries$entry_id <- paste0("audit-entry-", seq_len(150L))
  entries$external_id <- entries$entry_id
  entries$feed_id <- rep(feeds$feed_id, length.out = 150L)
  entries$url <- paste0("https://example.com/article/", seq_len(150L))
  entries$title <- paste(
    "A long article title about keeping source material readable on a small screen",
    seq_len(150L)
  )
  store$memory$entries <- entries
  store$memory$documents <- list()
  store$memory$document_heads <- character()
  markdown <- paste(
    rep(
      "A paragraph of synthetic source text for scrolling, reading, and layout checks. No private production material is used.",
      80L
    ),
    collapse = "\n\n"
  )
  markdown <- paste0(
    markdown,
    "\n\n```text\n",
    paste(rep("long_code_value_", 30L), collapse = ""),
    "\n```\n"
  )
  for (index in seq_len(150L)) {
    store_save_document(
      store,
      new_rill_document(
        entry_id = entries$entry_id[[index]],
        source_url = entries$url[[index]],
        markdown = markdown,
        acquisition_method = "sample",
        producer = "browser-fixture",
        title = entries$title[[index]],
        site = "Synthetic publisher"
      )
    )
  }
  store
}

app <- shiny::shinyApp(
  ui = function(request) rill_ui(config),
  server = function(input, output, session) {
    query <- shiny::parseQueryString(
      shiny::isolate(session$clientData$url_search) %||% ""
    )
    if (isTRUE(query$delay %in% c("3", "17"))) {
      Sys.sleep(as.numeric(query$delay))
    }
    if (isTRUE(query$access %in% c("pending", "denied"))) {
      identity_show_denied_modal(config, query$access)
      return(invisible(NULL))
    }
    store <- rill_store(config)
    if (
      grepl(
        "stress",
        shiny::isolate(session$clientData$url_search) %||% "",
        fixed = TRUE
      )
    ) {
      store <- stress_store(store)
    }
    shiny::observeEvent(input$audit_error, {
      shiny::showNotification(
        "The test request failed. Please try again.",
        type = "error",
        duration = NULL
      )
    })
    shiny::observeEvent(input$audit_disconnect, session$close())
    session$onSessionEnded(function() rill_store_close(store))
    rill_server(config, store)(input, output, session)
    if (identical(query$agent, "fixture")) {
      agent_status <- shiny::reactiveVal(NULL)
      output$reader_agent_status <- shiny::renderUI({
        reader_agent_status_ui(agent_status())
      })
      shiny::outputOptions(
        output,
        "reader_agent_status",
        suspendWhenHidden = FALSE
      )
      shiny::observeEvent(input$audit_agent_status, {
        agent_status(list(status = input$audit_agent_status))
      })
      shiny::observeEvent(input$audit_agent_stream, {
        agent_status(list(status = "running"))
        session$onFlushed(
          function() {
            response <- coro::async_generator(function() {
              coro::yield("Interpretation: ")
              coro::await(promises::promise(function(resolve, reject) {
                later::later(function() resolve(NULL), 5)
              }))
              coro::yield(
                "This is a synthetic response for browser validation."
              )
            })()
            promises::then(
              append_reader_chat(response, session),
              onFulfilled = function(value) {
                agent_status(list(status = "completed"))
              }
            )
          },
          once = TRUE
        )
      })
    }
  }
)
shiny::runApp(
  app,
  host = "127.0.0.1",
  port = as.integer(Sys.getenv("RILL_BROWSER_PORT", "3876")),
  launch.browser = FALSE
)
