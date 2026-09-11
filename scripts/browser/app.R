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
    if (identical(query$orientation, "themes-only")) {
      orientation <- store_get_orientation(store, config$actor_id)
      orientation$cards <- list()
      store$memory$orientations[[config$actor_id]] <- orientation
    }
    if (identical(query$feedback, "fixture")) {
      for (reader_id in c(config$actor_id, "feedback-other-reader")) {
        store_ensure_reader(store, reader_id)
        run <- store_start_agent_run(
          store,
          reader_id,
          "question",
          "feedback-fixture",
          pinned_inputs = list(
            question = "Explain the source boundary.",
            model = "fixture-model",
            policy_version = "fixture-policy",
            document_id = if (
              isTRUE(query$resume %in% c("1", "unsubscribed")) &&
                identical(reader_id, config$actor_id)
            ) {
              names(store$memory$documents)[[1L]]
            }
          ),
          worker_id = "fixture-worker"
        )
        store_claim_agent_run(
          store,
          reader_id,
          run$run_id,
          "fixture-worker",
          lease_expires_at = Sys.time() + 120
        )
        store_record_agent_run_response(
          store,
          reader_id,
          run$run_id,
          "fixture-worker",
          if (identical(reader_id, config$actor_id)) {
            paste(
              "\n\n## Source boundary",
              "Interpretation: keep source material separate from generated explanation.",
              "**Source evidence** remains separate from this explanation.",
              paste(
                rep(
                  "A retained paragraph for checking long answer review.",
                  40L
                ),
                collapse = "\n\n"
              ),
              strrep("unbroken", 40L),
              '<a id="feedback_withdraw" class="action-button" href="#">Inspect source</a>',

              sep = "\n\n"
            )
          } else {
            "PRIVATE_OTHER_READER_OUTPUT"
          }
        )
        run <- store_finish_agent_run(
          store,
          reader_id,
          run$run_id,
          "fixture-worker",
          "completed",
          finished_at = Sys.time() - 3
        )
        if (!identical(reader_id, config$actor_id)) {
          feedback_save(
            store,
            reader_id,
            feedback_target(store, reader_id, "question", run),
            "helpful"
          )
        }
      }
      if (identical(query$resume, "unsubscribed")) {
        document <- store$memory$documents[[1L]]
        entry <- store_get_entry(store, config$actor_id, document$entry_id)
        store_unsubscribe_feed(store, config$actor_id, entry$feed_id)
      }
    }
    if (
      grepl(
        "stress",
        shiny::isolate(session$clientData$url_search) %||% "",
        fixed = TRUE
      )
    ) {
      store <- stress_store(store)
    }
    if (identical(query$timeline, "fixture")) {
      store$memory$entries$preview_image_url[[
        1L
      ]] <- "https://example.org/timeline-river.png"
      store$memory$entries$preview_image_alt[[
        1L
      ]] <- "River through a wooded valley"
      store$memory$entries$preview_image_url[[
        3L
      ]] <- "https://example.org/missing-image.png"
      store$memory$entries$title[[1L]] <- "Small rivers, large consequences"
      store$memory$entries$summary[[
        1L
      ]] <- "What changes when we follow a river from its headwaters? A field notebook about water, woodland, and the places they connect."
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
    preview_fetch <- if (identical(query$timeline, "fixture")) {
      function(url, session) {
        image <- if (identical(url, "https://example.org/timeline-river.png")) {
          list(
            content = readBin(
              "scripts/browser/fixtures/timeline-river.png",
              "raw",
              n = 4 * 1024^2
            ),
            type = "image/png"
          )
        } else {
          NULL
        }
        promises::promise_resolve(image)
      }
    } else {
      queue_preview_fetch_async
    }
    rill_server(config, store, preview_fetch = preview_fetch)(
      input,
      output,
      session
    )
    if (identical(query$tools, "fixture")) {
      shiny::observeEvent(input$audit_document_result, {
        document <- sample_rill_data()$documents[[2L]]
        request <- ellmer::ContentToolRequest(
          id = "fixture-tool",
          name = "read_current_document",
          arguments = list()
        )
        result <- ellmer::ContentToolResult(
          value = rill_document_tool(document)(),
          request = request
        )
        stream <- coro::async_generator(function() {
          coro::yield(request)
          coro::yield(result)
          coro::yield(
            "Interpretation: this synthetic tool result stays attached to the source returned for this question."
          )
        })()
        append_reader_chat(
          track_reader_agent_stream(stream, function(text) invisible(NULL)),
          session
        )
      })
    }
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
