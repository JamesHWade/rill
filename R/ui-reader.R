rill_copy_text_ui <- function(label) {
  shiny::tagList(
    shiny::tags$button(
      type = "button",
      class = "btn btn-sm btn-outline-secondary",
      `data-rill-copy-text` = "",
      label
    ),
    shiny::tags$span(class = "rill-copy-status", role = "status")
  )
}

reader_focus_controls <- function() {
  shiny::tags$div(
    class = "reader-focus-controls",
    `aria-label` = "Pane layout",
    role = "group",
    shiny::tags$button(
      type = "button",
      `data-rill-pane-focus` = "reading",
      "Focus Reading"
    ),
    shiny::tags$button(
      type = "button",
      `data-rill-pane-focus` = "ask",
      "Focus Ask Rill"
    ),
    shiny::tags$button(
      type = "button",
      `data-rill-pane-focus` = "restore",
      disabled = NA,
      "Restore layout"
    )
  )
}

reader_pane_ui <- function(config) {
  bslib::layout_sidebar(
    shiny::tags$div(
      id = "rill-primary-surface",
      role = "region",
      class = "reader-scroll",
      tabindex = "-1",
      `aria-label` = "Reading surface",
      reader_focus_controls(),
      shiny::uiOutput("reader_header", container = shiny::tags$div),
      shiny::uiOutput("reader_body", container = shiny::tags$div)
    ),
    sidebar = bslib::sidebar(
      shiny::tags$header(
        class = "reader-agent-header",
        reader_focus_controls(),
        shiny::tags$div(
          class = "reader-agent-kicker",
          shiny::tags$p(class = "eyebrow", "Ask Rill")
        ),
        shiny::tags$h2(
          id = "reader-agent-title",
          "Ask about this story"
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
            "I answer from this story's text and say when I go beyond it.",
            "Try asking for a summary, the main argument, or the evidence",
            "behind a claim."
          )
        ),
        placeholder = "Ask about this story\u2026",
        height = "100%",
        fill = TRUE,
        enable_cancel = TRUE,
        footer = shiny::tags$span(
          paste0(
            if (isTRUE(config$reader_memory_enabled)) {
              "Your question, the story's text, and your accepted Reader Memory go to "
            } else {
              "Your question and the story's text go to "
            },
            rill_agent_data_destination(
              config$agent_model,
              config$agent_base_url %||% ""
            ),
            "."
          )
        )
      ),
      shiny::uiOutput(
        "reader_feedback_actions",
        class = "reader-feedback-actions"
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

reader_article_header_ui <- function(
  entry,
  document,
  can_prepare = TRUE,
  preparation = "missing"
) {
  is_fallback <- identical(document$acquisition_method, "feed_fallback")
  reading_minutes <- max(
    1L,
    ceiling(as.integer(document$word_count %||% 0L) / 225)
  )
  source_name <- document$site %||% entry$feed_title
  source_url <- rill_document_original_source_url(document)
  if (is.na(source_url)) {
    source_url <- entry$url
  }

  copy <- reading_copy_label(document)
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
      orientation_open_button("reader-action"),
      shiny::tags$button(
        type = "button",
        class = "reader-action reader-previous",
        onclick = "rillMoveStory(-1)",
        `aria-keyshortcuts` = "k",
        "Previous"
      ),
      shiny::tags$button(
        type = "button",
        class = "reader-action reader-next",
        onclick = "rillMoveStory(1)",
        `aria-keyshortcuts` = "j",
        "Next"
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
      `aria-label` = paste0(
        copy$label,
        ". Details about this copy are at the end of the story."
      ),
      bsicons::bs_icon("file-earmark-text"),
      shiny::tags$strong(copy$label),
      shiny::tags$span(
        class = "article-copy-producer",
        if (is_fallback) {
          switch(
            preparation,
            ready = "Full article ready",
            running = "Fetching the full article\u2026",
            waiting = "Full article unavailable for now",
            copy$detail
          )
        } else {
          copy$detail
        }
      ),
      if (is_fallback && isTRUE(can_prepare) && isTRUE(entry$library_access)) {
        shiny::actionButton(
          if (identical(preparation, "ready")) {
            "use_prepared_article"
          } else {
            "prepare_article"
          },
          if (identical(preparation, "ready")) {
            "Load full article"
          } else {
            "Prepare full article"
          },
          icon = bsicons::bs_icon("cloud-arrow-down"),
          class = "btn-prepare-today",
          disabled = preparation %in% c("running", "waiting"),
          title = if (identical(preparation, "ready")) {
            "Replace the feed copy with the full article"
          } else {
            "Fetch the full article in the background"
          }
        )
      }
    )
  )
}

reader_document_ui <- function(
  document,
  entry_id,
  selection_surface,
  open_id = NULL
) {
  source_url <- rill_document_original_source_url(document)
  captured_at <- format(
    as.POSIXct(document$captured_at, tz = "UTC"),
    "%Y-%m-%d %H:%M UTC",
    tz = "UTC"
  )
  copy <- reading_copy_label(document)

  provenance <- bslib::accordion(
    bslib::accordion_panel(
      shiny::tagList(
        bsicons::bs_icon("info-circle"),
        "About this copy"
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
        shiny::tags$dt("Copy"),
        shiny::tags$dd(paste(copy$label, "\u00b7", copy$detail)),
        shiny::tags$dt("Saved"),
        shiny::tags$dd(captured_at),
        shiny::tags$dt("Copy ID"),
        shiny::tags$dd(shiny::tags$code(document$document_id))
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
    `data-open-id` = open_id,
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
    shiny::tags$span("Answers use this story"),
    shiny::tags$strong(document$title %||% entry$title),
    shiny::tags$small(document$site %||% entry$feed_title)
  )
}

reading_copy_label <- function(document) {
  producer <- document$producer %||% "an unknown tool"
  switch(
    document$acquisition_method %||% "",
    feed_fallback = list(label = "Feed copy", detail = "May be an excerpt"),
    web_extraction = list(
      label = "Extracted copy",
      detail = paste("Made with", producer)
    ),
    browser_capture = list(
      label = "Captured copy",
      detail = paste("Sent by", producer)
    ),
    sample = list(label = "Demo copy", detail = "Bundled with Rill"),
    list(label = "Saved copy", detail = paste("Made by", producer))
  )
}

reader_agent_status_ui <- function(run, pending = NULL) {
  if (!is.null(pending)) {
    return(shiny::tags$p(
      class = "reader-agent-run-status",
      role = "status",
      `aria-live` = "polite",
      bsicons::bs_icon("hourglass-split"),
      "Pausing Orientation to answer your question\u2026"
    ))
  }
  if (is.null(run) || identical(run$status, "completed")) {
    return(NULL)
  }
  if (run$status %in% c("pending", "running")) {
    return(shiny::tags$p(
      class = "reader-agent-run-status",
      role = "status",
      `aria-live` = "polite",
      bsicons::bs_icon("stars"),
      "Reading the story\u2026"
    ))
  }
  if (identical(run$status, "cancelling")) {
    return(shiny::tags$p(
      class = "reader-agent-run-status",
      role = "status",
      `aria-live` = "polite",
      bsicons::bs_icon("stop-circle"),
      "Stopping the response\u2026"
    ))
  }

  shiny::tags$div(
    class = "reader-agent-retry",
    role = "alert",
    shiny::tags$p("The answer stopped before it finished."),
    shiny::actionButton(
      "retry_agent_run",
      "Retry",
      class = "btn-sm"
    )
  )
}
