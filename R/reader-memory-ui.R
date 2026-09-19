reader_memory_ui <- function(id) {
  shiny::actionButton(shiny::NS(id)("open"), "Reader Memory", class = "btn-sm")
}

reader_memory_server <- function(
  id,
  access,
  document,
  changed = function() NULL,
  demo = FALSE
) {
  shiny::moduleServer(id, function(input, output, session) {
    pending <- shiny::reactiveVal(NULL)
    shown <- shiny::reactiveVal(NULL)
    status <- shiny::reactiveVal(NULL)
    choices <- function() {
      records <- reader_memory_list(access)
      values <- vapply(records, `[[`, character(1), "memory_id")
      labels <- vapply(
        records,
        function(x) {
          paste0(if (x$archived) "Archived: " else "", substr(x$text, 1L, 65L))
        },
        character(1)
      )
      c("New memory" = "", stats::setNames(values, labels))
    }
    handle <- function(code) {
      tryCatch(code(), error = function(e) {
        status(
          if (
            inherits(e, c("rill_memory_unavailable", "graft_artifact_error"))
          ) {
            "The memory or source changed or is unavailable. Review it again."
          } else {
            "Memory could not be saved. Try again."
          }
        )
        NULL
      })
    }
    shiny::observeEvent(input$open, {
      options <- handle(choices)
      if (is.null(options)) {
        return()
      }
      pending(NULL)
      shown(NULL)
      status(NULL)
      shiny::showModal(shiny::modalDialog(
        title = "Reader Memory",
        shiny::tags$p(
          if (demo) {
            "Demo memory lasts only while this app is running."
          } else {
            "Memory is private to you and is created only when you accept it."
          }
        ),
        shiny::selectInput(
          session$ns("selected"),
          "Inspect or revise",
          choices = options
        ),
        shiny::uiOutput(session$ns("retained")),
        shiny::radioButtons(
          session$ns("kind"),
          "Remember as",
          c("Preference" = "preference", "Interpretation" = "interpretation")
        ),
        shiny::textAreaInput(
          session$ns("text"),
          "What should Rill remember?",
          rows = 3
        ),
        shiny::conditionalPanel(
          sprintf("input['%s'] === 'interpretation'", session$ns("kind")),
          shiny::textAreaInput(
            session$ns("quote"),
            "Exact passage from the selected reading copy",
            rows = 3
          ),
          shiny::tags$p(
            "An interpretation goes beyond the source. The passage anchors it; it does not prove it."
          )
        ),
        shiny::actionButton(session$ns("review"), "Review memory"),
        shiny::uiOutput(session$ns("preview")),
        shiny::uiOutput(session$ns("state_action")),
        shiny::textOutput(session$ns("status")),
        footer = shiny::modalButton("Close"),
        size = "l"
      ))
    })
    shiny::observeEvent(
      input$selected,
      {
        pending(NULL)
        selected <- input$selected %||% ""
        value <- if (nzchar(selected)) {
          handle(function() reader_memory_read(access, selected))
        } else {
          NULL
        }
        shown(value)
        shiny::updateRadioButtons(
          session,
          "kind",
          selected = value$kind %||% "preference"
        )
        shiny::updateTextAreaInput(session, "text", value = value$text %||% "")
        shiny::updateTextAreaInput(
          session,
          "quote",
          value = if (length(value$evidence)) value$evidence[[1L]]$quote else ""
        )
      },
      ignoreNULL = FALSE
    )
    shiny::observeEvent(
      list(input$text, input$kind, input$quote, input$selected),
      {
        pending(NULL)
      },
      priority = 100,
      ignoreInit = TRUE
    )
    shiny::observeEvent(input$review, {
      pending(NULL)
      status(NULL)
      proposal <- handle(function() {
        source <- if (identical(input$kind, "interpretation")) {
          document()
        } else {
          NULL
        }
        reader_memory_propose(
          access,
          input$text,
          input$kind,
          document_id = source$document_id,
          quote = if (identical(input$kind, "interpretation")) {
            input$quote
          } else {
            NULL
          },
          memory_id = if (nzchar(input$selected %||% "")) {
            input$selected
          } else {
            NULL
          }
        )
      })
      pending(proposal)
    })
    output$preview <- shiny::renderUI({
      proposal <- pending()
      if (is.null(proposal)) {
        return(NULL)
      }
      shiny::tagList(
        shiny::tags$h4("Review before accepting"),
        shiny::tags$p(proposal$text),
        if (!is.null(proposal$anchor)) {
          shiny::tags$blockquote(proposal$anchor$quote)
        },
        shiny::tags$p(
          "This may be used as Reader Context in later conversations."
        ),
        shiny::actionButton(
          session$ns("accept"),
          "Accept as Reader Memory",
          class = "btn-primary"
        )
      )
    })
    output$retained <- shiny::renderUI({
      value <- shown()
      if (is.null(value)) {
        return(NULL)
      }
      shiny::tagList(
        shiny::tags$p(value$text),
        if (length(value$evidence)) {
          shiny::tagList(
            shiny::tags$p(value$evidence[[1L]]$title),
            shiny::tags$blockquote(value$evidence[[1L]]$quote)
          )
        },
        shiny::tags$p(
          if (value$archived) {
            "Archived: excluded from automatic use."
          } else {
            "Available as Reader Context."
          }
        )
      )
    })
    output$state_action <- shiny::renderUI({
      value <- shown()
      if (is.null(value)) {
        return(NULL)
      }
      shiny::actionButton(
        session$ns(if (value$archived) "restore" else "archive"),
        if (value$archived) "Restore memory" else "Archive memory"
      )
    })
    output$status <- shiny::renderText(status())
    refresh <- function(memory_id, message) {
      pending(NULL)
      shown(reader_memory_read(access, memory_id))
      shiny::updateSelectInput(
        session,
        "selected",
        choices = choices(),
        selected = memory_id
      )
      status(message)
      changed()
    }
    shiny::observeEvent(input$accept, {
      proposal <- pending()
      shiny::req(!is.null(proposal))
      saved <- handle(function() reader_memory_accept(access, proposal))
      if (!is.null(saved)) refresh(saved$memory_id, "Remembered.")
    })
    transition <- function(action) {
      value <- shown()
      shiny::req(!is.null(value))
      result <- handle(function() {
        operation <- if (action == "archive") {
          reader_memory_archive
        } else {
          reader_memory_restore
        }
        operation(
          access,
          value$memory_id,
          value$basis$decision,
          rill_id(action, value$memory_id, value$basis$decision)
        )
      })
      if (!is.null(result)) {
        refresh(
          value$memory_id,
          if (action == "archive") "Archived." else "Restored."
        )
      }
    }
    shiny::observeEvent(input$archive, transition("archive"))
    shiny::observeEvent(input$restore, transition("restore"))
    list(pending = pending, shown = shown, status = status)
  })
}
