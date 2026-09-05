access_requests_owner <- function(adapter, resolution) {
  if (
    !adapter$kind %in% c("auth0", "oidc_proxy") ||
      !identical(resolution$status, "active") ||
      is.null(resolution$reader_id) ||
      !identical(resolution$reader_id, adapter$config$actor_id)
  ) {
    return(FALSE)
  }
  current <- tryCatch(
    adapter$session_status(resolution),
    error = \(error) NULL
  )
  identical(current$status, "active") &&
    identical(current$reader_id, resolution$reader_id)
}

access_requests_server <- function(
  id,
  store,
  adapter,
  resolution,
  session = shiny::getDefaultReactiveDomain()
) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      requests <- shiny::reactiveVal(NULL)
      status <- shiny::reactiveVal(NULL)
      opened <- shiny::reactiveVal(FALSE)

      authorized <- function() {
        if (access_requests_owner(adapter, resolution)) {
          return(TRUE)
        }
        requests(NULL)
        if (shiny::isolate(opened())) {
          shiny::removeModal(session = session)
          opened(FALSE)
        }
        FALSE
      }

      refresh <- function() {
        if (!authorized()) {
          return(invisible(NULL))
        }
        rows <- tryCatch(
          reader_admission_summary(store_list_reader_admissions(
            store,
            "pending"
          )),
          error = function(error) {
            status("Access requests could not be loaded. Try refreshing.")
            NULL
          }
        )
        requests(rows)
      }

      output$launcher <- shiny::renderUI({
        if (!access_requests_owner(adapter, resolution)) {
          return(NULL)
        }
        shiny::actionButton(
          session$ns("open"),
          "Access requests",
          icon = bsicons::bs_icon("person-plus"),
          class = "btn-sm"
        )
      })

      shiny::observeEvent(input$open, {
        if (!authorized()) {
          return()
        }
        status(NULL)
        refresh()
        opened(TRUE)
        shiny::showModal(
          shiny::modalDialog(
            title = "Access requests",
            shiny::tags$p(
              "Approve people you have invited to use Rill. Each approval creates",
              "a separate, empty Library."
            ),
            shiny::actionButton(session$ns("refresh"), "Refresh requests"),
            shiny::uiOutput(session$ns("status"), `aria-live` = "polite"),
            shiny::uiOutput(session$ns("pending")),
            shiny::uiOutput(session$ns("selection")),
            footer = shiny::modalButton("Close"),
            easyClose = TRUE
          ),
          session = session
        )
      })

      shiny::observeEvent(input$refresh, {
        status(NULL)
        refresh()
      })

      output$status <- shiny::renderUI({
        if (!authorized()) {
          return(NULL)
        }
        message <- status()
        if (!is.null(message)) shiny::tags$p(class = "mt-3", message)
      })

      output$pending <- shiny::renderUI({
        if (!authorized()) {
          return(NULL)
        }
        rows <- requests()
        if (is.null(rows)) {
          return(NULL)
        }
        if (!nrow(rows)) {
          return(shiny::tags$p(class = "mt-3", "No pending access requests."))
        }
        labels <- vapply(
          seq_len(nrow(rows)),
          function(index) {
            row <- rows[index, , drop = FALSE]
            paste(
              access_request_profile(row$display_name, "Unnamed Reader"),
              access_request_profile(row$email, "Email unavailable"),
              sep = " \u2014 "
            )
          },
          character(1)
        )
        shiny::selectInput(
          session$ns("request"),
          "Pending requests",
          choices = c(
            "Choose a request" = "",
            stats::setNames(rows$request_id, labels)
          ),
          selected = "",
          selectize = FALSE,
          width = "100%"
        )
      })

      selected_request <- shiny::reactive({
        rows <- requests()
        request_id <- input$request
        if (is.null(rows) || length(request_id) != 1L) {
          return(NULL)
        }
        index <- match(request_id, rows$request_id)
        if (is.na(index)) {
          return(NULL)
        }
        rows[index, , drop = FALSE]
      })

      output$selection <- shiny::renderUI({
        if (!authorized()) {
          return(NULL)
        }
        row <- selected_request()
        if (is.null(row)) {
          return(NULL)
        }
        shiny::tagList(
          shiny::tags$h4(access_request_profile(
            row$display_name,
            "Unnamed Reader"
          )),
          shiny::tags$p(access_request_profile(row$email, "Email unavailable")),
          shiny::tags$dl(
            shiny::tags$dt("First requested"),
            shiny::tags$dd(as.character(row$first_seen_at)),
            shiny::tags$dt("Last sign-in"),
            shiny::tags$dd(as.character(row$last_seen_at))
          ),
          shiny::tags$p(
            "They can reload Rill after approval to open their Library."
          ),
          shiny::actionButton(
            session$ns("approve"),
            "Approve access",
            class = "btn-primary"
          )
        )
      })

      shiny::observeEvent(input$approve, {
        if (!authorized()) {
          return()
        }
        row <- selected_request()
        if (is.null(row)) {
          return()
        }
        result <- tryCatch(
          store_approve_reader_admission(
            store,
            request_id = row$request_id[[1L]],
            responsible_id = resolution$reader_id,
            reason = "Invitation approved in Access requests"
          ),
          error = \(error) NULL
        )
        status(
          if (is.null(result)) {
            "Access could not be approved. Refresh the requests and try again."
          } else {
            "Access approved. The Reader can reload Rill to open their Library."
          }
        )
        refresh()
      })
    },
    session = session
  )
}

access_request_profile <- function(value, fallback) {
  if (is.na(value) || !nzchar(value)) fallback else value
}
