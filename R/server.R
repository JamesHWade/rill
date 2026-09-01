rill_server <- function(config, store) {
  force(config)
  force(store)

  function(input, output, session) {
    actor_id <- config$actor_id
    session_id <- rill_id(
      "session",
      actor_id,
      session$token,
      utc_now(),
      stats::runif(1)
    )
    selected_id <- shiny::reactiveVal(NULL)
    selected_position <- shiny::reactiveVal(NA_integer_)
    selected_feed <- shiny::reactiveVal(NULL)
    retained_ids <- shiny::reactiveVal(character())
    retained_context <- shiny::reactiveVal(NULL)
    refresh_tick <- shiny::reactiveVal(0L)
    status_text <- shiny::reactiveVal(NULL)
    status_kind <- shiny::reactiveVal("info")

    bump_refresh <- function() refresh_tick(refresh_tick() + 1L)

    current_context <- function() {
      list(
        view = input$view %||% "unread",
        feed_id = selected_feed(),
        sort = input$story_sort %||% "newest"
      )
    }

    clear_selection <- function(clear_retained = TRUE) {
      selected_id(NULL)
      selected_position(NA_integer_)
      if (clear_retained) {
        retained_ids(character())
        retained_context(NULL)
      }
    }

    retain_entry <- function(entry_id) {
      context <- current_context()
      ids <- retained_ids()
      if (!identical(retained_context(), context)) {
        ids <- character()
      }
      retained_context(context)
      retained_ids(unique(c(ids, entry_id)))
    }

    record_event <- function(
      type,
      entry_id = NULL,
      surface = "reader",
      position = NULL,
      payload = list(),
      event_id = NULL,
      happened_at = NULL
    ) {
      event <- list(
        event_id = event_id %||%
          rill_id("event", session_id, type, utc_now(), stats::runif(1)),
        actor_id = actor_id,
        entry_id = entry_id,
        session_id = session_id,
        event_type = type,
        happened_at = happened_at %||% utc_now(),
        surface = surface,
        position = position,
        payload = payload
      )
      tryCatch(
        store_record_event(store, event),
        error = function(error) {
          telemetry_log(
            "warn",
            "event.write_failed",
            list("event.type" = type, "error.type" = class(error)[[1]])
          )
        }
      )
      invisible(event)
    }

    feeds <- shiny::reactive({
      refresh_tick()
      store_list_feeds(store, actor_id)
    })

    queue_entries <- shiny::reactive({
      refresh_tick()
      view <- input$view %||% "unread"
      if (view %in% c("today", "week", "month")) {
        shiny::invalidateLater(60 * 1000, session)
      }
      store_list_entries(
        store,
        actor_id,
        view = view,
        feed_id = selected_feed(),
        limit = 150L,
        sort = input$story_sort %||% "newest"
      )
    })

    entries <- shiny::reactive({
      rows <- queue_entries()
      ids <- retained_ids()
      if (
        !length(ids) ||
          !identical(retained_context(), current_context())
      ) {
        return(rows)
      }

      all_rows <- store_list_entries(
        store,
        actor_id,
        view = "all",
        feed_id = selected_feed(),
        limit = 500L,
        sort = input$story_sort %||% "newest"
      )
      visible_ids <- unique(c(as.character(rows$entry_id), ids))
      all_rows[all_rows$entry_id %in% visible_ids, , drop = FALSE]
    })

    selected_feed_title <- shiny::reactive({
      feed_id <- selected_feed()
      if (is.null(feed_id)) {
        return(NULL)
      }
      feed_rows <- feeds()
      title <- feed_rows$title[feed_rows$feed_id == feed_id]
      if (length(title)) title[[1]] else NULL
    })

    selected_entry <- shiny::reactive({
      entry_id <- selected_id()
      shiny::req(entry_id)
      refresh_tick()
      store_get_entry(store, actor_id, entry_id)
    })

    selected_document <- shiny::reactive({
      entry <- selected_entry()
      shiny::withProgress(
        message = "Preparing a clean reading copy",
        value = 0.5,
        {
          get_or_extract_document(store, entry, config)
        }
      )
    })

    refresh_feeds_now <- function() {
      result <- shiny::withProgress(message = "Refreshing feeds", value = 0, {
        feed_rows <- store_list_feeds(
          store,
          actor_id,
          source_kind = "subscription"
        )
        if (!nrow(feed_rows)) {
          return(list())
        }
        output <- vector("list", nrow(feed_rows))
        for (index in seq_len(nrow(feed_rows))) {
          shiny::incProgress(
            1 / nrow(feed_rows),
            detail = feed_rows$title[[index]]
          )
          output[[index]] <- tryCatch(
            refresh_feed(store, as.list(feed_rows[index, , drop = FALSE])),
            error = function(error) list(error = conditionMessage(error))
          )
        }
        output
      })
      failures <- sum(vapply(
        result,
        function(item) !is.null(item$error),
        logical(1)
      ))
      status_text(
        if (failures) {
          paste0(
            failures,
            " feed",
            if (failures == 1L) "" else "s",
            " failed to refresh"
          )
        } else {
          "Feeds are up to date"
        }
      )
      status_kind(if (failures) "error" else "success")
      record_event(
        "feeds_refreshed",
        surface = "sidebar",
        payload = list(failures = failures)
      )
      bump_refresh()
      invisible(result)
    }

    output$feed_nav <- shiny::renderUI({
      feed_rows <- feeds()
      all_unread <- sum(feed_rows$unread_count, na.rm = TRUE)
      all_active <- is.null(selected_feed())
      links <- list(shiny::tags$button(
        type = "button",
        class = paste("feed-link", if (all_active) "is-active"),
        `aria-current` = if (all_active) "true" else NULL,
        onclick = "rillSelectFeed(null)",
        shiny::tags$span("All feeds"),
        shiny::tags$small(all_unread)
      ))

      if (nrow(feed_rows)) {
        links <- c(
          links,
          lapply(seq_len(nrow(feed_rows)), function(index) {
            feed <- feed_rows[index, , drop = FALSE]
            onclick <- sprintf(
              "rillSelectFeed(%s)",
              jsonlite::toJSON(as.character(feed$feed_id), auto_unbox = TRUE)
            )
            shiny::tags$button(
              type = "button",
              class = paste(
                "feed-link",
                if (identical(selected_feed(), as.character(feed$feed_id))) {
                  "is-active"
                }
              ),
              `aria-current` = if (
                identical(selected_feed(), as.character(feed$feed_id))
              ) {
                "true"
              } else {
                NULL
              },
              onclick = onclick,
              title = feed$title,
              shiny::tags$span(feed$title),
              shiny::tags$small(feed$unread_count)
            )
          })
        )
      }
      shiny::tagList(links)
    })

    output$rename_feed_control <- shiny::renderUI({
      feed_id <- selected_feed()
      if (is.null(feed_id)) {
        return(rename_feed_control_ui())
      }
      feed_rows <- feeds()
      selected <- feed_rows[feed_rows$feed_id == feed_id, , drop = FALSE]
      if (!nrow(selected)) {
        return(rename_feed_control_ui())
      }
      rename_feed_control_ui(as.list(selected[1, , drop = FALSE]))
    })

    output$list_title <- shiny::renderUI({
      label <- switch(
        input$view %||% "unread",
        unread = "Unread",
        all = "All stories",
        starred = "Starred",
        saved = "Saved",
        today = "Today",
        week = "This week",
        month = "This month"
      )
      feed_title <- selected_feed_title()
      if (!is.null(feed_title)) {
        label <- paste(label, "\u00b7", feed_title)
      }
      shiny::tags$h1(label)
    })

    output$story_count <- shiny::renderUI({
      count <- nrow(queue_entries())
      noun <- if (count == 1L) "story" else "stories"
      shiny::tags$span(
        class = "count-pill",
        title = paste(count, noun),
        `aria-label` = paste(count, noun),
        count
      )
    })

    output$prepare_today_control <- shiny::renderUI({
      if (!identical(input$view, "today")) {
        return(NULL)
      }
      prepare_today_button()
    })

    output$read_actions <- shiny::renderUI({
      read_actions_ui(selected_feed_title())
    })

    output$story_list <- shiny::renderUI({
      rows <- entries()
      if (!nrow(rows)) {
        return(empty_story_list(
          input$view %||% "unread",
          selected_feed_title()
        ))
      }
      shiny::tagList(lapply(seq_len(nrow(rows)), function(index) {
        story_card(
          as.list(rows[index, , drop = FALSE]),
          index,
          selected = identical(
            selected_id(),
            as.character(rows$entry_id[[index]])
          )
        )
      }))
    })

    output$reader_header <- shiny::renderUI({
      if (is.null(selected_id())) {
        return(shiny::tags$header(
          class = "reader-empty-header",
          rill_duck_mark("reader-monogram"),
          shiny::tags$h1("Choose something worth reading"),
          shiny::tags$p(
            "Rill will make a clean copy and keep track of what you've read."
          )
        ))
      }

      entry <- selected_entry()
      document <- selected_document()
      reading_minutes <- max(
        1L,
        ceiling(as.integer(document$word_count %||% 0L) / 225)
      )

      shiny::tags$header(
        class = "article-header",
        shiny::tags$div(
          class = "article-actions",
          shiny::tags$button(
            type = "button",
            class = "reader-action mobile-back",
            `aria-keyshortcuts` = "Escape",
            onclick = "rillCloseReader()",
            bsicons::bs_icon("arrow-left"),
            "Stories"
          ),
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
            class = paste("reader-action", if (isTRUE(entry$saved)) "is-active")
          ),
          shiny::tags$a(
            class = "reader-action original-link",
            href = entry$url,
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
          )
        ),
        shiny::tags$p(
          class = "article-source",
          document$site %||% entry$feed_title
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
        )
      )
    })

    output$reader_body <- shiny::renderUI({
      if (is.null(selected_id())) {
        return(NULL)
      }
      document <- selected_document()
      shiny::tags$article(
        id = "reader-document",
        class = "reader-document",
        `data-entry-id` = selected_id(),
        render_document(document),
        shiny::tags$footer(
          class = "article-footer",
          paste(
            "Reading copy prepared by",
            document$producer %||% "feed fallback"
          )
        )
      )
    })

    output$sidebar_status <- shiny::renderUI({
      text <- status_text()
      if (is.null(text)) {
        return(NULL)
      }
      shiny::tags$p(
        class = paste("sidebar-status", paste0("is-", status_kind())),
        text
      )
    })

    output$export_opml <- shiny::downloadHandler(
      filename = function() {
        paste0("rill-subscriptions-", format(Sys.Date(), "%Y-%m-%d"), ".opml")
      },
      content = function(file) {
        feed_rows <- store_list_feeds(
          store,
          actor_id,
          source_kind = "subscription"
        )
        write_opml(
          feed_rows,
          file,
          title = paste(config$app_name, "subscriptions")
        )
        record_event(
          "opml_exported",
          surface = "sidebar",
          payload = list(feeds = nrow(feed_rows))
        )
      },
      contentType = "text/x-opml"
    )

    shiny::observeEvent(
      input$view,
      {
        clear_selection()
      },
      ignoreInit = TRUE,
      priority = 100
    )

    shiny::observeEvent(
      input$story_sort,
      {
        clear_selection()
      },
      ignoreInit = TRUE,
      priority = 100
    )

    shiny::observeEvent(
      input$select_feed,
      {
        clear_selection()
        selected_feed(input$select_feed$id %||% NULL)
        record_event(
          "feed_filter",
          surface = "sidebar",
          payload = list(feed_id = selected_feed())
        )
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$select_entry,
      {
        request <- input$select_entry
        entry_id <- request$id %||% NULL
        if (is.null(entry_id)) {
          return()
        }
        retain_entry(entry_id)
        selected_id(entry_id)
        selected_position(as.integer(request$position %||% NA_integer_))
        store_mark_opened(store, actor_id, entry_id)
        record_event(
          "entry_opened",
          entry_id = entry_id,
          surface = "story_list",
          position = selected_position()
        )
        bump_refresh()
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$close_reader,
      {
        clear_selection(clear_retained = FALSE)
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$toggle_star,
      {
        entry <- selected_entry()
        value <- store_toggle_state(store, actor_id, entry$entry_id, "starred")
        record_event(
          "star_changed",
          entry$entry_id,
          payload = list(value = value)
        )
        bump_refresh()
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$mark_unread,
      {
        entry <- selected_entry()
        changed <- store_mark_unread(store, actor_id, entry$entry_id)
        if (changed) {
          record_event(
            "read_state_changed",
            entry$entry_id,
            payload = list(read = FALSE, reason = "manual_unread")
          )
        }
        status_kind("success")
        status_text("Marked story as unread")
        bump_refresh()
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$toggle_save,
      {
        entry <- selected_entry()
        value <- store_toggle_state(store, actor_id, entry$entry_id, "saved")
        record_event(
          "save_changed",
          entry$entry_id,
          payload = list(value = value)
        )
        bump_refresh()
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$client_event,
      {
        event <- input$client_event
        allowed <- c(
          "article_impression",
          "scroll_milestone",
          "dwell_heartbeat",
          "page_hidden",
          "open_original"
        )
        if (!event$type %in% allowed) {
          return()
        }

        payload <- list(
          scroll_percent = as.integer(event$scroll_percent %||% NA_integer_),
          dwell_seconds = as.integer(event$dwell_seconds %||% NA_integer_)
        )
        record_event(
          type = event$type,
          entry_id = event$entry_id %||% selected_id(),
          surface = "reader",
          position = selected_position(),
          payload = payload,
          event_id = event$event_id %||% NULL,
          happened_at = event$happened_at %||% NULL
        )
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$add_feed,
      {
        url <- trimws(input$new_feed_url %||% "")
        if (!nzchar(url)) {
          status_kind("error")
          status_text("Enter a feed or website URL.")
          shiny::showNotification(
            "Enter a feed or website URL.",
            type = "warning"
          )
          return()
        }

        result <- tryCatch(
          shiny::withProgress(
            message = "Finding and reading the feed",
            value = 0.5,
            {
              ingest_feed_url(store, url)
            }
          ),
          error = function(error) error
        )
        if (inherits(result, "error")) {
          telemetry_log(
            "warn",
            "feed.add_failed",
            list("error.type" = class(result)[[1]])
          )
          status_kind("error")
          status_text(conditionMessage(result))
          shiny::showNotification(
            conditionMessage(result),
            type = "error",
            duration = 8
          )
          return()
        }

        status_kind("success")
        status_text(paste(
          "Added",
          result$feed$title,
          "\u00b7",
          result$added,
          "stories"
        ))
        shiny::updateTextInput(session, "new_feed_url", value = "")
        record_event(
          "feed_added",
          surface = "sidebar",
          payload = list(feed_id = result$feed$feed_id)
        )
        bump_refresh()
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$rename_feed,
      {
        feed_id <- selected_feed()
        title <- trimws(input$feed_title %||% "")
        if (is.null(feed_id) || !nzchar(title)) {
          status_kind("error")
          status_text("Select a feed and enter a name.")
          shiny::showNotification(
            "Select a feed and enter a name.",
            type = "warning"
          )
          return()
        }

        result <- tryCatch(
          store_rename_feed(store, actor_id, feed_id, title),
          error = function(error) error
        )
        if (inherits(result, "error")) {
          status_kind("error")
          status_text(conditionMessage(result))
          shiny::showNotification(
            conditionMessage(result),
            type = "error"
          )
          return()
        }

        status_kind("success")
        status_text(paste("Renamed feed to", title))
        record_event(
          "feed_renamed",
          surface = "sidebar",
          payload = list(feed_id = feed_id, title = title)
        )
        bump_refresh()
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$import_opml,
      {
        upload <- input$import_opml
        if (is.null(upload) || !nrow(upload)) {
          return()
        }

        subscriptions <- tryCatch(
          read_opml(upload$datapath[[1]]),
          error = function(error) error
        )
        if (inherits(subscriptions, "error")) {
          status_kind("error")
          status_text(paste(
            "Couldn't import OPML:",
            conditionMessage(subscriptions)
          ))
          shiny::showNotification(
            "That file could not be read as OPML.",
            type = "error",
            duration = 8
          )
          session$sendCustomMessage("rill-reset-file", "import_opml")
          return()
        }

        result <- shiny::withProgress(
          message = "Importing subscriptions",
          value = 0,
          {
            import_opml_subscriptions(
              store,
              actor_id,
              subscriptions,
              refresh = FALSE,
              progress = function(index, total, title) {
                shiny::setProgress(
                  value = index / total,
                  detail = title
                )
              }
            )
          }
        )
        status_kind(
          if (result$total == 0L) {
            "warning"
          } else if (result$failed > 0L || result$refresh_failed > 0L) {
            if (result$imported > 0L) "warning" else "error"
          } else {
            "success"
          }
        )
        import_status <- format_opml_import_status(
          result,
          demo_mode = config$demo_mode,
          refresh_hint = TRUE
        )
        status_text(import_status)
        shiny::showNotification(
          import_status,
          type = switch(
            status_kind(),
            error = "error",
            warning = "warning",
            "message"
          ),
          duration = 8
        )
        record_event(
          "opml_imported",
          surface = "sidebar",
          payload = list(
            total = result$total,
            imported = result$imported,
            added = result$added,
            updated = result$updated,
            stories = result$stories,
            refresh_failed = result$refresh_failed,
            failed = result$failed
          )
        )
        session$sendCustomMessage("rill-reset-file", "import_opml")
        bump_refresh()
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$refresh_feeds,
      {
        refresh_feeds_now()
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$prepare_today,
      {
        shiny::req(identical(input$view, "today"))
        result <- tryCatch(
          shiny::withProgress(
            message = "Preparing today's reading copies",
            value = 0,
            {
              prepare_today_documents(
                store,
                config,
                progress = function(index, total, title) {
                  shiny::setProgress(
                    value = index / total,
                    detail = title
                  )
                }
              )
            }
          ),
          error = function(error) error
        )
        if (inherits(result, "error")) {
          status_kind("error")
          status_text("Today's reading copies couldn't be prepared")
          shiny::showNotification(
            conditionMessage(result),
            type = "error",
            duration = 8
          )
          return()
        }

        status <- format_prepare_today_status(result)
        status_kind(if (result$failed > 0L) "warning" else "success")
        status_text(status)
        shiny::showNotification(
          status,
          type = if (result$failed > 0L) "warning" else "message",
          duration = 8
        )
        record_event(
          "today_prepared",
          surface = "reading_queue",
          payload = result[c("total", "cached", "prepared", "failed")]
        )
      },
      ignoreInit = TRUE
    )

    mark_scope_read <- function(before = NULL, reason) {
      marked <- store_mark_entries_read(
        store,
        actor_id,
        feed_id = selected_feed(),
        before = before,
        reason = reason
      )
      count <- length(marked)
      if (count) {
        retained_ids(setdiff(retained_ids(), marked))
        if (!is.null(selected_id()) && selected_id() %in% marked) {
          clear_selection(clear_retained = FALSE)
        }
        payload <- list(
          count = count,
          feed_id = selected_feed(),
          reason = reason
        )
        if (!is.null(before)) {
          payload$before <- format(before, tz = "UTC", usetz = TRUE)
        }
        record_event(
          "read_state_bulk_changed",
          surface = "reading_queue",
          payload = payload
        )
      }

      status <- if (identical(reason, "bulk_older_than_day")) {
        if (count) {
          paste(
            "Marked",
            count,
            if (count == 1L) "story" else "stories",
            "older than a day as read"
          )
        } else {
          "No unread stories older than a day"
        }
      } else if (count) {
        paste(
          "Marked",
          count,
          if (count == 1L) "story" else "stories",
          "as read"
        )
      } else {
        "No unread stories to mark"
      }
      status_kind("success")
      status_text(status)
      shiny::showNotification(status, type = "message")
      bump_refresh()
      invisible(marked)
    }

    shiny::observeEvent(
      input$mark_all_read,
      {
        mark_scope_read(reason = "bulk_all")
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(
      input$mark_older_read,
      {
        mark_scope_read(
          before = Sys.time() - 24 * 60 * 60,
          reason = "bulk_older_than_day"
        )
      },
      ignoreInit = TRUE
    )

    if (isTRUE(config$refresh_on_start)) {
      session$onFlushed(function() refresh_feeds_now(), once = TRUE)
    }
  }
}
