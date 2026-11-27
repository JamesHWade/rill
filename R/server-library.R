feed_refresh_server <- function(
  input,
  session,
  config,
  store,
  actor_id,
  status_kind,
  status_text,
  record_event,
  bump_refresh,
  request_preparation
) {
  refresh_result <- shiny::reactiveVal(NULL)
  refresh_job <- NULL
  shiny::observe({
    busy <- identical(refresh_result()$status, "running")
    buttons <- c(
      "refresh_library",
      "retry_failed_feeds",
      "refresh_selected_feed"
    )
    buttons <- buttons[vapply(
      buttons,
      \(id) !is.null(input[[id]]),
      logical(1)
    )]
    for (id in buttons) {
      bslib::update_task_button(
        id,
        state = if (busy) "busy" else "ready",
        session = session
      )
    }
  })
  finish_refresh <- function(result) {
    refresh_result(result)
    if (identical(result$status, "error")) {
      status_kind("error")
      status_text("Refresh stopped. Please try again.")
      shiny::showNotification(status_text(), type = "error")
      bump_refresh(feeds_changed = TRUE)
      return(invisible(NULL))
    }
    status_text(feed_refresh_summary(result))
    if (identical(result$status, "skipped_overlap")) {
      status_kind("info")
      shiny::showNotification(status_text(), type = "default", duration = 8)
      return(invisible(result))
    }
    status_kind(if (result$failed_count) "warning" else "success")
    shiny::showNotification(
      status_text(),
      type = if (result$failed_count) "warning" else "message",
      duration = 8
    )
    record_event(
      "feeds_refreshed",
      surface = "sidebar",
      payload = list(
        failures = result$failed_count,
        checked = result$due_count,
        status = result$status
      )
    )
    bump_refresh(feeds_changed = TRUE)
    request_preparation()
    invisible(result)
  }
  poll_refresh <- function() {
    result <- tryCatch(
      poll_feed_refresh(refresh_job),
      error = function(error) {
        close_feed_refresh(refresh_job)
        list(status = "error")
      }
    )
    if (identical(result$status, "running")) {
      if (!session$isClosed()) {
        shiny::withReactiveDomain(
          session,
          shiny::isolate(refresh_result(result))
        )
      }
      shiny::withReactiveDomain(NULL, later::later(poll_refresh, 0.25))
    } else {
      refresh_job <<- NULL
      if (!session$isClosed()) {
        shiny::withReactiveDomain(
          session,
          shiny::isolate(finish_refresh(result))
        )
      }
    }
    invisible(NULL)
  }
  refresh_feeds_now <- function(feed_ids = NULL, failed_only = FALSE) {
    if (!is.null(refresh_job)) {
      return(invisible(NULL))
    }
    refresh_job <<- tryCatch(
      start_feed_refresh(config, store, actor_id, feed_ids, failed_only),
      error = \(error) NULL
    )
    if (is.null(refresh_job)) {
      finish_refresh(list(status = "error"))
      return(invisible(NULL))
    }
    refresh_result(list(status = "running"))
    shiny::withReactiveDomain(NULL, later::later(poll_refresh, 0))
    invisible(NULL)
  }

  list(start = refresh_feeds_now, result = refresh_result)
}

feed_management_server <- function(
  input,
  output,
  session,
  store,
  actor_id,
  selected_feed,
  feed_groups,
  feed_management_tick,
  bump_refresh,
  record_event,
  status_kind,
  status_text,
  feed_refresh
) {
  management_feed_id <- shiny::reactive({
    if (is.null(input$managed_feed)) selected_feed() else input$managed_feed
  })

  management_feeds <- shiny::reactive({
    feed_management_tick()
    store_list_feeds(store, actor_id, active_only = FALSE)
  })

  shiny::observeEvent(
    input$manage_feeds,
    {
      feed_management_tick(shiny::isolate(feed_management_tick()) + 1L)
      shiny::showModal(feed_manager_ui(
        management_feeds(),
        management_feed_id()
      ))
    },
    ignoreInit = TRUE
  )

  shiny::observeEvent(
    feed_management_tick(),
    {
      shiny::req(input$manage_feeds)
      rows <- management_feeds()
      shiny::updateSelectizeInput(
        session,
        "managed_feed",
        choices = feed_manager_choices(rows),
        selected = management_feed_id() %||% ""
      )
      groups <- feed_groups()
      group_choices <- stats::setNames(groups$group_id, groups$name)
      active <- rows[
        rows$status == "active" & rows$source_kind == "subscription",
        ,
        drop = FALSE
      ]
      shiny::updateSelectizeInput(
        session,
        "bulk_group_feeds",
        choices = feed_manager_choices(active)[-1],
        selected = intersect(
          input$bulk_group_feeds %||% character(),
          active$feed_id
        )
      )
      shiny::updateSelectizeInput(
        session,
        "bulk_groups",
        choices = group_choices,
        selected = intersect(
          input$bulk_groups %||% character(),
          groups$group_id
        )
      )
      shiny::updateSelectInput(
        session,
        "managed_group",
        choices = c("Choose a Group" = "", group_choices),
        selected = if ((input$managed_group %||% "") %in% groups$group_id) {
          input$managed_group
        } else {
          ""
        }
      )
    },
    ignoreInit = TRUE
  )

  output$feed_refresh_status <- shiny::renderUI({
    feed_refresh_status_ui(feed_refresh$result())
  })

  output$feed_refresh_activity <- shiny::renderUI({
    result <- feed_refresh$result()
    if (identical(result$status, "running")) feed_refresh_status_ui(result)
  })

  managed_feed <- shiny::reactive({
    feed_id <- management_feed_id()
    if (is.null(feed_id)) {
      return(NULL)
    }
    feed_rows <- management_feeds()
    selected <- feed_rows[feed_rows$feed_id == feed_id, , drop = FALSE]
    if (!nrow(selected)) {
      return(NULL)
    }
    as.list(selected[1, , drop = FALSE])
  })

  # A finished refresh can change a feed's poll results, address, and source
  # title. Rebuilding the controls then would discard a half-typed name or
  # unsaved Groups, so they are rebuilt only when the manager opens or the
  # feed, its status, or its kind changes. A reactiveVal only invalidates on
  # a changed value.
  managed_feed_fields <- shiny::reactiveVal(NULL)
  shiny::observe({
    feed <- managed_feed()
    shown <- c("feed_id", "status", "source_kind")
    managed_feed_fields(
      if (!is.null(feed)) feed[intersect(shown, names(feed))]
    )
  })
  managed_feed_groups <- shiny::reactiveVal(NULL)
  shiny::observe({
    groups <- feed_groups()
    managed_feed_groups(data.frame(
      group_id = groups$group_id,
      name = groups$name
    ))
  })

  # The name and Groups the controls were last built with. A newer value
  # replaces a field only while the field still holds that value.
  managed_feed_shown <- new.env(parent = emptyenv())
  output$feed_organization_control <- shiny::renderUI({
    input$manage_feeds
    if (is.null(managed_feed_fields())) {
      return(feed_organization_control_ui())
    }
    feed <- shiny::isolate(managed_feed())
    managed_feed_shown$feed_id <- feed$feed_id
    managed_feed_shown$title <- feed$title
    managed_feed_shown$group_ids <- feed_group_ids(feed)
    feed_organization_control_ui(feed, groups = managed_feed_groups())
  })
  shiny::observe({
    feed <- managed_feed()
    if (is.null(feed) || !identical(feed$feed_id, managed_feed_shown$feed_id)) {
      return()
    }
    if (!identical(feed$title, managed_feed_shown$title)) {
      if (
        identical(shiny::isolate(input$feed_title), managed_feed_shown$title)
      ) {
        shiny::updateTextInput(session, "feed_title", value = feed$title)
      }
      managed_feed_shown$title <- feed$title
    }
    group_ids <- feed_group_ids(feed)
    if (!setequal(group_ids, managed_feed_shown$group_ids)) {
      chosen <- shiny::isolate(input$feed_groups) %||% character()
      if (setequal(chosen, managed_feed_shown$group_ids)) {
        shiny::updateSelectizeInput(
          session,
          "feed_groups",
          selected = group_ids
        )
      }
      managed_feed_shown$group_ids <- group_ids
    }
  })

  output$managed_feed_url <- shiny::renderText(managed_feed()$feed_url)

  output$managed_feed_status <- shiny::renderUI({
    feed <- managed_feed()
    if (!is.null(feed)) feed_poll_status_ui(feed)
  })

  output$group_management_control <- shiny::renderUI({
    input$manage_feeds
    shiny::isolate(group_management_ui(management_feeds(), feed_groups()))
  })

  change_groups <- function(action) {
    result <- tryCatch(
      {
        action()
        NULL
      },
      error = identity
    )
    if (inherits(result, "error")) {
      shiny::showNotification(conditionMessage(result), type = "error")
      return(invisible(NULL))
    }
    feed_management_tick(shiny::isolate(feed_management_tick()) + 1L)
    bump_refresh()
    record_event("groups_updated", surface = "feed_manager")
    status_kind("success")
    status_text("Groups updated")
    shiny::showNotification("Groups updated", type = "message")
  }
  shiny::observeEvent(
    input$create_group,
    {
      change_groups(function() {
        store_create_group(store, actor_id, input$new_group_name)
      })
    },
    ignoreInit = TRUE
  )
  shiny::observeEvent(
    input$rename_group,
    {
      shiny::req(input$managed_group)
      change_groups(function() {
        store_rename_group(
          store,
          actor_id,
          input$managed_group,
          input$group_name
        )
      })
    },
    ignoreInit = TRUE
  )
  pending_group_delete <- shiny::reactiveVal(NULL)
  shiny::observeEvent(
    input$delete_group,
    {
      shiny::req(input$managed_group)
      pending_group_delete(input$managed_group)
    },
    ignoreInit = TRUE
  )
  # Binding the confirmation's own buttons sends their initial values, so
  # each reset listens to one input rather than to a list of them.
  shiny::observeEvent(input$cancel_delete_group, pending_group_delete(NULL))
  shiny::observeEvent(input$manage_feeds, pending_group_delete(NULL))
  shiny::observeEvent(
    input$managed_group,
    pending_group_delete(NULL),
    ignoreInit = TRUE
  )
  output$group_delete_confirmation <- shiny::renderUI({
    group_id <- pending_group_delete()
    if (is.null(group_id)) {
      return(NULL)
    }
    groups <- feed_groups()
    name <- groups$name[groups$group_id == group_id]
    if (!length(name)) {
      return(NULL)
    }
    group_delete_confirmation_ui(name[[1L]])
  })
  shiny::observeEvent(
    input$confirm_delete_group,
    {
      group_id <- pending_group_delete()
      shiny::req(group_id)
      pending_group_delete(NULL)
      change_groups(function() {
        store_delete_group(store, actor_id, group_id)
      })
    },
    ignoreInit = TRUE
  )
  shiny::observeEvent(
    input$save_feed_groups,
    {
      shiny::req(management_feed_id())
      change_groups(function() {
        store_update_group_memberships(
          store,
          actor_id,
          management_feed_id(),
          input$feed_groups %||% character()
        )
      })
    },
    ignoreInit = TRUE
  )
  shiny::observeEvent(
    input$add_feed_groups,
    {
      shiny::req(input$bulk_group_feeds, input$bulk_groups)
      change_groups(function() {
        store_update_group_memberships(
          store,
          actor_id,
          input$bulk_group_feeds,
          input$bulk_groups,
          "add"
        )
      })
    },
    ignoreInit = TRUE
  )
  shiny::observeEvent(
    input$remove_feed_groups,
    {
      shiny::req(input$bulk_group_feeds, input$bulk_groups)
      change_groups(function() {
        store_update_group_memberships(
          store,
          actor_id,
          input$bulk_group_feeds,
          input$bulk_groups,
          "remove"
        )
      })
    },
    ignoreInit = TRUE
  )

  list(feed_id = management_feed_id, feeds = management_feeds)
}

register_feed_actions <- function(
  input,
  session,
  config,
  store,
  actor_id,
  selected_feed,
  selected_entry,
  clear_selection,
  status_kind,
  status_text,
  record_event,
  bump_refresh,
  feed_management,
  feed_refresh
) {
  shiny::observeEvent(
    input$add_feed,
    {
      url <- trimws(input$new_feed_url %||% "")
      if (!nzchar(url)) {
        status_kind("error")
        status_text("Enter a feed or website URL.")
        session$sendCustomMessage(
          "rill-input-validity",
          list(
            id = "new_feed_url",
            invalid = TRUE,
            message = "Enter a feed or website URL."
          )
        )
        shiny::showNotification(
          "Enter a feed or website URL.",
          type = "warning"
        )
        return()
      }
      session$sendCustomMessage(
        "rill-input-validity",
        list(id = "new_feed_url", invalid = FALSE)
      )

      result <- tryCatch(
        shiny::withProgress(
          message = "Finding and reading the feed",
          value = 0.5,
          {
            ingest_feed_url(store, actor_id, url)
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
        if (identical(as.integer(result$added), 1L)) "story" else "stories"
      ))
      shiny::updateTextInput(session, "new_feed_url", value = "")
      record_event(
        "feed_added",
        surface = "sidebar",
        payload = list(feed_id = result$feed$feed_id)
      )
      bump_refresh(feeds_changed = TRUE)
    },
    ignoreInit = TRUE
  )

  shiny::observeEvent(
    input$rename_feed,
    {
      feed_id <- feed_management$feed_id()
      title <- trimws(input$feed_title %||% "")
      if (is.null(feed_id) || !nzchar(title)) {
        status_kind("error")
        status_text("Select a feed and enter a name.")
        session$sendCustomMessage(
          "rill-input-validity",
          list(
            id = "feed_title",
            invalid = TRUE,
            message = "Enter a name for the selected feed."
          )
        )
        shiny::showNotification(
          "Select a feed and enter a name.",
          type = "warning"
        )
        return()
      }
      session$sendCustomMessage(
        "rill-input-validity",
        list(id = "feed_title", invalid = FALSE)
      )

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
      bump_refresh(feeds_changed = TRUE)
    },
    ignoreInit = TRUE
  )

  shiny::observeEvent(
    input$move_feed,
    {
      feed_id <- feed_management$feed_id()
      folder <- trimws(input$feed_folder %||% "")
      if (is.null(feed_id) || !nzchar(folder)) {
        status_kind("error")
        status_text("Select a feed and enter a folder.")
        session$sendCustomMessage(
          "rill-input-validity",
          list(
            id = "feed_folder",
            invalid = TRUE,
            message = "Enter a folder for the selected feed."
          )
        )
        shiny::showNotification(
          "Select a feed and enter a folder.",
          type = "warning"
        )
        return()
      }
      session$sendCustomMessage(
        "rill-input-validity",
        list(id = "feed_folder", invalid = FALSE)
      )

      result <- tryCatch(
        store_move_feed(store, actor_id, feed_id, folder),
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
      status_text(paste("Moved feed to", folder))
      record_event(
        "feed_moved",
        surface = "sidebar",
        payload = list(feed_id = feed_id, folder = folder)
      )
      bump_refresh(feeds_changed = TRUE)
    },
    ignoreInit = TRUE
  )

  shiny::observeEvent(
    input$unsubscribe_feed,
    {
      feed_id <- feed_management$feed_id()
      if (is.null(feed_id)) {
        return()
      }
      entry <- tryCatch(selected_entry(), error = function(error) NULL)
      result <- tryCatch(
        store_unsubscribe_feed(store, actor_id, feed_id),
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

      if (identical(selected_feed(), feed_id)) {
        selected_feed(NULL)
      }
      if (identical(entry$feed_id, feed_id)) {
        clear_selection(force = TRUE)
      }
      status_kind("success")
      status_text(
        "Unsubscribed. Rill keeps its reading state in case you restore it."
      )
      record_event(
        "feed_unsubscribed",
        surface = "sidebar",
        payload = list(feed_id = feed_id)
      )
      bump_refresh(feeds_changed = TRUE)
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
      bump_refresh(feeds_changed = TRUE)
    },
    ignoreInit = TRUE
  )

  shiny::observeEvent(
    input$refresh_feeds,
    {
      feed_refresh$start()
    },
    ignoreInit = TRUE
  )

  shiny::observeEvent(
    input$refresh_library,
    {
      feed_refresh$start()
    },
    ignoreInit = TRUE
  )

  shiny::observeEvent(
    input$refresh_selected_feed,
    {
      shiny::req(nzchar(feed_management$feed_id() %||% ""))
      feed_refresh$start(feed_ids = feed_management$feed_id())
    },
    ignoreInit = TRUE
  )

  shiny::observeEvent(
    input$retry_failed_feeds,
    {
      feed_refresh$start(failed_only = TRUE)
    },
    ignoreInit = TRUE
  )

  shiny::observeEvent(
    input$restore_feed,
    {
      rows <- feed_management$feeds()
      feed_id <- feed_management$feed_id()
      restorable <- rows$feed_id[
        rows$status == "inactive" & rows$source_kind == "subscription"
      ]
      shiny::req(feed_id %in% restorable)
      result <- tryCatch(
        store_subscribe_feed(store, actor_id, feed_id),
        error = function(error) error
      )
      if (inherits(result, "error")) {
        shiny::showNotification(
          "Couldn't restore this subscription. Try again.",
          type = "error"
        )
        return()
      }
      status_kind("success")
      status_text("Feed restored with its Groups and reading state.")
      shiny::showNotification(status_text())
      record_event(
        "feed_restored",
        surface = "sidebar",
        payload = list(feed_id = feed_id)
      )
      bump_refresh(feeds_changed = TRUE)
    },
    ignoreInit = TRUE
  )
}
