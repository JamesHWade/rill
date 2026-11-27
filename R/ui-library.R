feed_navigation_ui <- function(
  feed_rows,
  navigation_groups,
  selected_feed,
  selected_group_ids,
  selected_ungrouped,
  selected_folder,
  open_feed_groups
) {
  all_unread <- sum(feed_rows$unread_count, na.rm = TRUE)
  all_active <- is.null(selected_feed()) &&
    !length(selected_group_ids()) &&
    !selected_ungrouped() &&
    is.null(selected_folder())
  links <- list(shiny::tags$button(
    type = "button",
    class = paste("feed-link", if (all_active) "is-active"),
    `aria-current` = if (all_active) "true" else NULL,
    onclick = "rillSelectFeed(null)",
    shiny::tags$span("All feeds"),
    shiny::tags$small(all_unread)
  ))

  # Groups the Reader expanded stay open when counts re-render.
  open_groups <- unlist(
    open_feed_groups(),
    use.names = FALSE
  )
  index <- navigation_groups()
  links <- c(
    links,
    lapply(seq_along(index$group_ids), function(i) {
      id <- index$group_ids[[i]]
      group_name <- index$names[[i]]
      rows <- feed_rows[index$rows[[i]], , drop = FALSE]
      active <- if (nzchar(id)) {
        id %in% selected_group_ids()
      } else {
        selected_ungrouped()
      }
      group_key <- if (nzchar(id)) id else "ungrouped"
      shiny::tags$div(
        class = "feed-folder",
        shiny::tags$button(
          type = "button",
          class = paste("feed-link", if (active) "is-active"),
          `aria-current` = if (active) "true" else NULL,
          onclick = sprintf(
            "rillSelectGroup(%s)",
            jsonlite::toJSON(id, auto_unbox = TRUE)
          ),
          shiny::tags$span(group_name),
          shiny::tags$small(index$unread[[i]])
        ),
        shiny::tags$details(
          `data-group-key` = group_key,
          open = if (
            (!is.null(selected_feed()) &&
              selected_feed() %in% rows$feed_id) ||
              group_key %in% open_groups
          ) {
            "open"
          } else {
            NULL
          },
          shiny::tags$summary(paste(
            nrow(rows),
            if (nrow(rows) == 1L) "feed in" else "feeds in",
            group_name
          )),
          lapply(seq_len(nrow(rows)), function(index) {
            feed <- rows[index, , drop = FALSE]
            active <- identical(
              selected_feed(),
              as.character(feed$feed_id)
            )
            shiny::tags$button(
              type = "button",
              class = paste("feed-link", if (active) "is-active"),
              `aria-current` = if (active) "true" else NULL,
              onclick = sprintf(
                "rillSelectFeed(%s)",
                jsonlite::toJSON(
                  as.character(feed$feed_id),
                  auto_unbox = TRUE
                )
              ),
              title = feed$title,
              shiny::tags$span(feed$title),
              shiny::tags$small(feed$unread_count)
            )
          })
        )
      )
    })
  )
  captures <- feed_rows[feed_rows$source_kind == "capture", , drop = FALSE]
  links <- c(
    links,
    lapply(seq_len(nrow(captures)), function(i) {
      feed <- captures[i, , drop = FALSE]
      active <- identical(selected_feed(), feed$feed_id[[1]])
      shiny::tags$button(
        type = "button",
        class = paste("feed-link", if (active) "is-active"),
        `aria-current` = if (active) "true" else NULL,
        onclick = sprintf(
          "rillSelectFeed(%s)",
          jsonlite::toJSON(feed$feed_id[[1]], auto_unbox = TRUE)
        ),
        shiny::tags$span(feed$title),
        shiny::tags$small(feed$unread_count)
      )
    })
  )
  shiny::tagList(links)
}

navigation_sidebar_ui <- function(config) {
  bslib::sidebar(
    shiny::tags$header(
      class = "compact-library-header",
      shiny::tags$button(
        type = "button",
        class = "compact-surface-back",
        onclick = "rillCloseLibrary()",
        bsicons::bs_icon("arrow-left"),
        shiny::tags$span("Back")
      ),
      shiny::tags$h1("Library")
    ),
    shiny::tags$div(
      class = "brand",
      rill_otter_mark("brand-mark"),
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
        ),
        shiny::tags$span(
          class = "nav-choice nav-choice-temporal-first",
          bsicons::bs_icon("calendar-day"),
          "Today"
        ),
        shiny::tags$span(
          class = "nav-choice",
          bsicons::bs_icon("calendar-week"),
          "This week"
        ),
        shiny::tags$span(
          class = "nav-choice",
          bsicons::bs_icon("calendar-month"),
          "This month"
        )
      ),
      choiceValues = c(
        "unread",
        "all",
        "starred",
        "saved",
        "today",
        "week",
        "month"
      ),
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
    shiny::uiOutput("feed_refresh_activity"),
    shiny::uiOutput(
      "feed_nav",
      container = shiny::tags$nav,
      class = "feed-list",
      `aria-label` = "Feeds"
    ),
    group_reading_control_ui(),
    shiny::tags$div(
      class = "sidebar-footer",
      shiny::uiOutput("recent_answer_control"),
      shiny::uiOutput(shiny::NS("access_requests", "launcher")),
      shiny::uiOutput("orientation_destination_settings"),
      appearance_control_ui(),
      if (isTRUE(config$reader_memory_enabled)) reader_memory_ui("memory"),
      shiny::actionButton(
        "manage_feeds",
        "Manage feeds",
        class = "btn-manage-feeds"
      ),
      identity_sign_out_ui(config),
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
    open = list(desktop = "open", mobile = "closed"),
    resizable = TRUE,
    fillable = TRUE,
    padding = 0,
    gap = 0
  )
}

feed_refresh_status_ui <- function(result) {
  if (is.null(result)) {
    return(shiny::tags$p("Check your feeds for new stories."))
  }
  if (!identical(result$status, "running")) {
    return(shiny::tags$p(role = "status", feed_refresh_summary(result)))
  }
  shiny::tags$div(
    role = "status",
    `aria-live` = "polite",
    shiny::tags$p(
      if (is.null(result$total)) {
        "Starting background refresh. You can keep reading."
      } else {
        paste0(
          result$index,
          " of ",
          result$total,
          " feeds checked \u00b7 ",
          result$title
        )
      }
    ),
    shiny::tags$progress(
      `aria-label` = "Feed refresh progress",
      value = result$index,
      max = result$total
    )
  )
}

feed_manager_choices <- function(feeds) {
  organization <- if ("groups" %in% names(feeds)) {
    vapply(
      feeds$groups,
      function(groups) {
        if (length(groups)) paste(groups, collapse = ", ") else "Ungrouped"
      },
      character(1)
    )
  } else {
    feeds$folder
  }
  if ("source_kind" %in% names(feeds)) {
    organization[feeds$source_kind == "capture"] <- "Captures"
  }
  labels <- paste(feeds$title, "\u00b7", organization)
  labels <- paste0(
    labels,
    ifelse(
      feeds$status == "inactive",
      " (unsubscribed)",
      ifelse(feeds$poll_status %in% "failed", " (needs attention)", "")
    )
  )
  c("Choose a feed" = "", stats::setNames(feeds$feed_id, labels))
}

feed_manager_ui <- function(feeds, selected = NULL) {
  rill_modal_dialog(
    title = "Manage feeds",
    feed_tools_ui(feeds, selected),
    id = "feed-manager",
    size = "l",
    easyClose = TRUE,
    footer = shiny::modalButton("Done")
  )
}

feed_tools_ui <- function(feeds = NULL, selected = NULL) {
  shiny::tags$div(
    class = "feed-tools",
    bslib::layout_columns(
      col_widths = c(6, 6),
      fill = FALSE,
      class = "feed-tool-section",
      shiny::tags$div(
        shiny::tags$h3("Add a feed"),
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
        bslib::input_task_button(
          "add_feed",
          "Add feed",
          class = "btn-add-feed"
        )
      ),
      shiny::tags$div(
        class = "opml-tools",
        shiny::tags$h3("Import and export"),
        shiny::tags$div(
          class = "opml-actions",
          shiny::tags$div(
            class = "opml-upload",
            shiny::fileInput(
              "import_opml",
              label = NULL,
              accept = c(".opml", ".xml", "text/x-opml", "application/xml"),
              buttonLabel = shiny::tagList(
                bsicons::bs_icon("upload"),
                "Import OPML"
              ),
              placeholder = "No file selected"
            )
          ),
          shiny::downloadButton(
            "export_opml",
            "Export OPML",
            icon = bsicons::bs_icon("download"),
            class = "btn-opml-export"
          )
        ),
        shiny::tags$p(
          class = "feed-tool-help",
          paste(
            "Exports keep every Group a feed belongs to. Other readers may",
            "keep only one per feed."
          )
        )
      )
    ),
    shiny::tags$section(
      class = "feed-tool-section",
      shiny::tags$h3("Check for new stories"),
      shiny::uiOutput("feed_refresh_status"),
      shiny::tags$div(
        class = "feed-tool-actions",
        bslib::input_task_button(
          "refresh_library",
          "Refresh all feeds",
          auto_reset = FALSE
        ),
        bslib::input_task_button(
          "retry_failed_feeds",
          "Retry failed feeds",
          type = "default",
          auto_reset = FALSE
        )
      )
    ),
    shiny::tags$section(
      class = "feed-tool-section rename-feed-tools",
      shiny::tags$h3("Edit a feed"),
      shiny::selectizeInput(
        "managed_feed",
        "Feed",
        choices = if (is.null(feeds)) {
          c("Choose a feed" = "")
        } else {
          feed_manager_choices(feeds)
        },
        selected = selected %||% "",
        width = "100%"
      ),
      shiny::uiOutput("feed_organization_control")
    ),
    shiny::uiOutput("group_management_control")
  )
}

feed_poll_status_ui <- function(feed) {
  shiny::tags$p(
    role = "status",
    if (identical(feed$poll_status, "failed")) {
      paste(
        "The last check failed, and automatic retries slow down after",
        "repeated failures. Refresh this feed to try now, or check its",
        "address."
      )
    } else {
      checked <- format_checked_time(feed$last_polled_at)
      if (is.null(checked)) {
        "Not checked yet."
      } else {
        paste("Last checked", checked)
      }
    }
  )
}

feed_group_ids <- function(feed) {
  unlist(feed$group_ids, use.names = FALSE) %||% character()
}

feed_organization_control_ui <- function(
  feed = NULL,
  folders = character(),
  groups = NULL
) {
  if (is.null(feed)) {
    return(shiny::tags$p(
      class = "feed-tool-help",
      "Choose a feed to check its status, rename, assign Groups, or unsubscribe."
    ))
  }

  if (identical(feed$status, "inactive")) {
    return(shiny::tagList(
      shiny::tags$p(
        "Unsubscribed. Restore this feed with its saved Groups and reading state."
      ),
      shiny::actionButton("restore_feed", "Restore subscription")
    ))
  }

  shiny::tagList(
    shiny::textOutput(
      "managed_feed_url",
      container = \(...) shiny::tags$p(class = "feed-source-url", ...)
    ),
    if (identical(feed$source_kind %||% "subscription", "subscription")) {
      shiny::tagList(
        shiny::uiOutput("managed_feed_status"),
        bslib::input_task_button(
          "refresh_selected_feed",
          "Refresh this feed",
          auto_reset = FALSE
        )
      )
    },
    shiny::textInput(
      "feed_title",
      label = "Name in your Library",
      value = feed$title
    ),
    shiny::actionButton(
      "rename_feed",
      "Rename feed",
      class = "btn-rename-feed"
    ),
    if (identical(feed$source_kind %||% "subscription", "subscription")) {
      shiny::tagList(
        shiny::selectizeInput(
          "feed_groups",
          label = "Groups",
          choices = if (is.null(groups)) {
            character()
          } else {
            stats::setNames(groups$group_id, groups$name)
          },
          selected = feed_group_ids(feed),
          multiple = TRUE,
          options = list(closeAfterSelect = TRUE),
          width = "100%"
        ),
        shiny::actionButton("save_feed_groups", "Save Groups")
      )
    },
    if (identical(feed$source_kind %||% "subscription", "subscription")) {
      shiny::actionButton(
        "unsubscribe_feed",
        "Unsubscribe",
        class = "btn-unsubscribe-feed"
      )
    },
    shiny::tags$p(
      class = "feed-tool-help",
      if (identical(feed$source_kind %||% "subscription", "subscription")) {
        paste(
          "Unsubscribing hides the feed and keeps your reading state, so you",
          "can restore it later."
        )
      } else {
        "Pages you captured stay in your Library and can't be unsubscribed."
      }
    )
  )
}

feed_refresh_summary <- function(result) {
  if (identical(result$status, "error")) {
    return("Refresh stopped. Please try again.")
  }
  if (identical(result$status, "skipped_overlap")) {
    return("Another refresh is running. Try again shortly.")
  }
  if (!result$due_count) {
    return("There are no feeds to check here.")
  }
  added <- sum(vapply(result$outcomes, \(x) x$added_count, integer(1)))
  paste0(
    result$due_count,
    " feed",
    if (result$due_count == 1L) "" else "s",
    " checked \u00b7 ",
    added,
    " new stor",
    if (added == 1L) "y" else "ies",
    if (result$failed_count) {
      paste0(
        " \u00b7 ",
        result$failed_count,
        " failed. Retry failed feeds in Manage feeds."
      )
    } else {
      "."
    }
  )
}

parse_story_time <- function(value) {
  tryCatch(
    suppressWarnings(as.POSIXct(value, tz = "UTC")),
    error = function(error) as.POSIXct(NA)
  )
}

format_checked_time <- function(value, now = Sys.time()) {
  parsed <- parse_story_time(value)
  if (length(parsed) == 0L || is.na(parsed)) {
    return(NULL)
  }
  minutes <- floor(as.numeric(difftime(now, parsed, units = "mins")))
  ago <- function(count, unit) {
    paste(count, if (count == 1) unit else paste0(unit, "s"), "ago")
  }
  if (minutes < 1) {
    return("just now")
  }
  if (minutes < 60) {
    return(ago(minutes, "minute"))
  }
  if (minutes < 1440) {
    return(ago(floor(minutes / 60), "hour"))
  }
  if (minutes < 10080) {
    return(ago(floor(minutes / 1440), "day"))
  }
  paste("on", gsub(" +", " ", format(parsed, "%b %e, %Y")))
}

format_story_time <- function(value, now = Sys.time()) {
  parsed <- parse_story_time(value)
  if (length(parsed) == 0L || is.na(parsed)) {
    return("")
  }
  seconds <- as.numeric(difftime(now, parsed, units = "secs"))
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
