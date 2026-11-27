story_sidebar_ui <- function() {
  bslib::sidebar(
    shiny::tags$header(
      class = "pane-header",
      shiny::tags$div(
        class = "queue-heading",
        shiny::tags$div(
          class = "queue-title",
          shiny::tags$p(class = "eyebrow", "Reading queue"),
          shiny::uiOutput("list_title", container = shiny::tags$div),
          shiny::uiOutput("calendar_context", container = shiny::tags$div)
        ),
        shiny::uiOutput("story_count", container = shiny::tags$div)
      ),
      htmltools::tagQuery(
        bslib::toolbar(
          orientation_open_button(),
          shiny::uiOutput(
            "prepare_today_control",
            container = shiny::tags$div,
            class = "prepare-today-control"
          ),
          shiny::uiOutput(
            "read_actions",
            container = shiny::tags$div,
            class = "read-actions"
          ),
          shiny::tags$div(
            class = "story-sort",
            htmltools::tagQuery(
              shiny::selectInput(
                "story_sort",
                label = NULL,
                choices = c(
                  "Newest" = "newest",
                  "Oldest" = "oldest",
                  "Recently added" = "recently_added",
                  "Feed A\u2013Z" = "feed",
                  "Title A\u2013Z" = "title"
                ),
                selected = "newest",
                selectize = FALSE,
                width = "126px"
              )
            )$find("select")$addAttrs(
              `aria-label` = "Sort stories"
            )$allTags()
          ),
          align = "left",
          gap = "8px"
        )
      )$addClass("queue-controls")$allTags()
    ),
    shiny::uiOutput(
      "queue_filters",
      container = shiny::tags$div,
      class = "queue-filters"
    ),
    shiny::uiOutput(
      "orientation_queue_status",
      container = shiny::tags$div,
      class = "orientation-queue-status-slot"
    ),
    shiny::uiOutput(
      "story_list",
      container = shiny::tags$div,
      class = "story-list",
      `aria-keyshortcuts` = "j k"
    ),
    id = "story_sidebar",
    class = "story-pane",
    width = "385px",
    open = list(desktop = "open", mobile = "closed"),
    resizable = TRUE,
    fillable = TRUE,
    padding = 0,
    gap = 0
  )
}

prepare_today_button <- function() {
  shiny::actionButton(
    "prepare_today",
    "Prepare",
    icon = bsicons::bs_icon("cloud-arrow-down"),
    class = "btn-prepare-today",
    title = "Prepare today\u2019s reading copies",
    `aria-label` = "Prepare today\u2019s reading copies"
  )
}

preparation_failures_ui <- function(failures) {
  title_id <- "preparation-details-title"
  modal <- shiny::modalDialog(
    title = shiny::tags$span(id = title_id, "Preparation details"),
    easyClose = TRUE,
    shiny::tags$p(
      "Stories keep the copies they already have. Choose Prepare to try again, ",
      "or open a story's original. If a failure keeps happening, send its ",
      "reference to whoever runs this Rill; it matches an entry in the server log."
    ),
    shiny::tags$ul(lapply(failures, function(failure) {
      shiny::tags$li(
        shiny::tags$strong(failure$title %||% "Today's reading copies"),
        shiny::tags$p(failure$message),
        shiny::tags$p(
          paste0("Stage: ", failure$stage, ". Reference: "),
          shiny::tags$code(failure$reference)
        )
      )
    }))
  )
  htmltools::tagQuery(modal)$addAttrs(
    role = "dialog",
    `aria-labelledby` = title_id
  )$allTags()
}

read_actions_ui <- function(feed_title = NULL) {
  scope <- if (is.null(feed_title)) "all feeds" else feed_title

  bslib::popover(
    shiny::tags$button(
      type = "button",
      class = "read-actions-trigger",
      title = "Manage reading status",
      `aria-label` = "Manage reading status",
      bsicons::bs_icon("check2-all")
    ),
    title = "Reading status",
    placement = "bottom",
    shiny::tags$div(
      class = "read-actions-menu",
      shiny::tags$p(
        class = "read-actions-scope",
        paste("Applies to", scope, "in every view.")
      ),
      shiny::actionButton(
        "mark_all_read",
        "Mark all as read",
        class = "btn-read-action"
      ),
      shiny::actionButton(
        "mark_older_read",
        "Mark older than a day as read",
        class = "btn-read-action"
      )
    )
  )
}

mark_unread_button <- function() {
  shiny::actionButton(
    "mark_unread",
    shiny::tagList(bsicons::bs_icon("circle"), "Mark unread"),
    class = "reader-action"
  )
}

source_initials <- function(source) {
  words <- strsplit(source, "[^\\p{L}\\p{N}]+", perl = TRUE)[[1L]]
  words <- words[nzchar(words)]
  names <- words[!tolower(words) %in% c("a", "an", "the")]
  if (length(names)) {
    words <- names
  }
  if (!length(words)) {
    return(toupper(substr(source, 1L, 2L)))
  }
  if (length(words) == 1L) {
    return(toupper(substr(words[[1L]], 1L, 2L)))
  }
  toupper(paste0(substr(words[[1L]], 1L, 1L), substr(words[[2L]], 1L, 1L)))
}

story_card <- function(
  entry,
  index,
  selected = FALSE,
  preview_src = NULL,
  time_label = format_story_time(entry$published_at)
) {
  entry_id <- as.character(entry$entry_id)
  is_read <- !is.na(entry$read_at) && nzchar(as.character(entry$read_at))
  saved <- isTRUE(entry$saved)
  title <- if (store_scalar_string(entry$title)) entry$title else "Untitled"
  source <- if (store_scalar_string(entry$feed_title)) {
    entry$feed_title
  } else {
    "Source"
  }
  initials <- source_initials(source)
  author <- if (store_scalar_string(entry$author)) entry$author else NULL
  byline <- paste(
    c(author, time_label),
    collapse = " \u00b7 "
  )
  image <- if (
    store_scalar_string(preview_src) && startsWith(preview_src, "session/")
  ) {
    preview_src
  } else {
    NA_character_
  }
  description_id <- paste0(
    "story-description-",
    digest::digest(entry_id, algo = "xxhash64")
  )
  summary_id <- paste0(description_id, "-summary")
  image_id <- paste0(description_id, "-image")
  descriptions <- c(
    if (store_scalar_string(entry$summary)) summary_id,
    if (!is.na(image) && store_scalar_string(entry$preview_image_alt)) image_id
  )
  read_action <- if (is_read) "mark_unread" else "mark_read"
  read_label <- if (is_read) "Mark unread" else "Mark read"
  action_button <- function(action, label, icon, class, ...) {
    shiny::tags$button(
      type = "button",
      class = class,
      `data-queue-action` = action,
      `data-entry-id` = entry_id,
      ...,
      queue_icon(icon),
      shiny::tags$span(label)
    )
  }
  shiny::tags$article(
    class = paste(
      c("story-row", if (is_read) "is-read", if (selected) "is-selected"),
      collapse = " "
    ),
    `data-entry-id` = entry_id,
    `data-queue-index` = as.integer(index),
    `aria-label` = title,
    shiny::tags$div(
      class = "story-swipe-tray",
      `aria-hidden` = "true",
      inert = NA,
      action_button(
        read_action,
        read_label,
        if (is_read) "circle" else "check2-circle",
        "story-swipe-button",
        tabindex = "-1"
      ),
      shiny::tags$span(class = "story-swipe-release", "Release")
    ),
    shiny::tags$div(
      class = "story-front",
      shiny::tags$div(
        class = "story-source",
        shiny::tags$span(
          class = "story-source-avatar",
          `aria-hidden` = "true",
          initials
        ),
        shiny::tags$div(
          class = "story-source-copy",
          shiny::tags$span(class = "feed-name", source),
          shiny::tags$span(class = "story-byline", byline)
        ),
        if (is_read) {
          shiny::tags$span(class = "story-state", "Read")
        } else {
          shiny::tags$span(
            class = "story-status-icon",
            role = "img",
            `aria-label` = "Unread",
            queue_icon("circle-fill")
          )
        }
      ),
      shiny::tags$button(
        type = "button",
        class = paste(
          c("story-card", if (is_read) "is-read", if (selected) "is-selected"),
          collapse = " "
        ),
        `data-entry-id` = entry_id,
        `aria-current` = if (selected) "true" else NULL,
        `aria-label` = paste("Open", title),
        `aria-describedby` = if (length(descriptions)) {
          paste(descriptions, collapse = " ")
        } else {
          NULL
        },
        onclick = sprintf(
          "rillSelectEntry(%s, Number(this.closest('.story-row').dataset.queueIndex))",
          jsonlite::toJSON(entry_id, auto_unbox = TRUE)
        ),
        shiny::tags$h3(title),
        if (store_scalar_string(entry$summary)) {
          shiny::tags$p(id = summary_id, entry$summary)
        },
        if (!is.na(image)) {
          shiny::tags$img(
            class = "story-preview-image",
            id = image_id,
            src = image,
            alt = if (store_scalar_string(entry$preview_image_alt)) {
              entry$preview_image_alt
            } else {
              ""
            },
            loading = "lazy",
            decoding = "async",
            referrerpolicy = "no-referrer",
            draggable = "false"
          )
        }
      ),
      shiny::tags$div(
        class = "story-actions",
        role = "group",
        `aria-label` = paste("Actions for", title),
        action_button(
          if (saved) "unsave" else "save",
          if (saved) "Saved" else "Save",
          if (saved) "bookmark-fill" else "bookmark",
          "story-action story-save",
          `aria-pressed` = if (saved) "true" else "false"
        ),
        action_button(
          read_action,
          read_label,
          if (is_read) "circle" else "check2-circle",
          "story-action story-read"
        ),
        if (isTRUE(entry$starred)) {
          shiny::tags$span(
            class = "story-starred",
            queue_icon("star-fill", title = "Starred")
          )
        }
      )
    )
  )
}

queue_filters_ui <- function(view) {
  shiny::tags$nav(
    `aria-label` = "Queue filter",
    lapply(c(unread = "Unread", all = "All", saved = "Saved"), function(label) {
      value <- switch(label, Unread = "unread", All = "all", Saved = "saved")
      shiny::tags$button(
        type = "button",
        class = "queue-filter",
        `aria-pressed` = if (identical(view, value)) "true" else "false",
        onclick = sprintf("rillQueueView('%s')", value),
        label
      )
    })
  )
}

compact_queue_navigation_ui <- function() {
  shiny::tags$nav(
    class = "compact-queue-navigation",
    `aria-label` = "Reader navigation",
    shiny::tags$button(
      type = "button",
      onclick = "rillQueueView('unread')",
      `data-queue-nav` = "queue",
      bsicons::bs_icon("collection"),
      shiny::tags$span("Queue")
    ),
    shiny::tags$button(
      type = "button",
      onclick = "rillOpenLibrary()",
      `data-queue-nav` = "library",
      bsicons::bs_icon("book"),
      shiny::tags$span("Library")
    ),
    shiny::tags$button(
      type = "button",
      onclick = "rillQueueView('saved')",
      `data-queue-nav` = "saved",
      bsicons::bs_icon("bookmark"),
      shiny::tags$span("Saved")
    )
  )
}

queue_notice_ui <- function() {
  shiny::tags$aside(
    id = "queue-notice",
    class = "queue-notice",
    hidden = NA,
    `aria-label` = "Queue action",
    shiny::tags$span(
      id = "queue-notice-message",
      role = "status",
      `aria-live` = "polite"
    ),
    shiny::tags$button(type = "button", id = "queue-undo", hidden = NA, "Undo"),
    shiny::tags$button(
      type = "button",
      id = "queue-notice-dismiss",
      `aria-label` = "Dismiss message",
      bsicons::bs_icon("x-lg")
    )
  )
}

empty_story_list <- function(view, feed_title = NULL) {
  scoped_feed <- !is.null(feed_title) && nzchar(feed_title)
  state <- switch(
    view,
    unread = list(
      title = "You're all caught up",
      body = if (scoped_feed) {
        paste0("No unread stories from ", feed_title, ".")
      } else {
        "New stories will land here after the next refresh."
      }
    ),
    starred = list(
      title = "No starred stories yet",
      body = "Star a story to keep it here."
    ),
    saved = list(
      title = "Nothing saved yet",
      body = "Save a story to come back to it later."
    ),
    today = list(
      title = "Nothing published today",
      body = if (scoped_feed) {
        paste0("No stories from ", feed_title, " were published today.")
      } else {
        "Stories published today will appear here."
      }
    ),
    week = list(
      title = "Nothing published this week",
      body = if (scoped_feed) {
        paste0("No stories from ", feed_title, " were published this week.")
      } else {
        "Stories published since Monday will appear here."
      }
    ),
    month = list(
      title = "Nothing published this month",
      body = if (scoped_feed) {
        paste0("No stories from ", feed_title, " were published this month.")
      } else {
        "Stories published this calendar month will appear here."
      }
    ),
    list(
      title = "No stories yet",
      body = if (scoped_feed) {
        paste0("There are no stories from ", feed_title, " yet.")
      } else {
        "Add a feed from Manage feeds to start reading."
      }
    )
  )

  shiny::tags$div(
    class = "empty-list",
    rill_reading_otter("empty-otter"),
    shiny::tags$p(class = "empty-eyebrow", "Reading queue"),
    shiny::tags$h3(state$title),
    shiny::tags$p(state$body)
  )
}
