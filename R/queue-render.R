queue_icon <- local({
  icons <- new.env(parent = emptyenv())
  function(name, title = NULL) {
    key <- paste(name, title, sep = ":")
    if (!exists(key, envir = icons, inherits = FALSE)) {
      icons[[key]] <- htmltools::HTML(
        htmltools::renderTags(
          bsicons::bs_icon(name, title = title)
        )$html
      )
    }
    icons[[key]]
  }
})

queue_card_renderer <- function() {
  cache <- new.env(parent = emptyenv())
  function(rows, selected_id, preview_src) {
    ids <- as.character(rows$entry_id)
    remove <- setdiff(ls(cache, all.names = TRUE), ids)
    if (length(remove)) {
      rm(list = remove, envir = cache)
    }
    built <- 0L
    now <- Sys.time()
    cards <- lapply(seq_len(nrow(rows)), function(index) {
      entry <- lapply(rows, \(column) column[[index]])
      previous <- cache[[ids[[index]]]]
      published <- if (
        !is.null(previous) &&
          identical(previous$published_at, entry$published_at)
      ) {
        previous$published
      } else {
        parse_story_time(entry$published_at)
      }
      time_label <- format_story_time(published, now)
      selected <- identical(selected_id, ids[[index]])
      preview <- preview_src(entry)
      key <- digest::digest(
        list(
          entry,
          selected,
          preview,
          time_label
        ),
        algo = "xxhash64"
      )
      if (is.null(previous) || !identical(previous$key, key)) {
        card <- story_card(entry, index, selected, preview, time_label)
        card$attribs$`data-queue-version` <- key
        previous <- list(
          key = key,
          html = htmltools::renderTags(card)$html,
          published_at = entry$published_at,
          published = published
        )
        cache[[ids[[index]]]] <- previous
        built <<- built + 1L
      }
      htmltools::HTML(sub(
        'data-queue-index="[0-9]+"',
        sprintf('data-queue-index="%d"', index),
        previous$html
      ))
    })
    list(cards = cards, built = built)
  }
}

queue_batch_server <- function(context, input, selected_index = \() 0L) {
  size <- shiny::reactiveVal(30L)
  previous_context <- NULL
  shiny::observeEvent(
    context(),
    {
      current <- context()
      if (!identical(current, previous_context)) {
        previous_context <<- current
        size(30L)
      }
    },
    priority = 100
  )
  visible_size <- shiny::reactive(max(size(), selected_index()))
  shiny::observeEvent(
    input$queue_more,
    size(visible_size() + 30L),
    ignoreInit = TRUE
  )
  visible_size
}
