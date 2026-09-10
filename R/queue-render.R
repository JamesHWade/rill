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
    cards <- lapply(seq_len(nrow(rows)), function(index) {
      entry <- as.list(rows[index, , drop = FALSE])
      selected <- identical(selected_id, ids[[index]])
      preview <- preview_src(entry)
      key <- digest::digest(
        list(
          entry,
          selected,
          preview,
          format_story_time(entry$published_at)
        ),
        algo = "xxhash64"
      )
      previous <- cache[[ids[[index]]]]
      if (is.null(previous) || !identical(previous$key, key)) {
        card <- story_card(entry, index, selected, preview)
        card$attribs$`data-queue-version` <- key
        previous <- list(key = key, html = htmltools::renderTags(card)$html)
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

queue_batch_server <- function(context, input) {
  size <- shiny::reactiveVal(30L)
  shiny::observeEvent(context(), size(30L), priority = 100)
  shiny::observeEvent(input$queue_more, size(size() + 30L), ignoreInit = TRUE)
  size
}
