root <- commandArgs(trailingOnly = TRUE)[[1L]]
pkgload::load_all(root, quiet = TRUE)
rows <- sample_rill_data()$entries[rep(1L, 150L), ]
rows$entry_id <- paste0("synthetic-", seq_len(150L))
rows$feed_title <- "Example source"
rows$read_at <- NA_character_
rows$saved <- FALSE
rows$starred <- FALSE
rows$summary <- paste(rep("Source excerpt.", 20), collapse = " ")
if (exists("queue_card_renderer", mode = "function")) {
  batch <- rows[seq_len(30L), ]
  cold <- function() {
    renderer <- queue_card_renderer()
    htmltools::renderTags(renderer(batch, NULL, \(entry) NULL)$cards)$html
  }
  renderer <- queue_card_renderer()
  warm <- function() {
    htmltools::renderTags(renderer(batch, NULL, \(entry) NULL)$cards)$html
  }
} else {
  cold <- function() {
    htmltools::renderTags(shiny::tagList(lapply(
      seq_len(nrow(rows)),
      function(index) {
        story_card(as.list(rows[index, , drop = FALSE]), index)
      }
    )))$html
  }
  warm <- cold
}
cold_html <- cold()
invisible(warm())
cat(jsonlite::toJSON(
  list(
    cold_ms = replicate(5L, system.time(cold())[[3L]] * 1000),
    warm_ms = replicate(5L, system.time(warm())[[3L]] * 1000),
    html_bytes = nchar(cold_html, type = "bytes")
  ),
  auto_unbox = TRUE,
  pretty = TRUE
))
