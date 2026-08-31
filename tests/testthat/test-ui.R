testthat::test_that("selected story cards expose their current state", {
  entry <- as.list(sample_rill_data()$entries[1, , drop = FALSE])
  entry$feed_title <- "The R Blog"
  entry$read_at <- NA_character_
  entry$starred <- FALSE

  card <- story_card(entry, index = 1L, selected = TRUE)
  html <- htmltools::renderTags(card)$html

  testthat::expect_match(html, 'aria-current="true"', fixed = TRUE)
  testthat::expect_match(
    html,
    paste0('data-entry-id="', entry$entry_id, '"'),
    fixed = TRUE
  )
  testthat::expect_match(html, "A calmer way to keep up with R", fixed = TRUE)
})

testthat::test_that("selected read stories explain why they remain in the queue", {
  entry <- as.list(sample_rill_data()$entries[1, , drop = FALSE])
  entry$feed_title <- "The R Blog"
  entry$read_at <- "2026-08-30 12:00:00 UTC"
  entry$starred <- FALSE

  card <- story_card(entry, index = 1L, selected = TRUE)
  html <- htmltools::renderTags(card)$html

  testthat::expect_match(html, "story-state", fixed = TRUE)
  testthat::expect_match(html, ">Read<", fixed = TRUE)
})

testthat::test_that("empty queues suggest the relevant next action", {
  starred <- htmltools::renderTags(empty_story_list("starred"))$html
  scoped <- htmltools::renderTags(
    empty_story_list("unread", "The R Blog")
  )$html

  testthat::expect_match(starred, "No starred stories yet", fixed = TRUE)
  testthat::expect_match(starred, "Press F", fixed = TRUE)
  testthat::expect_match(
    scoped,
    "No unread stories from The R Blog",
    fixed = TRUE
  )
})

testthat::test_that("invalid story times render without a label", {
  testthat::expect_identical(format_story_time(NA_character_), "")
  testthat::expect_identical(format_story_time("not-a-time"), "")
})

testthat::test_that("feed management exposes OPML import and export", {
  html <- htmltools::renderTags(feed_tools_ui())$html

  testthat::expect_match(html, "Manage feeds", fixed = TRUE)
  testthat::expect_match(html, 'id="import_opml"', fixed = TRUE)
  testthat::expect_match(html, "Import OPML", fixed = TRUE)
  testthat::expect_match(html, 'id="export_opml"', fixed = TRUE)
  testthat::expect_match(html, "Export OPML", fixed = TRUE)
})

testthat::test_that("the application shell uses the Rill duck mark", {
  config <- rill_config()
  marks <- htmltools::tagQuery(rill_ui(config))$find(
    ".brand-mark .theme-logo"
  )$selectedTags()

  testthat::expect_length(marks, 2L)
  testthat::expect_setequal(
    vapply(marks, function(mark) mark$attribs$src, character(1)),
    c("rill-assets/rill-duck.png", "rill-assets/rill-duck-dark.png")
  )
})

testthat::test_that("appearance control offers system, light, and dark modes", {
  control <- appearance_control_ui()
  html <- htmltools::renderTags(control)$html

  testthat::expect_match(html, "Appearance", fixed = TRUE)
  testthat::expect_match(html, 'value="system" checked', fixed = TRUE)
  testthat::expect_match(html, 'value="light"', fixed = TRUE)
  testthat::expect_match(html, 'value="dark"', fixed = TRUE)
})

testthat::test_that("navigation and story queue use native resizable sidebars", {
  query <- htmltools::tagQuery(rill_ui(rill_config()))
  layouts <- query$find(".bslib-sidebar-layout")$selectedTags()
  sidebars <- query$find(".bslib-sidebar-input")$selectedTags()

  testthat::expect_length(layouts, 2L)
  testthat::expect_length(sidebars, 2L)
  testthat::expect_true(all(vapply(
    sidebars,
    function(sidebar) "data-resizable" %in% names(sidebar$attribs),
    logical(1)
  )))
  testthat::expect_setequal(
    vapply(sidebars, function(sidebar) sidebar$attribs$id, character(1)),
    c("navigation_sidebar", "story_sidebar")
  )
})
