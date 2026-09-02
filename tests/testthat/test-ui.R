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
  today <- htmltools::renderTags(empty_story_list("today"))$html
  week <- htmltools::renderTags(empty_story_list("week"))$html
  month <- htmltools::renderTags(empty_story_list("month"))$html

  testthat::expect_match(starred, "No starred stories yet", fixed = TRUE)
  testthat::expect_match(starred, "Press F", fixed = TRUE)
  testthat::expect_match(
    scoped,
    "No unread stories from The R Blog",
    fixed = TRUE
  )
  testthat::expect_match(today, "Nothing published today", fixed = TRUE)
  testthat::expect_match(week, "Nothing published this week", fixed = TRUE)
  testthat::expect_match(month, "Nothing published this month", fixed = TRUE)
})

testthat::test_that("invalid story times render without a label", {
  testthat::expect_identical(format_story_time(NA_character_), "")
  testthat::expect_identical(format_story_time("not-a-time"), "")
})

testthat::test_that("feed management exposes OPML import and export", {
  html <- htmltools::renderTags(feed_tools_ui())$html

  testthat::expect_match(html, "Manage feeds", fixed = TRUE)
  testthat::expect_match(html, 'id="rename_feed_control"', fixed = TRUE)
  testthat::expect_match(html, 'id="import_opml"', fixed = TRUE)
  testthat::expect_match(html, "Import OPML", fixed = TRUE)
  testthat::expect_match(html, 'id="export_opml"', fixed = TRUE)
  testthat::expect_match(html, "Export OPML", fixed = TRUE)
})

testthat::test_that("populated story output remains a scroll container", {
  styles <- paste(
    readLines(rill_package_file("app", "www", "styles.css"), warn = FALSE),
    collapse = "\n"
  )

  testthat::expect_match(
    styles,
    "div.story-list.shiny-html-output",
    fixed = TRUE
  )
  testthat::expect_match(styles, "display: block !important", fixed = TRUE)
})

testthat::test_that("the reading queue offers story sort dimensions", {
  html <- htmltools::renderTags(story_sidebar_ui())$html

  testthat::expect_match(html, 'id="story_sort"', fixed = TRUE)
  testthat::expect_match(html, 'value="newest" selected', fixed = TRUE)
  testthat::expect_match(html, 'value="oldest"', fixed = TRUE)
  testthat::expect_match(html, 'value="recently_added"', fixed = TRUE)
  testthat::expect_match(html, 'value="feed"', fixed = TRUE)
  testthat::expect_match(html, 'value="title"', fixed = TRUE)
})

testthat::test_that("navigation offers local calendar views", {
  html <- htmltools::renderTags(
    navigation_sidebar_ui(list(app_name = "Rill", demo_mode = FALSE))
  )$html

  testthat::expect_match(html, 'value="today"', fixed = TRUE)
  testthat::expect_match(html, "Today", fixed = TRUE)
  testthat::expect_match(html, 'value="week"', fixed = TRUE)
  testthat::expect_match(html, "This week", fixed = TRUE)
  testthat::expect_match(html, 'value="month"', fixed = TRUE)
  testthat::expect_match(html, "This month", fixed = TRUE)
})

testthat::test_that("today offers a reading-copy preparation action", {
  html <- htmltools::renderTags(prepare_today_button())$html

  testthat::expect_match(html, 'id="prepare_today"', fixed = TRUE)
  testthat::expect_match(html, "Prepare", fixed = TRUE)
  testthat::expect_match(
    html,
    'aria-label="Prepare today’s reading copies"',
    fixed = TRUE
  )
})

testthat::test_that("reading status actions explain their scope", {
  html <- htmltools::renderTags(read_actions_ui("The R Blog"))$html

  testthat::expect_match(html, 'id="mark_all_read"', fixed = TRUE)
  testthat::expect_match(html, 'id="mark_older_read"', fixed = TRUE)
  testthat::expect_match(html, "The R Blog", fixed = TRUE)
  testthat::expect_match(html, "Open history stays unchanged", fixed = TRUE)
})

testthat::test_that("the reader offers an explicit unread action", {
  html <- htmltools::renderTags(mark_unread_button())$html

  testthat::expect_match(html, 'id="mark_unread"', fixed = TRUE)
  testthat::expect_match(html, "Mark unread", fixed = TRUE)
})

testthat::test_that("the reader includes a source-bounded Conversation", {
  html <- htmltools::renderTags(reader_pane_ui())$html

  testthat::expect_match(html, 'id="reader_chat"', fixed = TRUE)
  testthat::expect_match(html, "enable-cancel", fixed = TRUE)
  testthat::expect_match(html, 'id="reader_agent_status"', fixed = TRUE)
  testthat::expect_match(html, 'data-open-mobile="closed"', fixed = TRUE)
  testthat::expect_match(html, "Ask Rill about this story", fixed = TRUE)
  testthat::expect_match(html, "selected reading copy only", fixed = TRUE)
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

testthat::test_that("the application panes use native resizable sidebars", {
  query <- htmltools::tagQuery(rill_ui(rill_config()))
  layouts <- query$find(".bslib-sidebar-layout")$selectedTags()
  sidebars <- query$find(".bslib-sidebar-input")$selectedTags()

  testthat::expect_length(layouts, 3L)
  testthat::expect_length(sidebars, 3L)
  testthat::expect_true(all(vapply(
    sidebars,
    function(sidebar) "data-resizable" %in% names(sidebar$attribs),
    logical(1)
  )))
  testthat::expect_setequal(
    vapply(sidebars, function(sidebar) sidebar$attribs$id, character(1)),
    c("navigation_sidebar", "story_sidebar", "reader_agent_sidebar")
  )
})
