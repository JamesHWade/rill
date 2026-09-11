testthat::test_that("queue cards reuse unchanged HTML and refresh Reader state", {
  rows <- sample_rill_data()$entries[rep(1L, 3L), ]
  rows$entry_id <- c("one", "two", "three")
  rows$feed_title <- "Source"
  rows$read_at <- NA_character_
  rows$saved <- FALSE
  rows$starred <- FALSE
  renderer <- queue_card_renderer()
  preview <- \(entry) NULL
  first <- renderer(rows, NULL, preview)
  testthat::expect_identical(first$built, 3L)
  second <- renderer(rows, NULL, preview)
  testthat::expect_identical(second$built, 0L)
  testthat::expect_identical(second$cards, first$cards)
  selected <- renderer(rows, "two", preview)
  testthat::expect_identical(selected$built, 1L)
  testthat::expect_match(
    selected$cards[[2]],
    'aria-current="true"',
    fixed = TRUE
  )
  rows$read_at[[2]] <- "2026-09-10 12:00:00 UTC"
  rows$saved[[1]] <- TRUE
  changed <- renderer(rows, "two", preview)
  testthat::expect_identical(changed$built, 2L)
  testthat::expect_match(changed$cards[[1]], '>Saved</span>', fixed = TRUE)
  testthat::expect_match(changed$cards[[2]], '>Read</span>', fixed = TRUE)
  testthat::expect_identical(renderer(rows[0, ], NULL, preview)$cards, list())
  testthat::expect_identical(renderer(rows, NULL, preview)$built, 3L)
  testthat::expect_identical(
    queue_card_renderer()(rows, NULL, preview)$built,
    3L
  )
})

testthat::test_that("cached publication times still refresh relative labels", {
  rows <- sample_rill_data()$entries[1L, ]
  rows$read_at <- NA_character_
  renderer <- queue_card_renderer()
  label <- "59m"
  parse <- parse_story_time
  parsed_strings <- 0L
  testthat::local_mocked_bindings(
    format_story_time = function(...) label,
    parse_story_time = function(value) {
      if (is.character(value)) {
        parsed_strings <<- parsed_strings + 1L
      }
      parse(value)
    }
  )
  first <- renderer(rows, NULL, \(entry) NULL)
  testthat::expect_match(first$cards[[1L]], "59m", fixed = TRUE)
  testthat::expect_identical(renderer(rows, NULL, \(entry) NULL)$built, 0L)
  testthat::expect_identical(parsed_strings, 1L)
  label <- "1h"
  later <- renderer(rows, NULL, \(entry) NULL)
  testthat::expect_identical(later$built, 1L)
  testthat::expect_match(later$cards[[1L]], "1h", fixed = TRUE)
  testthat::expect_identical(parsed_strings, 1L)
  rows$published_at <- "2026-09-10 00:00:00 UTC"
  renderer(rows, NULL, \(entry) NULL)
  testthat::expect_identical(parsed_strings, 2L)
})

testthat::test_that("queue batches expand and reset when the context changes", {
  shiny::testServer(
    function(input, output, session) {
      batch <- queue_batch_server(\() input$context, input)
    },
    {
      session$setInputs(context = "all")
      testthat::expect_identical(batch(), 30L)
      session$setInputs(queue_more = 1L)
      testthat::expect_identical(batch(), 60L)
      session$setInputs(queue_more = 2L)
      testthat::expect_identical(batch(), 90L)
      session$setInputs(context = "today")
      testthat::expect_identical(batch(), 30L)
    }
  )
})

testthat::test_that("cached queue icons preserve their accessible markup", {
  for (name in c("circle", "circle-fill", "bookmark", "check2-circle")) {
    testthat::expect_identical(
      as.character(queue_icon(name)),
      as.character(htmltools::renderTags(bsicons::bs_icon(name))$html)
    )
  }
  testthat::expect_match(queue_icon("star-fill", "Starred"), "Starred")
})

testthat::test_that("removing a story reindexes cached cards without rebuilding them", {
  rows <- sample_rill_data()$entries[rep(1L, 2L), ]
  rows$entry_id <- c("one", "two")
  rows$read_at <- NA_character_
  renderer <- queue_card_renderer()
  renderer(rows, NULL, \(entry) NULL)
  remaining <- renderer(rows[2L, ], NULL, \(entry) NULL)
  testthat::expect_identical(remaining$built, 0L)
  testthat::expect_match(
    remaining$cards[[1]],
    'data-queue-index="1"',
    fixed = TRUE
  )
})


testthat::test_that("calendar polls preserve expanded batches until context changes", {
  shiny::testServer(
    function(input, output, session) {
      context <- shiny::reactive({
        input$tick
        list(view = input$view, since = input$since)
      })
      batch <- queue_batch_server(context, input)
    },
    {
      session$setInputs(view = "today", since = "2026-09-10", tick = 1L)
      session$setInputs(queue_more = 1L)
      session$setInputs(queue_more = 2L)
      testthat::expect_identical(batch(), 90L)
      session$setInputs(tick = 2L)
      testthat::expect_identical(batch(), 90L)
      session$setInputs(tick = 3L)
      testthat::expect_identical(batch(), 90L)
      session$setInputs(since = "2026-09-11", tick = 4L)
      testthat::expect_identical(batch(), 30L)
      session$setInputs(queue_more = 3L)
      testthat::expect_identical(batch(), 60L)
      session$setInputs(view = "week")
      testthat::expect_identical(batch(), 30L)
    }
  )
})


testthat::test_that("queue batches include a selection moved beyond the boundary", {
  shiny::testServer(
    function(input, output, session) {
      batch <- queue_batch_server(
        \() input$context,
        input,
        \() input$selected_index
      )
    },
    {
      session$setInputs(context = "all", selected_index = 30L)
      testthat::expect_identical(batch(), 30L)
      session$setInputs(selected_index = 31L)
      testthat::expect_identical(batch(), 31L)
      session$setInputs(queue_more = 1L)
      testthat::expect_identical(batch(), 61L)
      session$setInputs(selected_index = 0L)
      testthat::expect_identical(batch(), 61L)
      session$setInputs(context = "today")
      testthat::expect_identical(batch(), 30L)
    }
  )
})
