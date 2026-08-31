testthat::test_that("Defuddle frontmatter is separated from markdown", {
  input <- paste(
    "---",
    "title: A clean page",
    "author: Grace Hopper",
    "domain: example.com",
    "---",
    "",
    "## The document",
    "",
    "Readable text.",
    sep = "\n"
  )

  result <- parse_markdown_frontmatter(input)

  testthat::expect_equal(result$metadata$title, "A clean page")
  testthat::expect_equal(result$metadata$author, "Grace Hopper")
  testthat::expect_match(result$markdown, "## The document", fixed = TRUE)
})

testthat::test_that("plain markdown without frontmatter is unchanged", {
  input <- "# Hello\n\nThis is the body."
  result <- parse_markdown_frontmatter(input)

  testthat::expect_length(result$metadata, 0L)
  testthat::expect_identical(result$markdown, input)
})

testthat::test_that("local Defuddle uses the CLI markdown contract", {
  call <- NULL
  runner <- function(command, args, timeout) {
    call <<- list(command = command, args = args, timeout = timeout)
    list(
      status = 0L,
      stdout = "---\ntitle: Local copy\n---\n\nReadable text.",
      stderr = ""
    )
  }
  config <- list(defuddle_command = "/opt/defuddle")

  markdown <- fetch_defuddled_markdown_local(
    "https://example.com/articles/one?from=rill&format=full",
    config,
    runner = runner
  )

  testthat::expect_identical(call$command, "/opt/defuddle")
  testthat::expect_identical(
    call$args,
    c(
      "parse",
      "https://example.com/articles/one?from=rill&format=full",
      "--md",
      "--frontmatter",
      "--user-agent",
      rill_user_agent()
    )
  )
  testthat::expect_identical(call$timeout, 30)
  testthat::expect_match(markdown, "title: Local copy", fixed = TRUE)
})

testthat::test_that("local Defuddle reports CLI failures", {
  runner <- function(command, args, timeout) {
    list(status = 1L, stdout = "", stderr = "Unable to fetch the page")
  }

  testthat::expect_snapshot(
    fetch_defuddled_markdown_local(
      "https://example.com/article",
      list(defuddle_command = "defuddle"),
      runner = runner
    ),
    error = TRUE
  )
})

testthat::test_that("local Defuddle requires an installed executable", {
  testthat::expect_snapshot(
    run_defuddle_cli(
      "rill-defuddle-command-that-does-not-exist",
      character(),
      timeout = 1
    ),
    error = TRUE
  )
})

testthat::test_that("rendered documents drop executable markup", {
  dirty <- paste0(
    "<p onclick='alert(1)'>Safe words</p>",
    "<script>alert(2)</script>",
    "<a href='javascript:alert(3)'>bad link</a>"
  )
  clean <- sanitize_rendered_html(dirty)

  testthat::expect_match(clean, "Safe words")
  testthat::expect_no_match(
    clean,
    "onclick|script|javascript:",
    ignore.case = TRUE
  )
})
