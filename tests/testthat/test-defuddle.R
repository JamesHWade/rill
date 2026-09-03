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

testthat::test_that("Defuddle metadata uses one publication timestamp", {
  testthat::local_mocked_bindings(
    fetch_defuddled_markdown = function(source_url, config) "document",
    parse_markdown_frontmatter = function(markdown) {
      list(
        metadata = list(
          published = paste(
            "2026-08-31T04:15:00+00:00,",
            "2026-08-31T00:15:00-04:00"
          )
        ),
        markdown = markdown
      )
    }
  )
  entry <- as.list(sample_rill_data()$entries[1, , drop = FALSE])
  entry$feed_title <- "Example feed"

  document <- document_from_defuddle(
    entry,
    list(defuddle_backend = "hosted")
  )

  testthat::expect_identical(
    document$published_at,
    "2026-08-31T04:15:00+00:00"
  )
})

testthat::test_that("reader feed labels do not alter captured source metadata", {
  entry <- as.list(sample_rill_data()$entries[1, , drop = FALSE])
  entry$feed_title <- "R news"
  entry$source_feed_title <- "The R Blog"

  document <- document_fallback(entry)

  testthat::expect_identical(document$site, "The R Blog")
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

testthat::test_that("today preparation extracts only uncached articles", {
  store <- rill_store(list(demo_mode = TRUE))
  store$memory$documents <- list()
  store$memory$document_heads <- character()
  store$memory$entries$published_at <- format(
    as.POSIXct("2026-08-19 12:00:00", tz = "UTC") -
      c(60, rep(60 * 60 * 24 * 40, 5)),
    tz = "UTC",
    usetz = TRUE
  )
  calls <- 0L
  testthat::local_mocked_bindings(
    document_from_defuddle = function(entry, config) {
      calls <<- calls + 1L
      new_rill_document(
        entry_id = entry$entry_id,
        source_url = entry$url,
        markdown = "Prepared reading copy.",
        acquisition_method = "web_extraction",
        producer = "defuddle-test"
      )
    }
  )
  now <- as.POSIXct("2026-08-19 14:00:00", tz = "UTC")

  first <- prepare_today_documents(
    store,
    list(actor_id = "reader"),
    now = now,
    timezone = "UTC"
  )
  second <- prepare_today_documents(
    store,
    list(actor_id = "reader"),
    now = now,
    timezone = "UTC"
  )

  testthat::expect_identical(
    first[c("total", "cached", "prepared", "failed")],
    list(total = 1L, cached = 0L, prepared = 1L, failed = 0L)
  )
  testthat::expect_identical(
    second[c("total", "cached", "prepared", "failed")],
    list(total = 1L, cached = 1L, prepared = 0L, failed = 0L)
  )
  testthat::expect_identical(calls, 1L)
})

testthat::test_that("today preparation uses the requested Reader Library", {
  store <- rill_store(list(demo_mode = TRUE, actor_id = "legacy-reader"))
  store_ensure_reader(store, "session-reader")

  result <- prepare_today_documents(
    store,
    list(actor_id = "legacy-reader"),
    reader_id = "session-reader",
    now = as.POSIXct("2026-08-19 14:00:00", tz = "UTC"),
    timezone = "UTC"
  )

  testthat::expect_identical(result$total, 0L)
})

testthat::test_that("today preparation leaves failed articles retryable", {
  store <- rill_store(list(demo_mode = TRUE))
  store$memory$documents <- list()
  store$memory$document_heads <- character()
  store$memory$entries$published_at <- format(
    as.POSIXct("2026-08-19 12:00:00", tz = "UTC") -
      c(60, rep(60 * 60 * 24 * 40, 5)),
    tz = "UTC",
    usetz = TRUE
  )
  testthat::local_mocked_bindings(
    document_from_defuddle = function(entry, config) {
      cli::cli_abort("Extraction unavailable.")
    }
  )

  result <- prepare_today_documents(
    store,
    list(actor_id = "reader"),
    now = as.POSIXct("2026-08-19 14:00:00", tz = "UTC"),
    timezone = "UTC"
  )

  testthat::expect_identical(result$failed, 1L)
  testthat::expect_length(store_list_documents(store, "reader"), 0L)
})

testthat::test_that("today preparation retries feed fallbacks", {
  store <- rill_store(list(demo_mode = TRUE))
  entry <- as.list(store$memory$entries[1, , drop = FALSE])
  store$memory$entries$published_at <- format(
    as.POSIXct("2026-08-19 12:00:00", tz = "UTC") -
      c(60, rep(60 * 60 * 24 * 40, 5)),
    tz = "UTC",
    usetz = TRUE
  )
  fallback <- document_fallback(entry, reason = "feed-fallback")
  store$memory$documents <- stats::setNames(
    list(fallback),
    fallback$document_id
  )
  store$memory$document_heads <- stats::setNames(
    fallback$document_id,
    entry$entry_id
  )
  testthat::local_mocked_bindings(
    document_from_defuddle = function(entry, config) {
      new_rill_document(
        entry_id = entry$entry_id,
        source_url = entry$url,
        markdown = "Prepared reading copy.",
        acquisition_method = "web_extraction",
        producer = "defuddle-test"
      )
    }
  )

  result <- prepare_today_documents(
    store,
    list(actor_id = "reader"),
    now = as.POSIXct("2026-08-19 14:00:00", tz = "UTC"),
    timezone = "UTC"
  )

  testthat::expect_identical(result$cached, 0L)
  testthat::expect_identical(result$prepared, 1L)
  testthat::expect_identical(
    store_get_document(store, "reader", entry$entry_id)$acquisition_method,
    "web_extraction"
  )
})

testthat::test_that("opening an Orientation feed copy upgrades it", {
  store <- rill_store(list(demo_mode = TRUE))
  store$memory$documents <- list()
  store$memory$document_heads <- character()
  entry <- as.list(store$memory$entries[1, , drop = FALSE])
  fallback <- document_fallback(entry, reason = "orientation-feed-copy")
  store_save_document(store, fallback)
  calls <- 0L
  testthat::local_mocked_bindings(
    document_from_defuddle = function(entry, config) {
      calls <<- calls + 1L
      new_rill_document(
        entry_id = entry$entry_id,
        source_url = entry$url,
        markdown = "The complete extracted article.",
        acquisition_method = "web_extraction",
        producer = "defuddle-test"
      )
    }
  )

  document <- get_or_extract_document(store, "reader", entry, list())

  testthat::expect_identical(calls, 1L)
  testthat::expect_identical(document$acquisition_method, "web_extraction")
  testthat::expect_identical(
    store_get_document(store, "reader", entry$entry_id)$document_id,
    document$document_id
  )
  testthat::expect_identical(
    store_get_document_by_id(
      store,
      "reader",
      fallback$document_id
    )$document_id,
    fallback$document_id
  )
})

testthat::test_that("opening a selected Orientation copy upgrades its selection", {
  store <- rill_store(list(demo_mode = TRUE))
  store$memory$documents <- list()
  store$memory$document_heads <- character()
  entry <- as.list(store$memory$entries[1, , drop = FALSE])
  fallback <- document_fallback(entry, reason = "orientation-feed-copy")
  store_save_document(store, fallback)
  store_select_document(store, "reader", fallback$document_id)
  calls <- 0L
  testthat::local_mocked_bindings(
    document_from_defuddle = function(entry, config) {
      calls <<- calls + 1L
      new_rill_document(
        entry_id = entry$entry_id,
        source_url = entry$url,
        markdown = "The complete extracted article.",
        acquisition_method = "web_extraction",
        producer = "defuddle-test"
      )
    }
  )

  document <- get_or_extract_document(store, "reader", entry, list())
  cached <- get_or_extract_document(store, "reader", entry, list())

  testthat::expect_identical(calls, 1L)
  testthat::expect_identical(cached$document_id, document$document_id)
  testthat::expect_identical(
    store$memory$document_selections$document_id,
    document$document_id
  )
})

testthat::test_that("opening an ordinary cached document does not extract", {
  store <- rill_store(list(demo_mode = TRUE))
  store$memory$documents <- list()
  store$memory$document_heads <- character()
  entry <- as.list(store$memory$entries[1, , drop = FALSE])
  cached <- document_fallback(entry, reason = "feed-fallback")
  store_save_document(store, cached)
  calls <- 0L
  testthat::local_mocked_bindings(
    document_from_defuddle = function(entry, config) {
      calls <<- calls + 1L
      stop("ordinary cached documents must not be re-extracted")
    }
  )

  document <- get_or_extract_document(store, "reader", entry, list())

  testthat::expect_identical(calls, 0L)
  testthat::expect_identical(document$document_id, cached$document_id)
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

testthat::test_that("rendered documents restore safe video embeds", {
  document <- list(
    markdown = paste(
      "![](https://www.youtube.com/watch?v=18PIeJoxYtc)",
      "![Youtube video](https://i.ytimg.com/vi/U4xozqrcWdQ/hqdefault.jpg)",
      sep = "\n\n"
    )
  )

  html <- as.character(render_document(document))

  testthat::expect_length(
    gregexpr("<iframe", html, fixed = TRUE)[[1]],
    2L
  )
  testthat::expect_match(
    html,
    "https://www.youtube-nocookie.com/embed/18PIeJoxYtc",
    fixed = TRUE
  )
  testthat::expect_match(html, 'sandbox="allow-scripts', fixed = TRUE)
  testthat::expect_no_match(html, "<img", fixed = TRUE)
})

testthat::test_that("the sanitizer permits only known video providers", {
  input <- paste0(
    "<iframe src='https://player.vimeo.com/video/12345' onload='bad()'></iframe>",
    "<iframe src='https://attacker.example/embed/12345'></iframe>"
  )

  html <- sanitize_rendered_html(input)

  testthat::expect_match(
    html,
    "https://player.vimeo.com/video/12345",
    fixed = TRUE
  )
  testthat::expect_no_match(html, "attacker.example", fixed = TRUE)
  testthat::expect_no_match(html, "onload", fixed = TRUE)
})

testthat::test_that("raw trusted video frames survive markdown tag filtering", {
  document <- list(
    markdown = paste0(
      "<iframe src='https://www.youtube.com/embed/18PIeJoxYtc'></iframe>",
      "<iframe src='https://attacker.example/embed/18PIeJoxYtc'></iframe>"
    )
  )

  html <- as.character(render_document(document))

  testthat::expect_match(
    html,
    "https://www.youtube-nocookie.com/embed/18PIeJoxYtc",
    fixed = TRUE
  )
  testthat::expect_no_match(html, "attacker.example", fixed = TRUE)
})
