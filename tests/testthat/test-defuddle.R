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

testthat::test_that("feed fallbacks preserve Markdown autolinks and adjacent text", {
  entry <- as.list(sample_rill_data()$entries[1, , drop = FALSE])
  entry$feed_content <- paste(
    "See <https://example.com/article> for **details**.",
    "Contact <reader@example.com> or <mailto:editor@example.com> for help.",
    sep = "\n\n"
  )

  document <- document_fallback(entry, reason = "feed-fallback")
  html <- xml2::read_html(as.character(render_document(document)))

  testthat::expect_identical(document$markdown, entry$feed_content)
  testthat::expect_identical(
    xml2::xml_attr(xml2::xml_find_all(html, ".//a"), "href"),
    c(
      "https://example.com/article",
      "mailto:reader@example.com",
      "mailto:editor@example.com"
    )
  )
  testthat::expect_identical(
    xml2::xml_text(xml2::xml_find_first(html, ".//strong")),
    "details"
  )
  testthat::expect_match(xml2::xml_text(html), "for help.", fixed = TRUE)
})

testthat::test_that("Markdown feed copies resolve source-relative links and images", {
  entry <- as.list(sample_rill_data()$entries[1, , drop = FALSE])
  entry$url <- "https://example.com/posts/story/"
  entry$feed_content <- paste(
    "Read [details](/posts/details) and [more][related].",
    "![Image](../image.png)",
    "[related]: related?mode=full",
    sep = "\n\n"
  )

  html <- xml2::read_html(as.character(render_document(document_fallback(
    entry
  ))))

  testthat::expect_identical(
    xml2::xml_attr(xml2::xml_find_all(html, ".//a"), "href"),
    c(
      "https://example.com/posts/details",
      "https://example.com/posts/story/related?mode=full"
    )
  )
  testthat::expect_identical(
    xml2::xml_attr(xml2::xml_find_all(html, ".//img"), "src"),
    "https://example.com/posts/image.png"
  )
})

testthat::test_that("resolving source URLs keeps in-document fragment links local", {
  entry <- as.list(sample_rill_data()$entries[1, , drop = FALSE])
  entry$url <- "https://example.com/posts/story/"
  entry$feed_content <- paste(
    "[Notes](#notes) and <a href='#details'>details</a>.",
    "<h2 id='notes'>Notes</h2><h2 id='details'>Details</h2>",
    "![Image](photo.png)",
    sep = "\n\n"
  )

  html <- xml2::read_html(as.character(render_document(document_fallback(
    entry
  ))))

  testthat::expect_identical(
    xml2::xml_attr(xml2::xml_find_all(html, ".//a"), "href"),
    c("#notes", "#details")
  )
  testthat::expect_identical(
    xml2::xml_attr(xml2::xml_find_all(html, ".//h2"), "id"),
    c("notes", "details")
  )
  testthat::expect_identical(
    xml2::xml_attr(xml2::xml_find_all(html, ".//img"), "src"),
    "https://example.com/posts/story/photo.png"
  )
})

testthat::test_that("resolving source URLs keeps generated footnotes local", {
  entry <- as.list(sample_rill_data()$entries[1, , drop = FALSE])
  entry$url <- "https://example.com/posts/story/"
  entry$feed_content <- "Text[^note].\n\n![Image](photo.png)\n\n[^note]: A note."

  html <- xml2::read_html(as.character(render_document(document_fallback(
    entry
  ))))

  testthat::expect_identical(
    xml2::xml_attr(
      xml2::xml_find_all(
        html,
        ".//a[@data-footnote-ref or @data-footnote-backref]"
      ),
      "href"
    ),
    c("#fn-note", "#fnref-note")
  )
  testthat::expect_identical(
    xml2::xml_attr(xml2::xml_find_all(html, ".//img"), "src"),
    "https://example.com/posts/story/photo.png"
  )
})

testthat::test_that("HTML feed copies resolve bare relative destinations before sanitizing", {
  content <- feed_content_markdown(
    "<p><a href='details'>Details</a><img src='photo.png'><a href='javascript:unsafe()'>Unsafe</a></p>",
    "https://example.com/posts/story/"
  )
  html <- xml2::read_html(content)

  testthat::expect_identical(
    xml2::xml_attr(xml2::xml_find_all(html, ".//a"), "href"),
    c("https://example.com/posts/story/details", NA_character_)
  )
  testthat::expect_identical(
    xml2::xml_attr(xml2::xml_find_all(html, ".//img"), "src"),
    "https://example.com/posts/story/photo.png"
  )
})

testthat::test_that("feed copies preserve Markdown around embedded HTML", {
  entry <- as.list(sample_rill_data()$entries[1, , drop = FALSE])
  entry$url <- "https://example.com/article"
  entry$feed_content <- paste(
    "# Heading\n\n**Important**: <https://example.com/details>",
    "<img src='/photo.jpg' alt='Photo'>",
    "Read [the source](https://example.com/source) and *consider it*.",
    "- First point\n- Second point",
    "<script>unsafe()</script>",
    sep = "\n\n"
  )

  html <- xml2::read_html(as.character(render_document(document_fallback(
    entry
  ))))

  testthat::expect_identical(
    xml2::xml_text(xml2::xml_find_all(html, ".//h1")),
    "Heading"
  )
  testthat::expect_identical(
    xml2::xml_text(xml2::xml_find_all(html, ".//strong")),
    "Important"
  )
  testthat::expect_identical(
    xml2::xml_text(xml2::xml_find_all(html, ".//em")),
    "consider it"
  )
  testthat::expect_identical(
    xml2::xml_attr(xml2::xml_find_all(html, ".//a"), "href"),
    c("https://example.com/details", "https://example.com/source")
  )
  testthat::expect_identical(
    xml2::xml_attr(xml2::xml_find_all(html, ".//img"), "src"),
    "https://example.com/photo.jpg"
  )
  testthat::expect_length(xml2::xml_find_all(html, ".//li"), 2L)
  testthat::expect_no_match(as.character(html), "script|unsafe")
})

testthat::test_that("feed copies recognize HTML tag boundaries", {
  for (content in c(
    "<A HREF='/article'>Read this.</A>",
    "<a href='/article'>Read this.</a>",
    "<a\nhref='/article'>Read this.</a><br/>More."
  )) {
    html <- xml2::read_html(feed_content_markdown(
      content,
      "https://example.com/article"
    ))
    testthat::expect_identical(
      xml2::xml_attr(xml2::xml_find_all(html, ".//a"), "href"),
      "https://example.com/article"
    )
    testthat::expect_match(xml2::xml_text(html), "Read this.", fixed = TRUE)
  }
})

testthat::test_that("feed fallbacks retain article structure safely", {
  entry <- as.list(sample_rill_data()$entries[1, , drop = FALSE])
  entry$url <- "https://example.com/articles/pelicans/"
  entry$feed_content <- paste0(
    "<p>Read the <a href='/comparison/'>comparison grid</a>.</p>",
    "<p><img src='/grid.webp' alt='Pelican comparison grid'></p>",
    "<ul><li>First observation.</li><li>Second observation.</li></ul>",
    "<base href='https://unsafe.example/'><script>unsafe()</script>",
    "<p onclick='unsafe()'>Conclusion.</p>"
  )

  document <- document_fallback(entry, reason = "feed-fallback")
  html <- xml2::read_html(as.character(render_document(document)))

  testthat::expect_length(xml2::xml_find_all(html, ".//li"), 2L)
  testthat::expect_length(xml2::xml_find_all(html, ".//p"), 3L)
  testthat::expect_identical(
    xml2::xml_attr(xml2::xml_find_first(html, ".//img"), "alt"),
    "Pelican comparison grid"
  )
  testthat::expect_identical(
    xml2::xml_attr(xml2::xml_find_first(html, ".//a"), "href"),
    "https://example.com/comparison/"
  )
  testthat::expect_no_match(
    as.character(render_document(document)),
    "script|onclick|unsafe|<base"
  )
  testthat::expect_identical(document$acquisition_method, "feed_fallback")
})

testthat::test_that("feed copies retain image-only articles but reject empty markup", {
  image <- feed_content_markdown(
    "<img src='/grid.webp' alt='Comparison'>",
    "https://example.com/article"
  )
  testthat::expect_match(
    image,
    'src="https://example.com/grid.webp"',
    fixed = TRUE
  )
  testthat::expect_error(
    feed_content_markdown("<p> </p>", "https://example.com/article"),
    class = "rill_document_invalid"
  )
  testthat::expect_error(
    feed_content_markdown(
      "<script>unsafe()</script>",
      "https://example.com/article"
    ),
    class = "rill_document_invalid"
  )
})

testthat::test_that("feed copies retain HTML5 media with nested sources", {
  entry <- as.list(sample_rill_data()$entries[1, , drop = FALSE])
  entry$url <- "https://example.com/posts/story/"

  for (media in c("video", "audio")) {
    entry$feed_content <- paste0(
      "<",
      media,
      " controls><source src='clip.mp4'>",
      "<source src='../clip.ogg'></",
      media,
      ">"
    )
    html <- xml2::read_html(as.character(render_document(document_fallback(
      entry
    ))))

    testthat::expect_length(
      xml2::xml_find_all(html, paste0(".//", media, "[@controls]")),
      1L
    )
    testthat::expect_identical(
      xml2::xml_attr(
        xml2::xml_find_all(html, paste0(".//", media, "//source")),
        "src"
      ),
      c(
        "https://example.com/posts/story/clip.mp4",
        "https://example.com/posts/clip.ogg"
      )
    )
  }
})

testthat::test_that("feed Markdown must render readable content even when unchanged", {
  for (content in c(
    "<!-- Only a comment -->",
    "[details]: https://example.com/details",
    " \n\t"
  )) {
    testthat::expect_error(
      feed_content_markdown(content, "https://example.com/article"),
      class = "rill_document_invalid"
    )
  }
  markdown <- "<!-- A comment -->\n\n**Readable text**."
  testthat::expect_identical(
    feed_content_markdown(markdown, "https://example.com/article"),
    markdown
  )
})

testthat::test_that("feed-copy word counts use visible text with block boundaries", {
  entry <- as.list(sample_rill_data()$entries[1, , drop = FALSE])
  entry$url <- "https://example.com/posts/story/"
  cases <- list(
    list(content = "<p>Hello</p><p>reader</p>", words = 2L),
    list(content = "<ul><li>One</li><li>Two</li></ul>", words = 2L),
    list(content = "<p>un<strong>break</strong>able</p>", words = 1L),
    list(content = "<p>one<br>two</p>", words = 2L),
    list(content = "Read [these details](details).", words = 3L),
    list(content = "# Title\n\nWords in **bold**.", words = 4L),
    list(
      content = "<img src='image.png' alt='An image with many words'>",
      words = 0L
    ),
    list(
      content = paste0(
        "<div class='one two three'><p>Hello <strong>reader</strong>.</p>",
        "<p><a href='details' title='A long link title'>Read this</a></p></div>"
      ),
      words = 4L
    )
  )

  for (case in cases) {
    entry$feed_content <- case$content
    document <- document_fallback(entry)

    testthat::expect_identical(
      document$word_count,
      case$words,
      info = case$content
    )
  }
})

testthat::test_that("preparation does not replace a newer browser capture selection", {
  store <- preparation_test_store()
  entry <- as.list(store$memory$entries[1, , drop = FALSE])
  fallback <- document_fallback(entry, reason = "feed-fallback")
  captured <- new_rill_document(
    entry$entry_id,
    entry$url,
    "Private browser copy.",
    "browser_capture",
    "clipper",
    reader_id = "reader"
  )
  prepared <- new_rill_document(
    entry$entry_id,
    entry$url,
    "Full public article.",
    "web_extraction",
    "defuddle-test"
  )
  store_save_document(store, fallback)
  store_select_document(store, "reader", fallback$document_id)
  store_save_document(store, captured)
  store_select_document(store, "reader", captured$document_id)

  save_prepared_document(store, "reader", fallback, prepared)

  testthat::expect_identical(
    store_get_document(store, "reader", entry$entry_id)$document_id,
    captured$document_id
  )
  testthat::expect_identical(
    store_get_document_by_id(store, "reader", fallback$document_id)$markdown,
    fallback$markdown
  )
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

testthat::test_that("the bundled extractor selects an available local runtime", {
  node <- bundled_defuddle_invocation(c(
    node = "/runtime/node",
    deno = "/runtime/deno"
  ))
  testthat::expect_identical(node$command, "/runtime/node")
  testthat::expect_identical(
    node$args,
    rill_package_file("defuddle", "defuddle.cjs")
  )
  deno <- bundled_defuddle_invocation(c(node = "", deno = "/runtime/deno"))
  testthat::expect_identical(deno$command, "/runtime/deno")
  testthat::expect_identical(deno$args[[1L]], "run")
  testthat::expect_contains(deno$args, "--cached-only")
  testthat::expect_contains(deno$args, "--no-prompt")
  testthat::expect_identical("--allow-env" %in% deno$args, FALSE)
  testthat::expect_error(
    bundled_defuddle_invocation(c(node = "", deno = "")),
    class = "rill_defuddle_cli_missing"
  )
})

testthat::test_that("the shipped CLI extracts a structured article on Node and Deno", {
  available <- Sys.which(c("node", "deno"))
  testthat::skip_if_not(any(nzchar(available)), "Node or Deno is required")
  path <- withr::local_tempfile(fileext = ".html")
  writeLines(
    paste0(
      "<!doctype html><html><head><title>Building a reading room</title>",
      "<meta name='author' content='An author'></head><body><article>",
      "<h1>Building a reading room</h1>",
      "<p>A reading room needs comfortable chairs, good lighting, and a place ",
      "to put books. This first paragraph explains the design decisions.</p>",
      "<h2>Materials</h2><p>The second paragraph explains how to choose ",
      "materials that will last, and links to ",
      "<a href='https://example.com/materials'>the materials guide</a>.</p>",
      "<ul><li>Wooden shelves</li><li>Adjustable lighting</li></ul>",
      "<img src='https://example.com/room.jpg' alt='Reading room'>",
      "</article></body></html>"
    ),
    path
  )
  for (runtime in names(available)[nzchar(available)]) {
    runtimes <- c(node = "", deno = "")
    runtimes[[runtime]] <- available[[runtime]]
    invocation <- bundled_defuddle_invocation(runtimes)
    result <- run_defuddle_cli(
      invocation$command,
      c(invocation$args, "parse", path, "--md", "--frontmatter"),
      timeout = 30
    )
    testthat::expect_identical(result$status, 0L, info = result$stderr)
    parsed <- parse_markdown_frontmatter(result$stdout)
    testthat::expect_identical(parsed$metadata$title, "Building a reading room")
    testthat::expect_identical(parsed$metadata$author, "An author")
    html <- xml2::read_html(as.character(render_document(list(
      markdown = parsed$markdown
    ))))
    testthat::expect_identical(
      xml2::xml_text(xml2::xml_find_first(html, ".//h2")),
      "Materials"
    )
    testthat::expect_length(xml2::xml_find_all(html, ".//p"), 2L)
    testthat::expect_length(xml2::xml_find_all(html, ".//li"), 2L)
    testthat::expect_identical(
      xml2::xml_attr(xml2::xml_find_first(html, ".//a"), "href"),
      "https://example.com/materials"
    )
    testthat::expect_identical(
      xml2::xml_attr(xml2::xml_find_first(html, ".//img"), "src"),
      "https://example.com/room.jpg"
    )
  }
})

testthat::test_that("bundled extraction records the shipped producer version", {
  store <- preparation_test_store()
  entry <- as.list(store$memory$entries[1, , drop = FALSE])
  testthat::local_mocked_bindings(
    fetch_defuddled_markdown = function(...) {
      "# Extracted article\n\nReadable content."
    }
  )
  document <- document_from_defuddle(
    entry,
    list(defuddle_backend = "local", defuddle_command = "bundled")
  )
  testthat::expect_identical(document$producer, "defuddle-local")
  testthat::expect_identical(document$producer_version, "0.19.3")
  testthat::expect_identical(document$acquisition_method, "web_extraction")
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

testthat::test_that("preparation failures expose safe extraction diagnostics", {
  store <- preparation_test_store()
  testthat::local_mocked_bindings(
    fetch_defuddled_markdown = function(source_url, config) {
      rlang::abort(
        "Request https://secret.example/?key=private-token failed: private body",
        parent = rlang::error_cnd("httr2_http_403")
      )
    }
  )

  logs <- testthat::capture_messages({
    result <- prepare_today_documents(
      store,
      list(actor_id = "reader", defuddle_backend = "hosted"),
      now = as.POSIXct("2026-08-19 14:00:00", tz = "UTC"),
      timezone = "UTC"
    )
  })

  failure <- result$failures[[1]]
  testthat::expect_identical(result$failed, 1L)
  testthat::expect_identical(failure$stage, "extraction")
  testthat::expect_identical(failure$http_status, 403L)
  testthat::expect_identical(failure$title, store$memory$entries$title[[1]])
  testthat::expect_match(logs, failure$reference, fixed = TRUE)
  testthat::expect_no_match(logs, failure$title, fixed = TRUE)
  testthat::expect_no_match(
    jsonlite::toJSON(result),
    "private-token|private body|secret.example"
  )
  testthat::expect_no_match(logs, "private-token|private body|secret.example")
  testthat::expect_length(store_list_documents(store, "reader"), 0L)
})

testthat::test_that("preparation distinguishes saving from extraction failures", {
  store <- preparation_test_store()
  testthat::local_mocked_bindings(
    fetch_defuddled_markdown = function(source_url, config) "Reading copy.",
    store_save_document = function(...) stop("postgresql://user:password@db")
  )

  logs <- testthat::capture_messages({
    result <- prepare_today_documents(
      store,
      list(actor_id = "reader"),
      now = as.POSIXct("2026-08-19 14:00:00", tz = "UTC"),
      timezone = "UTC"
    )
  })

  testthat::expect_identical(result$failures[[1]]$stage, "storage")
  testthat::expect_identical(result$failures[[1]]$code, "storage_failed")
  testthat::expect_match(result$errors[[1]], "saved", fixed = TRUE)
  testthat::expect_no_match(logs, "postgresql|password")
  testthat::expect_length(store_list_documents(store, "reader"), 0L)
})

testthat::test_that("preparation diagnostics recognize real HTTP conditions", {
  error <- tryCatch(
    httr2::resp_check_status(httr2::response(
      status_code = 429L,
      url = "https://example.com/?key=private-token"
    )),
    error = identity
  )

  logs <- testthat::capture_messages({
    failure <- preparation_failure(error, "extraction", list())
  })

  testthat::expect_identical(failure$http_status, 429L)
  testthat::expect_identical(failure$error_type, "httr2_http_429")
  testthat::expect_match(failure$message, "HTTP 429", fixed = TRUE)
  testthat::expect_no_match(logs, "example.com|private-token")
})

testthat::test_that("preparation diagnostics classify known non-HTTP failures", {
  errors <- lapply(
    c(
      "rill_defuddle_cli_missing",
      "rill_defuddle_cli_failed",
      "rill_document_invalid",
      "curl_error_operation_timedout",
      "httr2_failure",
      "private-token"
    ),
    rlang::error_cnd,
    message = "private body"
  )
  errors[[6]] <- structure(
    list(message = "private body"),
    class = c("private-token", "error", "condition")
  )

  logs <- testthat::capture_messages({
    failures <- lapply(
      errors,
      preparation_failure,
      stage = "extraction",
      config = list(defuddle_backend = "private-token")
    )
  })

  testthat::expect_identical(
    vapply(failures, `[[`, character(1), "code"),
    c(
      "extractor_missing",
      "extractor_failed",
      "invalid_document",
      "request_timeout",
      "request_failed",
      "extraction_failed"
    )
  )
  testthat::expect_no_match(
    paste(logs, collapse = "\n"),
    "private-token|private body"
  )
  testthat::expect_identical(failures[[6]]$error_type, "unknown_error")
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
  store_select_document(store, "reader", fallback$document_id)
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

testthat::test_that("today preparation upgrades a selected Orientation copy once", {
  store <- rill_store(list(demo_mode = TRUE))
  store$memory$documents <- list()
  store$memory$document_heads <- character()
  entry <- as.list(store$memory$entries[1, , drop = FALSE])
  store$memory$entries$published_at <- format(
    as.POSIXct("2026-08-19 12:00:00", tz = "UTC") -
      c(60, rep(60 * 60 * 24 * 40, 5)),
    tz = "UTC",
    usetz = TRUE
  )
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

  testthat::expect_identical(calls, 1L)
  testthat::expect_identical(first$prepared, 1L)
  testthat::expect_identical(second$cached, 1L)
  testthat::expect_no_match(
    store$memory$document_selections$document_id,
    fallback$document_id,
    fixed = TRUE
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

testthat::test_that("feed-copy video embeds survive the complete rendering pipeline", {
  entry <- as.list(sample_rill_data()$entries[1, , drop = FALSE])
  entry$url <- "https://example.com/posts/story/"
  entry$feed_content <- paste(
    "# Video notes",
    "![YouTube](https://i.ytimg.com/vi/U4xozqrcWdQ/hqdefault.jpg)",
    "<div><iframe src='https://player.vimeo.com/video/12345' onload='bad()'></iframe></div>",
    "<iframe src='https://attacker.example/embed/12345'></iframe>",
    "Read [the details](details).",
    sep = "\n\n"
  )

  document <- document_fallback(entry, reason = "feed-fallback")
  html <- xml2::read_html(as.character(render_document(document)))
  frames <- xml2::xml_find_all(html, ".//iframe")

  testthat::expect_identical(
    xml2::xml_attr(frames, "src"),
    c(
      "https://www.youtube-nocookie.com/embed/U4xozqrcWdQ",
      "https://player.vimeo.com/video/12345"
    )
  )
  testthat::expect_identical(
    xml2::xml_attr(frames, "sandbox"),
    rep("allow-scripts allow-same-origin allow-presentation", 2L)
  )
  testthat::expect_identical(
    xml2::xml_attr(frames, "onload"),
    rep(NA_character_, 2L)
  )
  testthat::expect_identical(
    xml2::xml_text(xml2::xml_find_first(html, ".//h1")),
    "Video notes"
  )
  testthat::expect_identical(
    xml2::xml_attr(xml2::xml_find_first(html, ".//a"), "href"),
    "https://example.com/posts/story/details"
  )
  testthat::expect_no_match(xml2::xml_text(html), "![](", fixed = TRUE)
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
testthat::test_that("Orientation feed copies preserve readable structure", {
  entry <- list(
    entry_id = "structured-feed",
    url = "https://example.com/article",
    title = "Structured feed",
    feed_content = paste0(
      "<h2>Projects</h2><p>One.</p><p>Two.</p>",
      "<ul><li><a href='/project'>Project</a></li></ul>",
      "<img src='/photo.jpg' alt='Photo'>"
    )
  )
  document <- document_fallback(entry, reason = "orientation-feed-copy")
  html <- xml2::read_html(as.character(render_document(document)))
  testthat::expect_length(xml2::xml_find_all(html, ".//h2"), 1L)
  testthat::expect_length(xml2::xml_find_all(html, ".//p"), 2L)
  testthat::expect_length(xml2::xml_find_all(html, ".//li/a"), 1L)
  testthat::expect_identical(
    xml2::xml_attr(xml2::xml_find_first(html, ".//img"), "src"),
    "https://example.com/photo.jpg"
  )
})
testthat::test_that("extraction diagnostics classify gateways without retaining response content", {
  response <- httr2::response(
    status_code = 403L,
    headers = list(
      "content-type" = "text/html; charset=utf-8",
      "server" = "cloudflare",
      "cf-mitigated" = "challenge",
      "set-cookie" = "private-secret"
    ),
    body = charToRaw("<html>Private article URL and private-secret</html>")
  )
  attributes <- extraction_response_attributes(response)
  testthat::expect_identical(attributes$http.response.challenge, TRUE)
  testthat::expect_identical(attributes$http.response.content_type, "text/html")
  testthat::expect_no_match(
    jsonlite::toJSON(attributes),
    "private|article|cookie"
  )
})
