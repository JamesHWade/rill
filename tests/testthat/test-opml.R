testthat::test_that("read_opml reads nested subscriptions and skips comments", {
  file <- withr::local_tempfile(fileext = ".opml")
  writeLines(
    c(
      "<?xml version=\"1.0\" encoding=\"UTF-8\"?>",
      "<opml version=\"2.0\"><head><title>Reader export</title></head><body>",
      "<outline text=\"Data\"><outline text=\"R\">",
      "<outline type=\"rss\" text=\"R and Data\" title=\"R &amp; Data\" xmlUrl=\"https://example.com/feed.xml\" htmlUrl=\"https://example.com/\"/>",
      "</outline></outline>",
      "<outline type=\"rss\" text=\"Ungrouped\" xmlUrl=\"https://example.org/atom.xml\"/>",
      "<outline text=\"Old\" isComment=\"true\"><outline type=\"rss\" text=\"Ignored\" xmlUrl=\"https://ignored.example/feed\"/></outline>",
      "</body></opml>"
    ),
    file
  )

  subscriptions <- read_opml(file)

  testthat::expect_identical(
    subscriptions$title,
    c("R & Data", "Ungrouped")
  )
  testthat::expect_identical(
    subscriptions$folder,
    c("Data / R", "Unsorted")
  )
  testthat::expect_identical(
    subscriptions$site_url,
    c("https://example.com/", NA_character_)
  )
})

testthat::test_that("write_opml creates a portable subscription list", {
  file <- withr::local_tempfile(fileext = ".opml")
  feeds <- data.frame(
    title = c("R Weekly", "R Blog", "Loose feed"),
    feed_url = c(
      "https://rweekly.org/atom.xml",
      "https://blog.r-project.org/feed.xml",
      "https://example.net/feed"
    ),
    site_url = c(
      "https://rweekly.org/",
      "https://www.r-project.org/",
      NA_character_
    ),
    folder = c("Community", "Data / R", "Unsorted"),
    stringsAsFactors = FALSE
  )

  result <- write_opml(feeds, file, title = "My subscriptions")
  document <- xml2::read_xml(file)
  roundtrip <- read_opml(file)

  testthat::expect_identical(result, file)
  testthat::expect_identical(
    xml2::xml_attr(xml2::xml_root(document), "version"),
    "2.0"
  )
  testthat::expect_identical(
    xml2::xml_text(xml2::xml_find_first(document, "//head/title")),
    "My subscriptions"
  )
  testthat::expect_length(
    xml2::xml_find_all(document, "//outline[@type='rss'][@text][@xmlUrl]"),
    3L
  )
  testthat::expect_equal(
    roundtrip[c("title", "feed_url", "site_url", "folder")],
    normalize_opml_subscriptions(feeds)
  )
})

testthat::test_that("read_opml explains malformed input", {
  file <- withr::local_tempfile(fileext = ".opml", lines = "<not-opml />")

  testthat::expect_snapshot(read_opml(file), error = TRUE)
})

testthat::test_that("write_opml requires HTTP feed URLs", {
  feeds <- data.frame(feed_url = "not a URL")
  file <- withr::local_tempfile(fileext = ".opml")

  testthat::expect_snapshot(write_opml(feeds, file), error = TRUE)
})

testthat::test_that("OPML imports add and reorganize subscriptions", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  subscriptions <- data.frame(
    title = c("New feed", "R Weekly"),
    feed_url = c(
      "https://example.com/feed.xml",
      "https://rweekly.org/atom.xml"
    ),
    site_url = c("https://example.com/", "https://rweekly.org/"),
    folder = c("New folder", "Newsletters"),
    stringsAsFactors = FALSE
  )

  result <- import_opml_subscriptions(
    store,
    config$actor_id,
    subscriptions,
    refresh = FALSE
  )
  feeds <- store_list_feeds(store, config$actor_id)

  testthat::expect_identical(result$imported, 2L)
  testthat::expect_identical(result$added, 1L)
  testthat::expect_identical(result$updated, 1L)
  testthat::expect_equal(nrow(feeds), 4L)
  testthat::expect_identical(
    feeds$folder[feeds$feed_url == "https://rweekly.org/atom.xml"],
    "Newsletters"
  )

  store_ensure_reader(store, "other-reader")
  import_opml_subscriptions(
    store,
    "other-reader",
    data.frame(
      title = "My R Weekly",
      feed_url = "https://rweekly.org/atom.xml",
      site_url = "https://rweekly.org/",
      folder = "Morning",
      stringsAsFactors = FALSE
    ),
    refresh = FALSE
  )
  other_library <- store_list_feeds(store, "other-reader")
  feeds <- store_list_feeds(store, config$actor_id)
  testthat::expect_identical(other_library$folder, "Morning")
  testthat::expect_identical(other_library$title, "My R Weekly")
  testthat::expect_identical(
    feeds$folder[feeds$feed_url == "https://rweekly.org/atom.xml"],
    "Newsletters"
  )
})

testthat::test_that("OPML imports skip subscriptions Rill cannot fetch", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  subscriptions <- data.frame(
    title = "Private feed",
    feed_url = "http://127.0.0.1/feed.xml",
    site_url = NA_character_,
    folder = "Private"
  )

  result <- import_opml_subscriptions(
    store,
    config$actor_id,
    subscriptions,
    refresh = FALSE
  )

  testthat::expect_identical(result$imported, 0L)
  testthat::expect_identical(result$failed, 1L)
  testthat::expect_match(format_opml_import_status(result), "1 feed skipped")
})
