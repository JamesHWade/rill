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
    normalize_opml_subscriptions(feeds)[c(
      "title",
      "feed_url",
      "site_url",
      "folder"
    )]
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
    feeds$groups[[which(feeds$feed_url == "https://rweekly.org/atom.xml")]],
    c("Community", "Newsletters")
  )
  testthat::expect_identical(
    feeds$title[feeds$feed_url == "https://example.com/feed.xml"],
    "New feed"
  )
  testthat::expect_identical(
    feeds$source_title[feeds$feed_url == "https://example.com/feed.xml"],
    "https://example.com/feed.xml"
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
    feeds$groups[[which(feeds$feed_url == "https://rweekly.org/atom.xml")]],
    c("Community", "Newsletters")
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

testthat::test_that("failed OPML refreshes preserve shared source titles", {
  withr::local_envvar(DATABASE_URL = "")
  config <- rill_config()
  store <- rill_store(config)
  feed <- store_list_feeds(store, config$actor_id)[1L, , drop = FALSE]
  source_title <- feed$source_title[[1L]]
  store_rename_feed(
    store,
    config$actor_id,
    feed$feed_id[[1L]],
    "Private OPML label"
  )
  testthat::local_mocked_bindings(
    refresh_feed = function(...) cli::cli_abort("Feed unavailable.")
  )

  result <- import_opml_subscriptions(
    store,
    config$actor_id,
    data.frame(
      title = "Private OPML label",
      feed_url = feed$feed_url,
      site_url = feed$site_url,
      folder = "Research",
      stringsAsFactors = FALSE
    )
  )
  shared_feed <- store_find_feed_by_url(store, feed$feed_url[[1L]])
  reader_feed <- store_list_feeds(store, config$actor_id)
  reader_feed <- reader_feed[
    reader_feed$feed_id == feed$feed_id[[1L]],
    ,
    drop = FALSE
  ]

  testthat::expect_identical(result$refresh_failed, 1L)
  testthat::expect_identical(shared_feed$title, source_title)
  testthat::expect_identical(reader_feed$title, "Private OPML label")
})

testthat::test_that("OPML preserves overlapping and empty Groups with exact names", {
  feeds <- data.frame(
    title = c("Shared", "Loose"),
    feed_url = c("https://example.com/feed", "https://example.org/feed")
  )
  feeds$groups <- list(
    c("Research / Methods", "R & <data>", "Unsorted"),
    character()
  )
  file <- withr::local_tempfile(fileext = ".opml")
  write_opml(feeds, file, groups = c("Empty", "Research / Methods"))
  result <- read_opml(file)
  testthat::expect_equal(nrow(result), 2L)
  testthat::expect_setequal(
    result$groups[[match("Shared", result$title)]],
    feeds$groups[[1]]
  )
  testthat::expect_length(result$groups[[match("Loose", result$title)]], 0L)
  testthat::expect_contains(attr(result, "group_catalog"), "Empty")
  testthat::expect_length(
    xml2::xml_find_all(xml2::read_xml(file), "//outline[@xmlUrl]"),
    4L
  )
  store <- rill_store(list(demo_mode = TRUE, actor_id = "reader"))
  summary <- import_opml_subscriptions(store, "reader", result, refresh = FALSE)
  testthat::expect_equal(summary$imported, 2L)
  testthat::expect_contains(store_list_groups(store, "reader")$name, "Empty")
  imported <- store_list_feeds(store, "reader")
  testthat::expect_setequal(
    imported$groups[[match("https://example.com/feed", imported$feed_url)]],
    feeds$groups[[1]]
  )
})

testthat::test_that("repeated standard OPML outlines combine memberships", {
  file <- withr::local_tempfile(fileext = ".opml")
  writeLines(
    '<opml version="2.0"><body><outline text="One"><outline text="A" xmlUrl="https://example.com/feed"/></outline><outline text="Two"><outline text="A" xmlUrl="https://example.com/feed"/></outline></body></opml>',
    file
  )
  result <- read_opml(file)
  testthat::expect_equal(nrow(result), 1L)
  testthat::expect_setequal(result$groups[[1]], c("One", "Two"))
})

testthat::test_that("an empty Library preserves its Group catalog through OPML", {
  file <- withr::local_tempfile(fileext = ".opml")
  write_opml(
    data.frame(feed_url = character()),
    file,
    groups = c("Empty", "Later")
  )
  result <- read_opml(file)
  testthat::expect_equal(nrow(result), 0L)
  testthat::expect_setequal(attr(result, "group_catalog"), c("Empty", "Later"))
})

testthat::test_that("overlapping OPML exports count unique subscriptions on import", {
  file <- withr::local_tempfile(fileext = ".opml")
  feeds <- data.frame(
    title = paste("Feed", seq_len(5001L)),
    feed_url = paste0("https://example.com/feed/", seq_len(5001L))
  )
  feeds$groups <- rep(list(c("One", "Two")), nrow(feeds))
  write_opml(feeds, file)
  testthat::expect_length(
    xml2::xml_find_all(xml2::read_xml(file), "//outline[@xmlUrl]"),
    10002L
  )
  result <- read_opml(file)
  testthat::expect_equal(nrow(result), 5001L)
  testthat::expect_setequal(result$feed_url, feeds$feed_url)
  testthat::expect_identical(result$groups, rep(list(c("One", "Two")), 5001L))
})

testthat::test_that("OPML still limits distinct subscriptions", {
  file <- withr::local_tempfile(fileext = ".opml")
  writeLines(
    c(
      '<opml version="2.0"><body>',
      paste0(
        '<outline xmlUrl="https://example.com/feed/',
        seq_len(10001L),
        '"/>'
      ),
      '</body></opml>'
    ),
    file
  )
  testthat::expect_error(read_opml(file), class = "rill_error_opml")
})
