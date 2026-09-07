testthat::test_that("RSS items become normalized entries", {
  rss <- paste0(
    "<?xml version='1.0'?><rss version='2.0'><channel>",
    "<title>Example RSS</title><link>https://example.com</link>",
    "<item><guid>post-1</guid><title>First post</title>",
    "<link>https://example.com/first</link>",
    "<description><![CDATA[<p>Hello <strong>reader</strong>.</p>]]></description>",
    "<pubDate>Sun, 30 Aug 2026 12:00:00 GMT</pubDate></item>",
    "</channel></rss>"
  )

  result <- parse_feed_document(rss, "https://example.com/feed.xml")

  testthat::expect_equal(result$feed$title, "Example RSS")
  testthat::expect_equal(nrow(result$entries), 1L)
  testthat::expect_equal(result$entries$external_id, "post-1")
  testthat::expect_match(result$entries$summary, "Hello reader")
})

testthat::test_that("Atom links and authors are recognized", {
  atom <- paste0(
    "<?xml version='1.0'?><feed xmlns='http://www.w3.org/2005/Atom'>",
    "<title>Example Atom</title><link rel='alternate' href='https://example.org'/>",
    "<entry><id>tag:example.org,2026:1</id><title>An atom entry</title>",
    "<link rel='alternate' href='/post'/><author><name>Ada</name></author>",
    "<updated>2026-08-30T12:00:00Z</updated><summary>Useful notes</summary>",
    "</entry></feed>"
  )

  result <- parse_feed_document(atom, "https://example.org/atom.xml")

  testthat::expect_equal(result$feed$title, "Example Atom")
  testthat::expect_equal(result$entries$url, "https://example.org/post")
  testthat::expect_equal(result$entries$author, "Ada")
})

testthat::test_that("RSS 1.0 preserves channel metadata and sibling items", {
  rdf <- paste0(
    '<r:RDF xmlns:r="http://www.w3.org/1999/02/22-rdf-syntax-ns#" ',
    'xmlns="http://purl.org/rss/1.0/" ',
    'xmlns:dc="http://purl.org/dc/elements/1.1/" ',
    'xmlns:content="http://purl.org/rss/1.0/modules/content/">',
    '<channel r:about="https://example.org/feed">',
    '<title>Research</title><link>https://example.org</link>',
    '</channel><item r:about="urn:article:one"><title>First</title>',
    '<link>/one</link><dc:creator>Ada</dc:creator>',
    '<dc:date>2026-09-06T12:00:00Z</dc:date>',
    '<content:encoded><![CDATA[<p>Source text.</p>]]></content:encoded>',
    '</item><item r:about="urn:article:two"><title>Second</title>',
    '<link>/two</link></item></r:RDF>'
  )

  result <- parse_feed_document(rdf, "https://example.org/feed")

  testthat::expect_identical(result$feed$title, "Research")
  testthat::expect_identical(result$feed$site_url, "https://example.org")
  testthat::expect_identical(result$entries$title, c("First", "Second"))
  testthat::expect_identical(
    result$entries$url,
    c("https://example.org/one", "https://example.org/two")
  )
  testthat::expect_identical(
    result$entries$external_id,
    c("urn:article:one", "urn:article:two")
  )
  testthat::expect_identical(result$entries$author, c("Ada", NA_character_))
  testthat::expect_identical(
    result$entries$published_at[[1L]],
    "2026-09-06 12:00:00 UTC"
  )
  testthat::expect_identical(
    result$entries$feed_content[[1L]],
    "<p>Source text.</p>"
  )
})

testthat::test_that("empty feeds refresh metadata without losing saved entries", {
  documents <- c(
    '<rss><channel><title>Empty RSS</title><link>/</link></channel></rss>',
    '<feed xmlns="http://www.w3.org/2005/Atom"><title>Empty Atom</title><link href="/"/></feed>',
    '<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"><channel><title>Empty RDF</title><link>/</link></channel></rdf:RDF>',
    '<rss><channel><title>No usable links</title><link>/</link><item><title>No link</title></item></channel></rss>'
  )
  titles <- c("Empty RSS", "Empty Atom", "Empty RDF", "No usable links")
  store <- rill_store(list(demo_mode = TRUE, actor_id = "reader"))
  feed <- as.list(store$memory$feeds[1L, , drop = FALSE])
  original_entries <- store$memory$entries
  testthat::local_mocked_bindings(fetch_feed = function(...) parsed)

  for (index in seq_along(documents)) {
    parsed <- parse_feed_document(
      documents[[index]],
      "https://example.org/redirected-feed",
      headers = list(etag = "updated")
    )
    testthat::expect_identical(parsed$entries, empty_entries())
    refreshed <- refresh_feed(store, feed)
    saved <- store_list_feeds(store, "reader")
    saved <- saved[saved$feed_id == feed$feed_id, , drop = FALSE]

    testthat::expect_identical(refreshed$added, 0L)
    testthat::expect_identical(refreshed$feed_id, feed$feed_id)
    testthat::expect_identical(saved$title, titles[[index]])
    testthat::expect_identical(saved$site_url, "https://example.org/")
    testthat::expect_identical(saved$etag, "updated")
    testthat::expect_identical(store$memory$entries, original_entries)
  }
})

testthat::test_that("unrelated XML is not accepted as an empty feed", {
  testthat::expect_error(
    parse_feed_document(
      "<error><message>Unavailable</message></error>",
      "https://example.org/feed"
    ),
    class = "rill_feed_unsupported_document"
  )
  parsed <- parse_feed_document(
    "<rss><channel/></rss>",
    "https://example.org/feed"
  )
  testthat::expect_identical(parsed$feed$title, "https://example.org/feed")
})

testthat::test_that("local network feed URLs are rejected", {
  testthat::expect_snapshot(
    validate_public_http_url("http://127.0.0.1/feed"),
    error = TRUE
  )
  testthat::expect_snapshot(
    validate_public_http_url("http://192.168.1.2/rss"),
    error = TRUE
  )
  testthat::expect_equal(
    validate_public_http_url("https://example.com/feed"),
    "https://example.com/feed"
  )
})

testthat::test_that("feed URLs require an HTTP scheme", {
  testthat::expect_snapshot(
    validate_public_http_url("example.com/feed"),
    error = TRUE
  )
})

testthat::test_that("re-adding a Feed restores its saved folder", {
  store <- rill_store(list(demo_mode = TRUE, actor_id = "reader"))
  feed <- as.list(store$memory$feeds[1L, , drop = FALSE])
  store_move_feed(store, "reader", feed$feed_id, "Research")
  store_unsubscribe_feed(store, "reader", feed$feed_id)

  testthat::local_mocked_bindings(
    fetch_feed = function(url, folder) {
      list(
        feed = feed,
        entries = empty_entries(),
        not_modified = FALSE
      )
    }
  )

  ingest_feed_url(store, "reader", feed$feed_url)

  restored <- store_list_feeds(store, "reader")
  restored <- restored[restored$feed_id == feed$feed_id, , drop = FALSE]
  testthat::expect_identical(restored$folder, "Research")
})

testthat::test_that("refresh counts new entries without counting updates again", {
  store <- rill_store(list(demo_mode = TRUE, actor_id = "reader"))
  xml <- paste0(
    '<rss version="2.0"><channel><title>Example</title>',
    '<link>https://example.com</link><item><guid>one</guid>',
    '<title>First</title><link>https://example.com/one</link>',
    '</item></channel></rss>'
  )
  parsed <- parse_feed_document(xml, "https://example.com/rss")
  parsed$not_modified <- FALSE
  testthat::local_mocked_bindings(fetch_feed = function(...) parsed)

  first <- ingest_feed_url(store, "reader", "https://example.com/rss")
  testthat::expect_identical(first$added, 1L)
  parsed$entries$title <- "Updated title"
  second <- refresh_feed(store, first$feed)
  testthat::expect_identical(second$added, 0L)
  entry <- store$memory$entries[
    store$memory$entries$feed_id == first$feed$feed_id,
    ,
    drop = FALSE
  ]
  testthat::expect_identical(entry$title, "Updated title")
})
