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
