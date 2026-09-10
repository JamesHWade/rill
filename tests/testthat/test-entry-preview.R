testthat::test_that("source image selection resolves URLs and excludes unsafe or decorative images", {
  preview <- entry_preview_image(
    '<img src="/pixel.gif" width="1" height="1"><img src="/river.jpg" width="800" height="300" alt="A river">',
    "https://example.org/story"
  )
  testthat::expect_identical(
    preview,
    list(url = "https://example.org/river.jpg", alt = "A river")
  )
  for (url in c(
    "javascript:alert(1)",
    "http://127.0.0.1/private",
    "https://user:pass@example.org/photo.jpg"
  )) {
    testthat::expect_identical(entry_preview_url(url), NA_character_)
  }
  testthat::expect_identical(
    entry_preview_image('<img src="/logo.png">', "https://example.org")$url,
    NA_character_
  )
  testthat::expect_identical(
    entry_preview_image(NA_character_, "https://example.org")$url,
    NA_character_
  )
})

testthat::test_that("feed media images take precedence over content images", {
  item <- xml2::read_xml(
    '<item xmlns:media="http://search.yahoo.com/mrss/"><media:content type="image/jpeg" url="https://example.org/media.jpg" /></item>'
  )
  testthat::expect_identical(
    entry_preview_image(
      '<img src="/body.jpg">',
      "https://example.org/story",
      item
    )$url,
    "https://example.org/media.jpg"
  )
})

testthat::test_that("preview metadata persists without loading feed content into queues", {
  for (backend in c("memory", "postgres")) {
    store <- local_orientation_backend_store(backend, "preview-reader")
    entries <- sample_rill_data()$entries[1L, ]
    entries$preview_image_url <- "https://example.org/landscape.jpg"
    entries$preview_image_alt <- "Woodland"
    store_upsert_entries(store, entries)
    queue <- store_list_entries(
      store,
      "preview-reader",
      view = "all",
      include_content = FALSE
    )
    entry <- queue[queue$entry_id == entries$entry_id, ]
    testthat::expect_identical(
      entry$preview_image_url,
      entries$preview_image_url
    )
    testthat::expect_identical(entry$preview_image_alt, "Woodland")
    testthat::expect_null(entry$feed_content)
  }
})

testthat::test_that("legacy entry updates preserve field identities", {
  store <- rill_store(list(demo_mode = TRUE, actor_id = "legacy-reader"))
  entry <- sample_rill_data()$entries[1L, ]
  entry$preview_image_url <- NULL
  entry$preview_image_alt <- NULL
  entry$title <- "Updated headline"
  store_upsert_entries(store, entry)
  actual <- store$memory$entries[1L, ]
  testthat::expect_identical(actual$title, entry$title)
  testthat::expect_identical(actual$published_at, entry$published_at)
  testthat::expect_identical(actual$content_hash, entry$content_hash)
  testthat::expect_identical(actual$preview_image_url, NA_character_)
})
