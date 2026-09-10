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
