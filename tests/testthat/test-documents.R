testthat::test_that("Document disclosure uses the canonical Original Source", {
  document <- list(
    canonical_url = "https://example.com/canonical",
    source_url = "https://feeds.example.com/item"
  )

  testthat::expect_identical(
    rill_document_original_source_url(document),
    document$canonical_url
  )

  document$canonical_url <- NA_character_
  testthat::expect_identical(
    rill_document_original_source_url(document),
    document$source_url
  )
})

testthat::test_that("Document limitations describe each acquisition method", {
  expected <- c(
    sample = "This demo story ships with Rill; it isn't from a real feed.",
    feed_fallback = "This copy comes from the feed and may be an excerpt.",
    web_extraction = paste(
      "Automatic extraction can leave out or reorder parts of the original",
      "page."
    ),
    browser_capture = paste(
      "This is the page as your browser captured it. The original may have",
      "changed since, and interactive content isn't included."
    ),
    unknown = "This copy may not include everything on the original page."
  )

  actual <- vapply(
    names(expected),
    \(method) rill_document_limitations(list(acquisition_method = method)),
    character(1)
  )

  testthat::expect_identical(actual, expected)
})

testthat::test_that("Document ownership matches its acquisition method", {
  testthat::expect_error(
    new_rill_document(
      entry_id = "entry-1",
      source_url = "https://example.com/article",
      markdown = "Private copy",
      acquisition_method = "browser_capture",
      producer = "clipper"
    ),
    class = "rill_document_invalid"
  )
  testthat::expect_error(
    new_rill_document(
      entry_id = "entry-1",
      reader_id = "reader-one",
      source_url = "https://example.com/article",
      markdown = "Public copy",
      acquisition_method = "web_extraction",
      producer = "extractor"
    ),
    class = "rill_document_invalid"
  )
})
