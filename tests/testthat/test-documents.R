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
    sample = "Bundled demo content cannot support real-world claims.",
    feed_fallback = paste(
      "This reading copy contains stored feed content and may be incomplete."
    ),
    web_extraction = paste(
      "Automated extraction may omit or reorder material from the",
      "Original Source."
    ),
    browser_capture = paste(
      "This browser capture reflects the Original Source at capture time",
      "and may omit unavailable or interactive content."
    ),
    unknown = paste(
      "This reading copy may not include all material from the",
      "Original Source."
    )
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
