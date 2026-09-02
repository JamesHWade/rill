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
