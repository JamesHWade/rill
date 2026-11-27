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

testthat::test_that("rendered documents drop markup that browsers parse differently", {
  dirty <- c(
    "<p>Kept<!--><img src=x onerror=alert(1)>--> words</p>",
    "<noscript><!--</noscript><img src=x onerror=alert(2)>--></noscript>",
    "<textarea><!--</textarea><img src=x onerror=alert(3)>--></textarea>",
    "<title></title><img src=x onerror=alert(4)></title>",
    paste0(
      "<iframe src='https://www.youtube.com/embed/18PIeJoxYtc'>",
      "<p title='</iframe><img src=x onerror=alert(5)>'>fallback</p>",
      "</iframe>"
    ),
    "<p>Plain <?php echo 1; ?> text</p>"
  )

  clean <- vapply(dirty, sanitize_rendered_html, character(1))

  testthat::expect_no_match(clean, "onerror|<!--|<\\?", ignore.case = TRUE)
  testthat::expect_match(clean[[1L]], "Kept", fixed = TRUE)
  testthat::expect_match(clean[[5L]], "youtube-nocookie.com", fixed = TRUE)
  testthat::expect_no_match(clean[[5L]], "fallback", fixed = TRUE)
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
