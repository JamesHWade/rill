testthat::test_that("prototype ports are validated", {
  testthat::expect_identical(prototype_env$prototype_port("1"), 1L)
  testthat::expect_identical(prototype_env$prototype_port(" 7348 "), 7348L)
  testthat::expect_identical(prototype_env$prototype_port("65535"), 65535L)

  for (value in c("", "abc", "0", "65536", "1.5", "+7348")) {
    testthat::expect_error(
      prototype_env$prototype_port(value),
      class = "rill_prototype_port_error"
    )
  }
})

testthat::test_that("prototype UI stays self-contained", {
  testthat::expect_identical(
    fixed_count(prototype_html, "id=\"story_list\""),
    1L
  )
  testthat::expect_identical(
    fixed_count(prototype_html, "id=\"reader_header\""),
    1L
  )
  testthat::expect_identical(
    fixed_count(prototype_html, "id=\"reader_body\""),
    1L
  )
  testthat::expect_identical(
    fixed_count(prototype_html, "reader-agent-sidebar"),
    0L
  )
  testthat::expect_identical(
    fixed_count(prototype_html, "role=\"dialog\""),
    2L
  )
  testthat::expect_identical(fixed_count(prototype_html, "aria-modal"), 0L)

  for (variant in letters[1:4]) {
    testthat::expect_gt(
      fixed_count(prototype_html, paste0("prototype-variant-", variant)),
      0L
    )
    testthat::expect_identical(
      fixed_count(prototype_html, paste0("prototype-", variant, "-proposal")),
      1L
    )
  }

  testthat::expect_match(
    prototype_html,
    "Data Destination · OpenAI",
    fixed = TRUE
  )
  testthat::expect_match(
    prototype_head,
    "Preserved interaction prototype styles for issue #10.",
    fixed = TRUE
  )
  testthat::expect_match(
    prototype_head,
    "const variants = [",
    fixed = TRUE
  )
})

testthat::test_that("prototype sources are excluded from package builds", {
  build_ignore <- readLines(file.path(package_root, ".Rbuildignore"))
  testthat::expect_in("^prototypes$", build_ignore)
})
