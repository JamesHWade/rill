testthat::test_that("Reader admission commands require durable configuration", {
  withr::local_envvar(DATABASE_URL = "")

  testthat::expect_error(
    list_reader_admissions(),
    class = "rill_reader_admission_store_required"
  )
  testthat::expect_error(
    approve_reader_admission(
      "request-id",
      responsible_id = "operator:james"
    ),
    class = "rill_reader_admission_store_required"
  )
})

testthat::test_that("Reader admission command inputs are validated", {
  testthat::expect_error(
    list_reader_admissions("waiting"),
    class = "rill_reader_admission_status_invalid"
  )
  testthat::expect_error(
    approve_reader_admission("", responsible_id = "operator:james"),
    class = "rill_reader_admission_input_invalid"
  )
})
