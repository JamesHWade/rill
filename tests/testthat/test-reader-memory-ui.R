testthat::test_that("Reader Memory requires preview and explicit acceptance", {
  store <- local_orientation_backend_store("memory", "reader")
  access <- reader_memory_access(store, "reader")
  shiny::testServer(
    reader_memory_server,
    args = list(
      access = access,
      document = function() NULL
    ),
    {
      session$setInputs(
        text = "Prefer original research.",
        kind = "preference",
        selected = ""
      )
      session$setInputs(accept = 1)
      testthat::expect_length(reader_memory_list(access), 0L)
      session$setInputs(review = 1)
      testthat::expect_identical(pending()$text, "Prefer original research.")
      testthat::expect_length(reader_memory_list(access), 0L)
      session$setInputs(text = "Prefer climate research.")
      testthat::expect_null(pending())
      session$setInputs(accept = 2)
      testthat::expect_length(reader_memory_list(access), 0L)
      session$setInputs(review = 2)
      session$setInputs(accept = 3)
      testthat::expect_identical(
        reader_memory_list(access)[[1L]]$text,
        "Prefer climate research."
      )
      session$setInputs(archive = 1)
      testthat::expect_length(reader_memory_list(access, consult = TRUE), 0L)
      session$setInputs(restore = 1)
      testthat::expect_length(reader_memory_list(access, consult = TRUE), 1L)
    }
  )
})

testthat::test_that("stale memory approval reports failure without replacing newer intent", {
  store <- local_orientation_backend_store("memory", "reader")
  access <- reader_memory_access(store, "reader")
  original <- reader_memory_accept(
    access,
    reader_memory_propose(access, "Original")
  )
  shiny::testServer(
    reader_memory_server,
    args = list(
      access = access,
      document = function() NULL
    ),
    {
      session$setInputs(
        selected = original$memory_id,
        text = "Stale proposal",
        kind = "preference"
      )
      session$setInputs(review = 1)
      reader_memory_accept(
        access,
        reader_memory_propose(
          access,
          "Newer intent",
          memory_id = original$memory_id
        )
      )
      session$setInputs(accept = 1)
      testthat::expect_identical(
        reader_memory_read(access, original$memory_id)$text,
        "Newer intent"
      )
      testthat::expect_match(status(), "Review it again", fixed = TRUE)
    }
  )
})
