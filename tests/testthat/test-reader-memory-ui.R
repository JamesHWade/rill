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

testthat::test_that("committed acceptance survives an unavailable refresh", {
  store <- local_orientation_backend_store("memory", "reader")
  allowed <- TRUE
  access <- reader_memory_access(store, "reader", function() allowed)
  changed_count <- 0L
  shiny::testServer(
    reader_memory_server,
    args = list(
      access = access,
      document = function() NULL,
      changed = function() {
        changed_count <<- changed_count + 1L
        allowed <<- FALSE
      }
    ),
    {
      session$setInputs(
        text = "A retained preference",
        kind = "preference",
        selected = ""
      )
      session$setInputs(review = 1)
      session$setInputs(accept = 1)
      testthat::expect_match(status(), "action was saved", fixed = TRUE)
      testthat::expect_null(shown())
      testthat::expect_null(pending())
      testthat::expect_identical(changed_count, 1L)
      testthat::expect_length(
        reader_memory_list(reader_memory_access(store, "reader")),
        1L
      )
    }
  )
})

testthat::test_that("cached agents recheck memory before returning for a retry", {
  withr::local_envvar(
    DATABASE_URL = "",
    RILL_IDENTITY_MODE = "local",
    RILL_READER_MEMORY_ENABLED = "true"
  )
  config <- rill_config()
  store <- rill_store(config)
  access <- reader_memory_access(store, config$actor_id)
  saved <- reader_memory_accept(
    access,
    reader_memory_propose(access, "An accepted preference")
  )
  basis <- list(reader_memory_read(access, saved$memory_id)$basis)
  constructions <- 0L
  testthat::local_mocked_bindings(rill_reader_agent = function(...) {
    constructions <<- constructions + 1L
    list(instance = constructions)
  })
  shiny::testServer(rill_server(config, store), {
    document <- sample_rill_data()$documents[[1L]]
    first <- reader_agent_for(document, basis)
    testthat::expect_identical(reader_agent_for(document, basis), first)
    testthat::expect_identical(constructions, 1L)
    reader_memory_archive(
      access,
      saved$memory_id,
      saved$decision$id,
      "another-session"
    )
    testthat::expect_error(
      reader_agent_for(document, basis),
      class = "graft_artifact_error"
    )
    testthat::expect_identical(constructions, 1L)
  })
})
