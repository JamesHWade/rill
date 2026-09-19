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
    reordered <- lapply(basis, function(x) x[rev(names(x))])
    testthat::expect_identical(reader_agent_for(document, reordered), first)
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

testthat::test_that("memory mode changes reject preserved runs before constructing an agent", {
  for (enabled in c(FALSE, TRUE)) {
    withr::local_envvar(
      DATABASE_URL = "",
      RILL_IDENTITY_MODE = "local",
      RILL_READER_MEMORY_ENABLED = tolower(as.character(enabled))
    )
    config <- rill_config()
    store <- rill_store(config)
    calls <- 0L
    testthat::local_mocked_bindings(rill_reader_agent = function(...) {
      calls <<- calls + 1L
      list()
    })
    old <- if (enabled) {
      list(question = "Old question")
    } else {
      list(reader_memory = list())
    }
    shiny::testServer(rill_server(config, store), {
      doc <- sample_rill_data()$documents[[1L]]
      testthat::expect_error(
        run_reader_question("Old question", doc, pinned_inputs = old),
        class = "rill_agent_memory_mode_changed"
      )
      testthat::expect_error(
        run_reader_question(
          "Old question",
          doc,
          retry_of = list(pinned_inputs = old)
        ),
        class = "rill_agent_memory_mode_changed"
      )
      testthat::expect_identical(calls, 0L)
    })
  }
})

testthat::test_that("local and external memory changes visibly start a new conversation", {
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
  messages <- character()
  testthat::local_mocked_bindings(
    rill_reader_agent = function(...) list(),
    append_reader_chat = function(response, session) {
      messages <<- c(messages, response)
      invisible(NULL)
    }
  )
  shiny::testServer(rill_server(config, store), {
    doc <- sample_rill_data()$documents[[1L]]
    reader_agent_for(doc, basis)
    session$setInputs(`memory-selected` = saved$memory_id)
    session$setInputs(`memory-archive` = 1L)
    testthat::expect_null(reader_agent())
    testthat::expect_length(messages, 0L)
    testthat::expect_identical(reader_memory_context_notice(), TRUE)
    reader_agent_for(doc, list())
    testthat::expect_length(messages, 1L)
    testthat::expect_match(messages[[1L]], "New conversation", fixed = TRUE)
    testthat::expect_match(
      messages[[1L]],
      "not passed to the new conversation",
      fixed = TRUE
    )
    reader_agent_for(doc, list())
    reader_memory_accept(
      access,
      reader_memory_propose(access, "Added in another session")
    )
    latest <- lapply(reader_memory_list(access, consult = TRUE), `[[`, "basis")
    reader_agent_for(doc, latest)
    testthat::expect_length(messages, 2L)
    reader_agent_for(doc, latest)
    testthat::expect_length(messages, 2L)
  })
})

testthat::test_that("revisions keep the retained interpretation source while another Document is selected", {
  store <- local_orientation_backend_store("memory", "reader")
  access <- reader_memory_access(store, "reader")
  original_document <- capture_document(
    store,
    capture_test_payload(title = "Original reading copy"),
    "reader"
  )
  another_document <- capture_document(
    store,
    capture_test_payload(
      capture_id = "another-capture",
      source_url = "https://example.com/another",
      canonical_url = "https://example.com/another",
      title = "Another reading copy"
    ),
    "reader"
  )
  another_document <- store_get_document_by_id(
    store,
    "reader",
    another_document$document_id
  )
  saved <- reader_memory_accept(
    access,
    reader_memory_propose(
      access,
      "Original interpretation",
      "interpretation",
      original_document$document_id,
      "Source-grounded text."
    )
  )
  shiny::testServer(
    reader_memory_server,
    args = list(access = access, document = function() another_document),
    {
      session$setInputs(selected = saved$memory_id)
      session$setInputs(
        kind = "interpretation",
        text = "Revised interpretation",
        quote = "Source-grounded text."
      )
      session$setInputs(review = 1)
      testthat::expect_identical(
        pending()$anchor$document_id,
        original_document$document_id
      )
      testthat::expect_match(
        output$preview$html,
        "Original reading copy",
        fixed = TRUE
      )
      session$setInputs(accept = 1)
      revised <- reader_memory_read(access, saved$memory_id)
      testthat::expect_identical(revised$text, "Revised interpretation")
      testthat::expect_identical(
        revised$evidence[[1L]]$document_id,
        original_document$document_id
      )
      testthat::expect_identical(
        reader_memory_read(access, saved$memory_id, saved$decision$id)$text,
        "Original interpretation"
      )
    }
  )
})

testthat::test_that("memory lookup failures cannot create an empty pinned run", {
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
    reader_memory_propose(access, "Retain this context")
  )
  basis <- list(reader_memory_read(access, saved$memory_id)$basis)
  read_memories <- reader_memory_list
  unavailable <- TRUE
  recover <- function() unavailable <<- FALSE
  constructions <- 0L
  testthat::local_mocked_bindings(
    reader_memory_list = function(...) {
      if (unavailable) {
        reader_memory_abort("Temporary lookup failure")
      }
      read_memories(...)
    },
    rill_reader_agent = function(...) {
      constructions <<- constructions + 1L
      cli::cli_abort("Synthetic setup failure", class = "test_setup_failure")
    },
    append_reader_chat = function(...) invisible(NULL)
  )
  shiny::testServer(rill_server(config, store), {
    doc <- sample_rill_data()$documents[[1L]]
    testthat::expect_error(
      run_reader_question("Question", doc, request_key = "memory-recovery"),
      class = "rill_memory_unavailable"
    )
    testthat::expect_identical(constructions, 0L)
    testthat::expect_null(store_get_agent_run_by_request_key(
      store,
      config$actor_id,
      "memory-recovery"
    ))
    recover()
    run_reader_question("Question", doc, request_key = "memory-recovery")
    run <- store_get_agent_run_by_request_key(
      store,
      config$actor_id,
      "memory-recovery"
    )
    testthat::expect_identical(run$pinned_inputs$reader_memory, basis)
    testthat::expect_identical(run$status, "failed")
    testthat::expect_identical(constructions, 1L)
  })
})
