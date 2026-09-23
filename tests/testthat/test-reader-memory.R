testthat::test_that("only explicit acceptance creates isolated Reader Memory", {
  for (backend in c("memory", "postgres")) {
    store <- local_orientation_backend_store(backend, "reader")
    store_ensure_reader(store, "other")
    access <- reader_memory_access(store, "reader")
    other <- reader_memory_access(store, "other")
    proposal <- reader_memory_propose(access, "Prioritize original research.")
    testthat::expect_length(reader_memory_list(access), 0L)
    accepted <- reader_memory_accept(access, proposal)
    testthat::expect_identical(reader_memory_accept(access, proposal), accepted)
    memory <- reader_memory_read(access, accepted$memory_id)
    testthat::expect_identical(memory$text, proposal$text)
    testthat::expect_length(memory$evidence, 0L)
    testthat::expect_length(reader_memory_list(other), 0L)
    testthat::expect_error(
      reader_memory_accept(other, proposal),
      class = "rill_memory_unavailable"
    )
    testthat::expect_error(
      reader_memory_read(other, accepted$memory_id),
      class = "rill_memory_unavailable"
    )
    testthat::expect_error(
      reader_memory_consult(other, list(memory$basis)),
      class = "rill_memory_unavailable"
    )
    stale <- reader_memory_propose(
      access,
      "A stale change.",
      memory_id = accepted$memory_id,
      expected = accepted$decision$id
    )
    correction <- reader_memory_propose(
      access,
      "Prioritize original climate research.",
      memory_id = accepted$memory_id,
      expected = accepted$decision$id
    )
    changed <- reader_memory_accept(access, correction)
    testthat::expect_error(
      reader_memory_accept(access, stale),
      class = "graft_artifact_error"
    )
    testthat::expect_error(
      reader_memory_accept(access, proposal),
      class = "graft_stale_review_error"
    )
    testthat::expect_error(
      reader_memory_propose(
        access,
        "A new memory with an expected decision.",
        expected = accepted$decision$id
      ),
      class = "rill_memory_unavailable"
    )
    testthat::expect_error(
      reader_memory_propose(
        access,
        "A revision without an expected decision.",
        memory_id = accepted$memory_id
      ),
      class = "rill_memory_unavailable"
    )
    testthat::expect_error(
      reader_memory_propose(
        access,
        "A revision with the wrong expected decision.",
        memory_id = accepted$memory_id,
        expected = "not-the-current-decision"
      ),
      class = "rill_memory_unavailable"
    )
    testthat::expect_identical(
      reader_memory_read(access, accepted$memory_id, accepted$decision$id)$text,
      proposal$text
    )
    testthat::expect_identical(
      reader_memory_read(access, accepted$memory_id)$text,
      correction$text
    )
    testthat::expect_error(
      reader_memory_consult(access, list(memory$basis)),
      class = "graft_stale_review_error"
    )
    current <- reader_memory_read(access, accepted$memory_id)
    testthat::expect_identical(
      reader_memory_consult(access, list(current$basis))[[1L]],
      current
    )
    archived <- reader_memory_archive(
      access,
      accepted$memory_id,
      changed$decision$id,
      "archive"
    )
    testthat::expect_length(reader_memory_list(access, consult = TRUE), 0L)
    testthat::expect_identical(
      reader_memory_read(access, accepted$memory_id)$text,
      correction$text
    )
    testthat::expect_error(
      reader_memory_consult(access, list(current$basis)),
      class = "graft_artifact_error"
    )
    reader_memory_restore(access, accepted$memory_id, archived$id, "restore")
    testthat::expect_length(reader_memory_list(access, consult = TRUE), 1L)
    store_disable_reader(store, "reader", "operator", "Reader disabled")
    testthat::expect_error(
      reader_memory_list(access),
      class = "rill_memory_unavailable"
    )
    testthat::expect_length(reader_memory_list(other), 0L)
  }
})

testthat::test_that("interpretations retain exact private Document passages", {
  for (backend in c("memory", "postgres")) {
    store <- local_orientation_backend_store(backend, "reader")
    store_ensure_reader(store, "other")
    doc <- capture_document(store, capture_test_payload(), "reader")
    document <- store_get_document_by_id(store, "reader", doc$document_id)
    quote <- substr(document$markdown, 1L, min(80L, nchar(document$markdown)))
    access <- reader_memory_access(store, "reader")
    other <- reader_memory_access(store, "other")
    testthat::expect_error(
      reader_memory_propose(
        other,
        "Interpretation",
        "interpretation",
        document$document_id,
        quote
      ),
      class = "rill_memory_unavailable"
    )
    testthat::expect_error(
      reader_memory_propose(
        access,
        "Interpretation",
        "interpretation",
        document$document_id,
        "a passage absent from this source"
      ),
      class = "rill_memory_unavailable"
    )
    proposal <- reader_memory_propose(
      access,
      "An explicitly qualified interpretation.",
      "interpretation",
      document$document_id,
      quote
    )
    altered <- proposal
    altered$anchor$content_hash <- "different"
    testthat::expect_error(
      reader_memory_accept(access, altered),
      class = "rill_memory_unavailable"
    )
    saved <- reader_memory_accept(access, proposal)
    retained <- reader_memory_read(access, saved$memory_id)
    testthat::expect_identical(retained$kind, "interpretation")
    testthat::expect_identical(retained$evidence[[1L]]$quote, quote)
    testthat::expect_identical(
      retained$evidence[[1L]]$document_id,
      document$document_id
    )
    testthat::expect_identical(
      retained$evidence[[1L]]$content_hash,
      document$content_hash
    )
    testthat::expect_length(reader_memory_list(other), 0L)
  }
})

testthat::test_that("bound memory access rechecks revoked host authority", {
  store <- local_orientation_backend_store("memory", "reader")
  eligible <- TRUE
  access <- reader_memory_access(store, "reader", function() eligible)
  proposal <- reader_memory_propose(access, "Prefer short answers.")
  saved <- reader_memory_accept(access, proposal)
  basis <- list(reader_memory_read(access, saved$memory_id)$basis)
  eligible <- FALSE
  testthat::expect_error(
    reader_memory_accept(access, proposal),
    class = "rill_memory_unavailable"
  )
  testthat::expect_error(
    reader_memory_read(access, saved$memory_id),
    class = "rill_memory_unavailable"
  )
  testthat::expect_error(
    reader_memory_consult(access, basis),
    class = "rill_memory_unavailable"
  )
})

testthat::test_that("the memory tool has a bound scope and rejects changed eligibility", {
  store <- local_orientation_backend_store("memory", "reader")
  access <- reader_memory_access(store, "reader")
  accepted <- reader_memory_accept(
    access,
    reader_memory_propose(access, "Prefer original research.")
  )
  record <- reader_memory_read(access, accepted$memory_id)
  tool <- reader_memory_tool(access, list(record$basis))
  testthat::expect_length(formals(tool), 0L)
  testthat::expect_identical(
    jsonlite::fromJSON(tool(), simplifyVector = FALSE)[[1L]]$text,
    record$text
  )
  reader_memory_archive(
    access,
    accepted$memory_id,
    accepted$decision$id,
    "archive"
  )
  testthat::expect_error(tool(), class = "graft_artifact_error")
})

testthat::test_that("accepted memory and the host receipt roll back together", {
  store <- local_orientation_backend_store("postgres", "reader")
  access <- reader_memory_access(store, "reader")
  proposal <- reader_memory_propose(
    access,
    "Keep this only if the receipt commits."
  )
  before <- DBI::dbGetQuery(
    store$pool,
    "SELECT count(*) AS n FROM graft_artifact_objects"
  )$n
  receipt <- reader_memory_record_event
  fail <- TRUE
  testthat::local_mocked_bindings(reader_memory_record_event = function(...) {
    if (fail) {
      reader_memory_abort("Synthetic receipt failure")
    }
    receipt(...)
  })
  testthat::expect_error(
    reader_memory_accept(access, proposal),
    class = "rill_memory_unavailable"
  )
  testthat::expect_length(reader_memory_list(access), 0L)
  testthat::expect_identical(
    DBI::dbGetQuery(
      store$pool,
      "SELECT count(*) AS n FROM graft_artifact_objects"
    )$n,
    before
  )
  fail <- FALSE
  saved <- reader_memory_accept(access, proposal)
  testthat::expect_identical(
    reader_memory_read(access, saved$memory_id)$text,
    proposal$text
  )
  reader_memory_accept(access, proposal)
  testthat::expect_equal(
    DBI::dbGetQuery(
      store$pool,
      "SELECT count(*) AS n FROM events WHERE event_type = 'memory.accept'"
    )$n,
    1
  )
})

testthat::test_that("a fresh process rebinds Reader authority and exact memory", {
  store <- local_orientation_backend_store("postgres", "reader")
  store_ensure_reader(store, "other")
  access <- reader_memory_access(store, "reader")
  saved <- reader_memory_accept(
    access,
    reader_memory_propose(access, "Persist across workers.")
  )
  record <- reader_memory_read(access, saved$memory_id)
  schema <- DBI::dbGetQuery(store$pool, "SELECT current_schema() AS name")$name
  consult <- function(reader_id, basis) {
    callr::r(
      function(path, schema, reader_id, basis) {
        if (!is.null(path)) {
          pkgload::load_all(path, quiet = TRUE, helpers = FALSE)
        }
        args <- rill:::postgres_connection_args(Sys.getenv(
          "RILL_TEST_DATABASE_URL"
        ))
        args$options <- paste0("-csearch_path=", schema)
        database_pool <- do.call(
          pool::dbPool,
          c(list(drv = RPostgres::Postgres()), args)
        )
        on.exit(pool::poolClose(database_pool), add = TRUE)
        store <- structure(
          list(mode = "postgres", pool = database_pool),
          class = "rill_store"
        )
        access <- rill:::reader_memory_access(store, reader_id)
        tryCatch(
          rill:::reader_memory_consult(access, basis),
          error = function(e) class(e)
        )
      },
      list(
        path = if (pkgload::is_dev_package("rill")) {
          normalizePath(testthat::test_path("../.."))
        } else {
          NULL
        },
        schema = schema,
        reader_id = reader_id,
        basis = basis
      )
    )
  }
  testthat::expect_identical(
    consult("reader", list(record$basis))[[1L]]$text,
    record$text
  )
  testthat::expect_contains(
    consult("other", list(record$basis)),
    "rill_memory_unavailable"
  )
  reader_memory_accept(
    access,
    reader_memory_propose(
      access,
      "A correction.",
      memory_id = saved$memory_id,
      expected = saved$decision$id
    )
  )
  testthat::expect_contains(
    consult("reader", list(record$basis)),
    "graft_stale_review_error"
  )
  testthat::expect_identical(
    reader_memory_read(access, saved$memory_id, saved$decision$id)$text,
    record$text
  )
  store_disable_reader(store, "reader", "operator", "Disabled")
  testthat::expect_contains(
    consult("reader", list(record$basis)),
    "rill_memory_unavailable"
  )
})

testthat::test_that("Deputy gets only bound read-only memory context", {
  store <- local_orientation_backend_store("memory", "reader")
  access <- reader_memory_access(store, "reader")
  saved <- reader_memory_accept(
    access,
    reader_memory_propose(access, "Prefer primary research.")
  )
  record <- reader_memory_read(access, saved$memory_id)
  agent <- rill_reader_agent(
    sample_rill_data()$documents[[1L]],
    "reader",
    "memory-test",
    chat = ellmer::chat_openai(
      credentials = function() "test-key",
      model = "gpt-5.4"
    ),
    memory_access = access,
    memory_basis = list(record$basis)
  )
  testthat::expect_setequal(
    names(rill_agent_chat_call(agent, "get_tools", list())),
    c("read_current_document", "reader_memory")
  )
  testthat::expect_match(
    rill_agent_chat_call(agent, "get_system_prompt"),
    "untrusted context",
    fixed = TRUE
  )
})


testthat::test_that("the dialog limit does not exclude accepted memory from consultation", {
  for (backend in c("memory", "postgres")) {
    store <- local_orientation_backend_store(backend, "reader")
    access <- reader_memory_access(store, "reader")
    accepted <- lapply(seq_len(101L), function(i) {
      reader_memory_accept(
        access,
        reader_memory_propose(access, paste("Accepted preference", i))
      )
    })
    oldest <- accepted[[1L]]
    displayed <- reader_memory_list(access)
    testthat::expect_length(displayed, 100L)
    testthat::expect_setequal(
      vapply(displayed, `[[`, "", "memory_id"),
      vapply(accepted[-1L], `[[`, "", "memory_id")
    )
    eligible <- reader_memory_list(access, consult = TRUE)
    testthat::expect_length(eligible, 101L)
    testthat::expect_setequal(
      vapply(eligible, `[[`, "", "memory_id"),
      vapply(accepted, `[[`, "", "memory_id")
    )
    reader_memory_archive(
      access,
      oldest$memory_id,
      oldest$decision$id,
      "archive-oldest"
    )
    eligible <- reader_memory_list(access, consult = TRUE)
    testthat::expect_length(eligible, 100L)
    testthat::expect_setequal(
      vapply(eligible, `[[`, "", "memory_id"),
      vapply(accepted[-1L], `[[`, "", "memory_id")
    )
  }
})


testthat::test_that("the memory tool removes source URL credentials without changing retained evidence", {
  store <- local_orientation_backend_store("memory", "reader")
  access <- reader_memory_access(store, "reader")
  source_url <- paste0(
    "https://reader:source-secret@example.com/story?",
    "ticket=ST-secret-grant&session=private-session#private"
  )
  saved_document <- capture_document(
    store,
    capture_test_payload(source_url = source_url),
    "reader"
  )
  saved <- reader_memory_accept(
    access,
    reader_memory_propose(
      access,
      "A qualified interpretation.",
      "interpretation",
      saved_document$document_id,
      "Source-grounded text."
    )
  )
  record <- reader_memory_read(access, saved$memory_id)
  tool <- reader_memory_tool(access, list(record$basis))
  supplied <- jsonlite::fromJSON(tool(), simplifyVector = FALSE)[[1L]]
  testthat::expect_identical(
    supplied$evidence[[1L]]$source_url,
    "https://example.com/story"
  )
  expected <- record
  expected$evidence[[1L]]$source_url <- "https://example.com/story"
  testthat::expect_identical(
    canonical_json(supplied),
    canonical_json(expected)
  )
  testthat::expect_identical(
    reader_memory_read(access, saved$memory_id)$evidence[[1L]]$source_url,
    source_url
  )
})
