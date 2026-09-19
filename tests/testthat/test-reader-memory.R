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
      memory_id = accepted$memory_id
    )
    correction <- reader_memory_propose(
      access,
      "Prioritize original climate research.",
      memory_id = accepted$memory_id
    )
    changed <- reader_memory_accept(access, correction)
    testthat::expect_error(
      reader_memory_accept(access, stale),
      class = "graft_artifact_error"
    )
    testthat::expect_identical(reader_memory_accept(access, proposal), accepted)
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
      class = "graft_artifact_error"
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
  testthat::expect_identical(tool()[[1L]]$text, record$text)
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
    reader_memory_propose(access, "A correction.", memory_id = saved$memory_id)
  )
  testthat::expect_contains(
    consult("reader", list(record$basis)),
    "graft_artifact_error"
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
