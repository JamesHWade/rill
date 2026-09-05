testthat::test_that("extraction traces never capture raw exception content", {
  testthat::skip_if_not_installed("otelsdk")
  testthat::local_mocked_bindings(
    fetch_defuddled_markdown_hosted = function(...) {
      cli::cli_abort(
        "https://private.example/article?key=private-token",
        class = "rill_test_extraction_failure"
      )
    }
  )
  record <- otelsdk::with_otel_record(
    testthat::expect_error(
      fetch_defuddled_markdown("https://example.com", list()),
      class = "rill_test_extraction_failure"
    ),
    what = "traces"
  )

  testthat::expect_length(record$traces, 1L)
  testthat::expect_identical(record$traces[[1]]$status, "error")
  testthat::expect_no_match(
    paste(unlist(record$traces[[1]]$events), collapse = "\n"),
    "private.example|private-token"
  )
})

testthat::test_that("safe spans preserve results and trace parentage", {
  testthat::skip_if_not_installed("otelsdk")
  record <- otelsdk::with_otel_record(
    {
      telemetry_span("parent", {
        telemetry_span("child", invisible(42L))
      })
    },
    what = "traces"
  )

  testthat::expect_identical(record$value, 42L)
  testthat::expect_identical(
    record$traces$child$trace_id,
    record$traces$parent$trace_id
  )
  testthat::expect_identical(
    record$traces$child$parent,
    record$traces$parent$span_id
  )
  testthat::expect_identical(record$traces$child$status, "ok")
  testthat::expect_length(record$traces$child$events, 0L)
})

testthat::test_that("only the current validated browser acknowledgement ends an opening", {
  testthat::skip_if_not_installed("otelsdk")
  first <- strrep("a", 32)
  second <- strrep("b", 32)
  record <- otelsdk::with_otel_record(
    later::with_temp_loop({
      opening <- reading_telemetry(TRUE)
      opening$begin(first, "story_list")
      opening$begin(second, "orientation")
      opening$complete(first, 100)
      opening$complete(second, Inf)
      opening$complete(second, "private-token")
      opening$complete(second, 120001)
      opening$complete(second, 450)
      opening$complete(second, 451)
      opening$finish("disconnected")
    }),
    what = "traces"
  )

  testthat::expect_length(record$traces, 2L)
  testthat::expect_identical(
    record$traces[[1]]$attributes$reading.outcome,
    "superseded"
  )
  testthat::expect_identical(
    record$traces[[2]]$attributes$reading.outcome,
    "visible"
  )
  testthat::expect_identical(
    record$traces[[2]]$attributes$reading.first_text_ms,
    450
  )
  testthat::expect_identical(record$traces[[2]]$status, "ok")
  testthat::expect_no_match(
    paste(unlist(lapply(record$traces, `[[`, "attributes")), collapse = "\n"),
    "private-token|aaaaaaaa|bbbbbbbb"
  )
})

testthat::test_that("disabled or malformed browser timing never creates a trace", {
  testthat::skip_if_not_installed("otelsdk")
  record <- otelsdk::with_otel_record(
    later::with_temp_loop({
      opening <- reading_telemetry(FALSE)
      opening$begin(strrep("a", 32), "story_list")
      opening <- reading_telemetry(TRUE)
      for (id in list(NULL, NA_character_, 1, c("a", "b"), "private-token")) {
        opening$begin(id, "story_list")
      }
      opening$finish("disconnected")
    }),
    what = "traces"
  )
  testthat::expect_length(record$traces, 0L)
})

testthat::test_that("cold and cached reading copies have distinct store timing", {
  testthat::skip_if_not_installed("otelsdk")
  store <- preparation_test_store()
  entry <- as.list(store$memory$entries[1, , drop = FALSE])
  first <- otelsdk::with_otel_record(
    reading_document(store, "reader", entry),
    what = "traces"
  )
  second <- otelsdk::with_otel_record(
    reading_document(store, "reader", entry),
    what = "traces"
  )

  testthat::expect_identical(first$value, second$value)
  testthat::expect_contains(names(first$traces), "store.reading_copy.save")
  testthat::expect_identical(
    first$traces$article.reading_copy$attributes$copy.cached,
    FALSE
  )
  testthat::expect_identical(
    second$traces$article.reading_copy$attributes$copy.cached,
    TRUE
  )
  testthat::expect_identical(
    names(second$traces),
    c("store.reading_copy.lookup", "article.reading_copy")
  )
})
testthat::test_that("HTTP traces report real retries and 403s without private content", {
  testthat::skip_if_not_installed("otelsdk")
  endpoint <- local_telemetry_http()
  record <- otelsdk::with_otel_record(
    fetch_defuddled_markdown_hosted(
      "https://private.example/article",
      list(
        defuddle_base_url = paste0(endpoint, "/retry"),
        defuddle_api_key = "private-token"
      )
    ),
    what = "traces"
  )
  testthat::expect_identical(record$value, "# Article")
  span <- record$traces$article.extract.http
  testthat::expect_equal(span$attributes$http.response.status_code, 200)
  testthat::expect_equal(span$attributes$http.request.resend_count, 1)
  failure <- otelsdk::with_otel_record(
    testthat::expect_error(
      fetch_defuddled_markdown_hosted(
        "https://private.example/article",
        list(
          defuddle_base_url = paste0(endpoint, "/forbidden"),
          defuddle_api_key = "private-token"
        )
      ),
      class = "httr2_http_403"
    ),
    what = "traces"
  )
  span <- failure$traces$article.extract.http
  testthat::expect_identical(span$status, "error")
  testthat::expect_equal(span$attributes$http.response.status_code, 403)
  testthat::expect_equal(span$attributes$http.request.resend_count, 0)
  testthat::expect_no_match(
    paste(unlist(list(span$attributes, span$events)), collapse = "\n"),
    "private.example|private-token|private-response-body"
  )
})

testthat::test_that("a real worker exports extraction on its parent preparation trace", {
  testthat::skip_if_not_installed("otelsdk")
  directory <- withr::local_tempdir()
  path <- file.path(directory, "traces.jsonl")
  withr::local_envvar(c(
    OTEL_TRACES_EXPORTER = "otlp/file",
    OTEL_LOGS_EXPORTER = "none",
    OTEL_R_EMIT_SCOPES = "rill",
    OTEL_EXPORTER_OTLP_TRACES_FILE = path,
    OTEL_EXPORTER_OTLP_TRACES_FILE_ALIAS = file.path(directory, "alias.jsonl")
  ))
  store <- preparation_test_store()
  entry <- as.list(store$memory$entries[1, , drop = FALSE])
  record <- otelsdk::with_otel_record(
    {
      job <- start_article_preparation(
        store,
        list(
          defuddle_backend = "local",
          defuddle_command = "rill-no-such-extractor"
        ),
        "reader",
        entry$entry_id
      )
      withr::defer(close_article_preparation(job))
      job$process$wait(30000)
      suppressMessages(poll_article_preparation(
        job,
        store,
        list(defuddle_backend = "local")
      ))
    },
    what = "traces"
  )

  testthat::expect_identical(record$value$failure$code, "extractor_missing")
  spans <- read_telemetry_spans(path)
  worker <- spans$article.worker.extract
  testthat::expect_identical(
    worker$traceId,
    record$traces$article.prepare$trace_id
  )
  testthat::expect_identical(
    worker$parentSpanId,
    record$traces$article.worker.launch$span_id
  )
  testthat::expect_identical(spans$article.extract$parentSpanId, worker$spanId)
  testthat::expect_identical(record$traces$article.prepare$status, "error")
  testthat::expect_no_match(
    paste(readLines(path), collapse = "\n"),
    "example.com|rill-no-such-extractor|exception.stacktrace"
  )
})
testthat::test_that("browser timing follows replacement article nodes exactly once", {
  node <- Sys.which("node")
  testthat::skip_if(
    !nzchar(node),
    "Node.js is required for browser logic tests"
  )
  log <- withr::local_tempfile()
  status <- system2(
    node,
    shQuote(c(
      testthat::test_path("fixtures", "article-visible.cjs"),
      rill_package_file("app", "www", "app.js")
    )),
    stdout = log,
    stderr = log
  )
  testthat::expect_identical(
    status,
    0L,
    info = paste(readLines(log), collapse = "\n")
  )
})
testthat::test_that("safe spans end once on success and failure", {
  testthat::skip_if_not_installed("otelsdk")
  ends <- local_telemetry_ends()
  record <- otelsdk::with_otel_record(
    {
      telemetry_span("success", invisible(42L))
      testthat::expect_error(
        telemetry_span(
          "failure",
          cli::cli_abort("private-token", class = "rill_test_failure")
        ),
        class = "rill_test_failure"
      )
    },
    what = "traces"
  )
  testthat::expect_identical(
    vapply(ends(), `[[`, character(1), "status"),
    c("ok", "error")
  )
  testthat::expect_identical(record$traces$failure$status, "error")
})

testthat::test_that("preparation spans end once across failure completion and cancellation", {
  testthat::skip_if_not_installed("otelsdk")
  exercise <- function(outcome) {
    ends <- local_telemetry_ends()
    store <- preparation_test_store()
    entry <- as.list(store$memory$entries[1, , drop = FALSE])
    alive <- outcome %in% c("cancelled", "timeout")
    testthat::local_mocked_bindings(
      launch_article_preparation = function(...) {
        if (outcome == "launch_failed") {
          cli::cli_abort("private-token", class = "rill_test_launch")
        }
        list(
          is_alive = function() alive,
          kill = function() {
            alive <<- FALSE
          },
          get_result = function() {
            if (outcome == "timeout") {
              cli::cli_abort("private-token", class = "rill_test_timeout")
            }
            list(document = document_fallback(entry))
          }
        )
      }
    )
    record <- otelsdk::with_otel_record(
      {
        testthat::capture_messages({
          job <- start_article_preparation(
            store,
            list(),
            "reader",
            entry$entry_id
          )
          if (outcome != "launch_failed") {
            if (outcome != "cancelled") {
              poll_article_preparation(job, store, list(), timeout = 0)
            }
            close_article_preparation(job)
            close_article_preparation(job)
          }
        })
      },
      what = "traces"
    )
    contexts <- lapply(ends(), `[[`, "context")
    testthat::expect_identical(anyDuplicated(contexts), 0L, info = outcome)
    span <- record$traces$article.prepare
    testthat::expect_identical(
      span$status,
      switch(outcome, ready = "ok", cancelled = "unset", "error")
    )
    if (outcome != "launch_failed") {
      testthat::expect_identical(
        span$attributes$preparation.outcome,
        if (outcome == "timeout") "failed" else outcome
      )
    }
  }
  for (outcome in c("launch_failed", "ready", "cancelled", "timeout")) {
    exercise(outcome)
  }
})
