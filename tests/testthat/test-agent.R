testthat::test_that("the reader Agent receives one pinned source Document", {
  document <- sample_rill_data()$documents[[1]]

  source_tool <- rill_document_tool(document)
  supplied <- source_tool()

  testthat::expect_identical(attr(source_tool, "name"), "read_current_document")
  testthat::expect_identical(
    attr(source_tool, "annotations"),
    ellmer::tool_annotations(
      title = "Read the selected Rill Document",
      read_only_hint = TRUE,
      open_world_hint = FALSE,
      idempotent_hint = TRUE,
      destructive_hint = FALSE
    )
  )
  testthat::expect_identical(supplied$document_id, document$document_id)
  testthat::expect_identical(supplied$entry_id, document$entry_id)
  testthat::expect_identical(supplied$source_url, document$source_url)
  testthat::expect_identical(supplied$markdown, document$markdown)
  testthat::expect_identical(supplied$producer, document$producer)
  testthat::expect_identical(supplied$provenance, document$provenance)
})

testthat::test_that("the reader Agent is source-first and tightly bounded", {
  prompt <- rill_agent_system_prompt()
  limits <- rill_agent_usage_limits()
  permissions <- rill_agent_permissions()
  source_annotations <- ellmer::tool_annotations(
    read_only_hint = TRUE,
    open_world_hint = FALSE,
    destructive_hint = FALSE
  )

  allowed <- permissions$check(
    "read_current_document",
    list(),
    context = list(tool_annotations = source_annotations)
  )
  web_denied <- permissions$check("web_search", list(), context = list())
  code_denied <- permissions$check("run_r_code", list(), context = list())

  testthat::expect_match(prompt, "immutable Document", fixed = TRUE)
  testthat::expect_match(prompt, "Source Evidence", fixed = TRUE)
  testthat::expect_match(prompt, "Interpretation", fixed = TRUE)
  testthat::expect_match(prompt, "Unsupported Gap", fixed = TRUE)
  testthat::expect_identical(allowed$decision, "allow")
  testthat::expect_identical(web_denied$decision, "deny")
  testthat::expect_identical(code_denied$decision, "deny")
  testthat::expect_identical(limits$max_requests, 8L)
  testthat::expect_identical(limits$max_tool_calls, 16L)
  testthat::expect_identical(limits$max_total_tokens, 128000L)
  testthat::expect_identical(limits$max_output_tokens, 8000L)
  testthat::expect_identical(limits$max_cost_usd, 2)
  testthat::expect_identical(rill_agent_wall_time_seconds(), 5 * 60)
})

testthat::test_that("the reader Agent stream exposes its latest partial text", {
  stream <- coro::async_generator(function() {
    coro::yield("Source ")
    coro::yield("evidence.")
  })()
  partials <- character()
  tracked <- track_reader_agent_stream(
    stream,
    function(partial) partials <<- c(partials, partial)
  )
  collected <- NULL
  error <- NULL

  coro::async_collect(tracked) |>
    promises::then(function(value) collected <<- value) |>
    promises::catch(function(condition) error <<- condition)
  timeout <- Sys.time() + 2
  while (is.null(collected) && is.null(error) && Sys.time() < timeout) {
    later::run_now(0.01)
  }

  testthat::expect_null(error)
  testthat::expect_identical(collected, list("Source ", "evidence."))
  testthat::expect_identical(partials, c("Source ", "Source evidence."))
})

testthat::test_that("a Deputy Agent is pinned to the selected Document", {
  document <- sample_rill_data()$documents[[1]]
  chat <- ellmer::chat_openai(
    credentials = function() "test-key",
    model = "gpt-5.4"
  )

  agent <- rill_reader_agent(
    document = document,
    reader_id = "reader-1",
    session_id = "rill-session-1",
    chat = chat
  )

  testthat::expect_r6_class(agent, "Agent")
  testthat::expect_identical(
    names(agent$get_tools()),
    "read_current_document"
  )
  testthat::expect_identical(
    agent$get_system_prompt(),
    rill_agent_system_prompt()
  )
  testthat::expect_identical(
    agent$run_context,
    list(
      document_id = document$document_id,
      entry_id = document$entry_id,
      product = "rill",
      reader_id = "reader-1"
    )
  )
})
