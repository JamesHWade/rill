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
  testthat::expect_identical(supplied$content_hash, document$content_hash)
  testthat::expect_identical(supplied$record_hash, document$record_hash)
  testthat::expect_identical(supplied$source_url, document$source_url)
  testthat::expect_identical(supplied$markdown, document$markdown)
  testthat::expect_identical(supplied$producer, document$producer)
  testthat::expect_identical(
    supplied$provenance,
    rill_agent_provenance_summary(document)
  )
})

testthat::test_that("Agent destinations have stable consent identities", {
  openai <- rill_agent_data_destination_details(
    "openai/gpt-5",
    policy_url = "https://provider.example/privacy"
  )
  upgraded <- rill_agent_data_destination_details(
    "openai/gpt-6",
    policy_url = "https://provider.example/privacy"
  )
  changed_policy <- rill_agent_data_destination_details(
    "openai/gpt-5",
    policy_url = "https://provider.example/revised-privacy"
  )
  local <- rill_agent_data_destination_details("ollama/llama3.3")
  remote <- rill_agent_data_destination_details(
    "ollama/llama3.3",
    base_url = "http://ollama.example:11434"
  )
  unknown <- rill_agent_data_destination_details("other/model")
  gateway_a <- rill_agent_data_destination_details(
    "openai/gpt-5",
    base_url = "https://gateway.example:8443/provider-a"
  )
  gateway_b <- rill_agent_data_destination_details(
    "openai/gpt-5",
    base_url = "https://gateway.example:8443/provider-b"
  )
  gateway_port <- rill_agent_data_destination_details(
    "openai/gpt-5",
    base_url = "https://gateway.example:9443/provider-a"
  )

  testthat::expect_identical(openai$id, upgraded$id)
  testthat::expect_identical(identical(openai$id, changed_policy$id), FALSE)
  testthat::expect_identical(openai$name, "OpenAI")
  testthat::expect_identical(openai$kind, "external")
  testthat::expect_identical(local$kind, "installation")
  testthat::expect_identical(remote$kind, "external")
  testthat::expect_identical(identical(local$id, remote$id), FALSE)
  testthat::expect_identical(unknown$kind, "external")
  testthat::expect_identical(gateway_a$label, gateway_b$label)
  testthat::expect_identical(identical(gateway_a$id, gateway_b$id), FALSE)
  testthat::expect_identical(identical(gateway_a$id, gateway_port$id), FALSE)
})

testthat::test_that("the provider projection removes source credentials", {
  document <- sample_rill_data()$documents[[1]]
  document$source_url <- paste0(
    "https://reader:source-secret@example.com/story?",
    "ticket=ST-secret-grant&session=private-session&view=full#private"
  )
  document$canonical_url <- paste0(
    "https://example.com/story?AWSAccessKeyId=AKIASECRET&",
    "view=canonical"
  )
  document$provenance <- list(
    awsAccessKeyId = "AKIA-METADATA-SECRET",
    sso_assertion = "metadata-secret",
    request = list(
      authorization = "Bearer provenance-secret",
      embedded_source = paste0(
        "Captured from ",
        "https://worker:worker-secret@example.org/fetch?",
        "sig=provenance-signature&format=md"
      )
    )
  )

  supplied <- rill_document_tool(document)()

  testthat::expect_identical(
    supplied$source_url,
    "https://example.com/story"
  )
  testthat::expect_identical(
    supplied$canonical_url,
    "https://example.com/story"
  )
  testthat::expect_identical(
    supplied$provenance,
    rill_agent_provenance_summary(document)
  )
  testthat::expect_null(supplied$provenance$awsAccessKeyId)
  testthat::expect_null(supplied$provenance$sso_assertion)
  testthat::expect_null(supplied$provenance$request)
  testthat::expect_identical(supplied$content_hash, document$content_hash)
  testthat::expect_identical(supplied$record_hash, document$record_hash)
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
  testthat::expect_identical(
    rill_agent_data_destination("anthropic/claude-sonnet-4-5-20250929"),
    "Anthropic at api.anthropic.com"
  )
})

testthat::test_that("the reader Agent stream exposes its latest partial text", {
  stream <- coro::async_generator(function() {
    coro::yield("Source ")
    coro::yield("evidence.")
  })()
  partials <- character()
  tracked <- track_reader_agent_stream(
    stream,
    \(partial) partials <<- c(partials, partial)
  )
  collected <- NULL
  error <- NULL

  coro::async_collect(tracked) |>
    promises::then(\(value) collected <<- value) |>
    promises::catch(\(condition) error <<- condition)
  timeout <- Sys.time() + 2
  while (is.null(collected) && is.null(error) && Sys.time() < timeout) {
    later::run_now(0.01)
  }

  testthat::expect_null(error)
  testthat::expect_identical(collected, list("Source ", "evidence."))
  testthat::expect_identical(partials, c("Source ", "Source evidence."))
})

testthat::test_that("reader streaming supports current and legacy Deputy APIs", {
  modern_call <- NULL
  modern <- list(
    run_shiny = function(prompt, run_context) {
      modern_call <<- list(prompt = prompt, run_context = run_context)
      "modern-stream"
    },
    stream_async = \(...) stop("legacy path must not run")
  )
  legacy_call <- NULL
  legacy <- list(stream_async = function(prompt, stream, run_context) {
    legacy_call <<- list(
      prompt = prompt,
      stream = stream,
      run_context = run_context
    )
    "legacy-stream"
  })
  context <- list(rill_agent_run_id = "run-1")

  testthat::expect_identical(
    rill_agent_shiny_stream(modern, "What changed?", context),
    "modern-stream"
  )
  testthat::expect_identical(
    modern_call,
    list(prompt = "What changed?", run_context = context)
  )
  testthat::expect_identical(
    rill_agent_shiny_stream(legacy, "What changed?", context),
    "legacy-stream"
  )
  testthat::expect_identical(
    legacy_call,
    list(prompt = "What changed?", stream = "content", run_context = context)
  )
})

testthat::test_that("a Deputy Agent is pinned to the selected Document", {
  document <- sample_rill_data()$documents[[1]]
  chat <- ellmer::chat_openai(
    credentials = \() "test-key",
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
    names(rill_agent_chat_call(agent, "get_tools", list())),
    "read_current_document"
  )
  testthat::expect_identical(
    rill_agent_chat_call(agent, "get_system_prompt"),
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
  testthat::expect_identical(
    rill_agent_runtime_identity(agent, "openai"),
    list(
      model = "gpt-5.4",
      data_destination = "OpenAI",
      data_destination_id = rill_agent_data_destination_details("openai")$id
    )
  )
})
