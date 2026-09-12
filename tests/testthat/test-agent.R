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
  loopback <- rill_agent_data_destination_details(
    "ollama/llama3.3",
    base_url = "http://127.24.1.9:11434"
  )
  loopback_spoof <- rill_agent_data_destination_details(
    "ollama/llama3.3",
    base_url = "http://127.attacker.example:11434"
  )

  openrouter <- rill_agent_data_destination_details(
    "openrouter/meta/muse-spark-1.3-contributor",
    policy_url = "https://openrouter.ai/privacy"
  )

  testthat::expect_identical(openrouter$name, "OpenRouter")
  testthat::expect_identical(openrouter$label, "OpenRouter at openrouter.ai")
  testthat::expect_identical(
    openrouter$endpoint,
    "https://openrouter.ai/api/v1"
  )
  testthat::expect_identical(openrouter$kind, "external")
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
  testthat::expect_identical(loopback$kind, "installation")
  testthat::expect_identical(loopback_spoof$kind, "external")
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
  complete <- NULL
  tracked <- track_reader_agent_stream(
    stream,
    \(partial) partials <<- c(partials, partial),
    on_complete = \(response) complete <<- response
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
  testthat::expect_identical(complete, "Source evidence.")
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


testthat::test_that("source display uses the returned Document and preserves original tool content", {
  request <- ellmer::ContentToolRequest(
    id = "tool",
    name = "read_current_document",
    arguments = list()
  )
  value <- list(
    title = "Pinned source",
    source_url = "https://example.com/source",
    markdown = "Original text",
    record_hash = "retained-hash"
  )
  original <- ellmer::ContentToolResult(value = value, request = request)
  decorated <- reader_tool_result_display(original)
  testthat::expect_identical(decorated@value, value)
  testthat::expect_identical(decorated@request, request)
  testthat::expect_identical(original@extra, list())
  testthat::expect_match(
    htmltools::renderTags(decorated@extra$display$html)$html,
    "Pinned source",
    fixed = TRUE
  )
  failed <- ellmer::ContentToolResult(
    value = NULL,
    error = "Unavailable",
    request = request
  )
  testthat::expect_identical(reader_tool_result_display(failed), failed)
  unrelated <- ellmer::ContentToolResult(value = value)
  testthat::expect_identical(reader_tool_result_display(unrelated), unrelated)
})


testthat::test_that("source display makes missing metadata explicit without changing the result", {
  request <- ellmer::ContentToolRequest(
    id = "tool",
    name = "read_current_document",
    arguments = list()
  )
  for (missing in list(NULL, character(), "", "  ", NA_character_)) {
    value <- list(
      title = missing,
      site = missing,
      captured_at = missing,
      acquisition_method = missing,
      limitations = missing
    )
    result <- reader_tool_result_display(
      ellmer::ContentToolResult(value = value, request = request)
    )
    html <- xml2::read_html(as.character(result@extra$display$html))
    paragraphs <- xml2::xml_text(xml2::xml_find_all(
      html,
      "//div[@class='rill-document-result']/p"
    ))
    paragraphs <- trimws(gsub("[[:space:]]+", " ", paragraphs))
    testthat::expect_identical(
      paragraphs,
      c(
        "Untitled source",
        "Site: Not recorded",
        "Original Source URL unavailable",
        "Captured: Not recorded",
        "Preparation: Not recorded",
        "Limitations: Not recorded"
      )
    )
    testthat::expect_identical(result@value, value)
  }
  value <- list(site = "Example", limitations = c(NA, "", "Partial capture"))
  result <- reader_tool_result_display(
    ellmer::ContentToolResult(value = value, request = request)
  )
  html <- xml2::read_html(as.character(result@extra$display$html))
  paragraphs <- xml2::xml_text(xml2::xml_find_all(
    html,
    "//div[@class='rill-document-result']/p"
  ))
  paragraphs <- trimws(gsub("[[:space:]]+", " ", paragraphs))
  testthat::expect_contains(
    paragraphs,
    c(
      "Site: Example",
      "Limitations: Partial capture"
    )
  )
  testthat::expect_identical(result@value, value)
})

testthat::test_that("OpenRouter chats state their data policy and keep a fixed endpoint", {
  calls <- list()
  testthat::local_mocked_bindings(
    rill_ellmer_chat = function(...) {
      calls[[length(calls) + 1L]] <<- list(...)
      "chat"
    }
  )

  rill_agent_chat("openrouter/meta/muse-spark-1.3-contributor")
  rill_agent_chat(
    "openrouter/meta/muse-spark-1.3-contributor",
    base_url = "https://openrouter.ai/api/v1/"
  )
  rill_agent_chat("openai/gpt-5", base_url = "https://gateway.example/v1")

  testthat::expect_identical(
    calls[[1L]],
    list(
      name = "openrouter/meta/muse-spark-1.3-contributor",
      echo = "none",
      api_args = list(provider = list(data_collection = "allow"))
    )
  )
  testthat::expect_identical(calls[[2L]], calls[[1L]])
  testthat::expect_identical(
    calls[[3L]],
    list(
      name = "openai/gpt-5",
      echo = "none",
      base_url = "https://gateway.example/v1"
    )
  )
  testthat::expect_error(
    rill_agent_chat(
      "openrouter/meta/muse-spark-1.3-contributor",
      base_url = "https://gateway.example/v1"
    ),
    class = "rill_agent_url_invalid"
  )
  testthat::expect_length(calls, 3L)
})

testthat::test_that("usage limits keep the cost cap only when ellmer can price the model", {
  withr::local_options(lifecycle_verbosity = "error")
  priced <- ellmer::chat_openai(credentials = \() "test-key", model = "gpt-5.4")
  unpriced <- ellmer::chat_openrouter(
    credentials = \() "test-key",
    model = "meta/muse-spark-1.3-contributor"
  )

  testthat::expect_identical(rill_agent_usage_limits(priced)$max_cost_usd, 2)
  testthat::expect_null(rill_agent_usage_limits(unpriced)$max_cost_usd)
  testthat::expect_identical(
    rill_agent_run_limits(rill_agent_usage_limits(priced))$max_cost_usd,
    2
  )
  testthat::expect_null(
    rill_agent_run_limits(rill_agent_usage_limits(unpriced))$max_cost_usd
  )
  testthat::expect_identical(
    rill_agent_usage_limits(unpriced)$max_total_tokens,
    128000L
  )

  agent <- rill_reader_agent(
    document = sample_rill_data()$documents[[1]],
    reader_id = "reader-1",
    session_id = "rill-session-1",
    chat = unpriced
  )
  testthat::expect_null(agent$usage_limits$max_cost_usd)
  testthat::expect_null(
    rill_agent_run_limits(
      rill_agent_effective_limits(agent, rill_agent_usage_limits())
    )$max_cost_usd
  )
  testthat::expect_identical(
    rill_agent_effective_limits(
      simpleError("no agent"),
      rill_agent_usage_limits()
    )$max_cost_usd,
    2
  )
})
