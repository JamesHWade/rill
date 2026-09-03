capture_test_payload <- function(...) {
  utils::modifyList(
    list(
      capture_id = "browser-capture-001",
      source_url = "https://example.com/notes/one",
      canonical_url = "https://example.com/notes/one",
      title = "A locally captured document",
      author = "Ada Lovelace",
      site = "Example Notes",
      published_at = "2026-08-29T12:30:00Z",
      markdown = "# A local document\n\nSource-grounded text.",
      captured_at = "2026-08-30T22:15:00Z",
      producer = "rill-test-clipper",
      producer_version = "1.0.0",
      metadata = list(selection = "article")
    ),
    list(...)
  )
}

capture_test_request <- function(
  payload,
  token = "test-secret",
  path = capture_endpoint_path,
  method = "POST",
  content_type = "application/json"
) {
  body <- if (is.character(payload)) {
    payload
  } else {
    jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null")
  }
  list2env(
    list(
      PATH_INFO = path,
      REQUEST_METHOD = method,
      CONTENT_TYPE = content_type,
      HTTP_AUTHORIZATION = paste("Bearer", token),
      rook.input = list(read = function() charToRaw(body))
    ),
    parent = emptyenv()
  )
}
