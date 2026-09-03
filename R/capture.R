capture_endpoint_path <- "/api/v1/captures"
capture_max_body_bytes <- 5L * 1024L * 1024L

capture_abort <- function(message, class = "rill_capture_invalid") {
  cli::cli_abort(message, class = class, .envir = parent.frame())
}

capture_string <- function(
  payload,
  name,
  required = FALSE,
  max_chars = NULL
) {
  value <- payload[[name]]
  if (is.null(value) || !length(value) || all(is.na(value))) {
    if (required) {
      capture_abort("{.field {name}} is required.")
    }
    return(NA_character_)
  }
  if (!is.character(value) || length(value) != 1L || is.na(value)) {
    capture_abort("{.field {name}} must be a single string.")
  }
  if (required && !nzchar(trimws(value))) {
    capture_abort("{.field {name}} must not be empty.")
  }
  if (!is.null(max_chars) && nchar(value, type = "chars") > max_chars) {
    capture_abort("{.field {name}} is too long.")
  }
  value
}

capture_timestamp <- function(payload, name, required = FALSE) {
  value <- capture_string(payload, name, required = required, max_chars = 100L)
  if (is.na(value)) {
    return(value)
  }
  iso_8601 <- paste0(
    "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:",
    "[0-9]{2}(?:\\.[0-9]+)?(?:Z|[+-][0-9]{2}:[0-9]{2})$"
  )
  if (!grepl(iso_8601, value)) {
    capture_abort("{.field {name}} must be an ISO 8601 date-time.")
  }
  parsed <- suppressWarnings(parsedate::parse_date(value))
  if (is.na(parsed)) {
    capture_abort("{.field {name}} must be an ISO 8601 date-time.")
  }
  format(parsed, tz = "UTC", usetz = TRUE)
}

normalize_capture_payload <- function(payload) {
  if (
    !is.list(payload) || is.null(names(payload)) || !all(nzchar(names(payload)))
  ) {
    capture_abort("The request body must be a JSON object.")
  }

  metadata <- payload$metadata %||% list()
  if (!is.list(metadata)) {
    capture_abort("{.field metadata} must be a JSON object.")
  }
  markdown <- capture_string(payload, "markdown", required = TRUE)
  if (nchar(markdown, type = "bytes") > capture_max_body_bytes) {
    capture_abort(
      "{.field markdown} is larger than 5 MiB.",
      class = "rill_capture_too_large"
    )
  }

  source_url <- capture_public_url(
    capture_string(
      payload,
      "source_url",
      required = TRUE,
      max_chars = 4096L
    ),
    "source_url"
  )
  canonical_url <- capture_string(
    payload,
    "canonical_url",
    max_chars = 4096L
  )
  if (is.na(canonical_url)) {
    canonical_url <- source_url
  } else {
    canonical_url <- capture_public_url(canonical_url, "canonical_url")
  }

  list(
    capture_id = capture_string(
      payload,
      "capture_id",
      required = TRUE,
      max_chars = 500L
    ),
    source_url = source_url,
    canonical_url = canonical_url,
    title = capture_string(
      payload,
      "title",
      required = TRUE,
      max_chars = 1000L
    ),
    author = capture_string(payload, "author", max_chars = 1000L),
    site = capture_string(payload, "site", max_chars = 1000L),
    published_at = capture_timestamp(payload, "published_at"),
    markdown = markdown,
    captured_at = capture_timestamp(payload, "captured_at", required = TRUE),
    producer = capture_string(
      payload,
      "producer",
      required = TRUE,
      max_chars = 200L
    ),
    producer_version = capture_string(
      payload,
      "producer_version",
      max_chars = 200L
    ),
    metadata = metadata
  )
}

capture_public_url <- function(value, name) {
  tryCatch(
    validate_public_http_url(value),
    error = function(error) {
      capture_abort("{.field {name}} must be a public HTTP or HTTPS URL.")
    }
  )
}

capture_feed <- function() {
  list(
    feed_id = rill_id("feed", "local-captures"),
    feed_url = "rill://local-captures",
    site_url = NA_character_,
    title = "Local captures",
    folder = "Captured",
    source_kind = "capture",
    etag = NA_character_,
    last_modified = NA_character_,
    poll_status = "capture"
  )
}

capture_entry <- function(capture, feed, reader_id, received_at) {
  data.frame(
    entry_id = rill_id(
      "entry",
      feed$feed_id,
      reader_id,
      capture$canonical_url
    ),
    feed_id = feed$feed_id,
    external_id = rill_id(
      "capture-entry",
      reader_id,
      capture$canonical_url
    ),
    url = capture$source_url,
    canonical_url = capture$canonical_url,
    title = capture$title,
    author = capture$author,
    summary = plain_summary(capture$markdown),
    feed_content = NA_character_,
    published_at = capture$published_at,
    inserted_at = received_at,
    content_hash = rill_id("captured-entry", capture$markdown),
    stringsAsFactors = FALSE
  )
}

document_from_capture <- function(
  capture,
  entry_id,
  actor_id,
  received_at,
  document_id
) {
  new_rill_document(
    document_id = document_id,
    entry_id = entry_id,
    source_url = capture$source_url,
    canonical_url = capture$canonical_url,
    acquisition_method = "browser_capture",
    producer = capture$producer,
    reader_id = actor_id,
    producer_version = capture$producer_version,
    producer_record_id = capture$capture_id,
    title = capture$title,
    author = capture$author,
    site = capture$site,
    published_at = capture$published_at,
    markdown = capture$markdown,
    captured_at = capture$captured_at,
    received_at = received_at,
    provenance = list(
      captured_by = actor_id,
      producer_metadata = capture$metadata
    )
  )
}

capture_document <- function(
  store,
  payload,
  actor_id,
  received_at = utc_now()
) {
  capture <- normalize_capture_payload(payload)
  document_id <- rill_id(
    "document",
    "browser-capture",
    actor_id,
    capture$capture_id
  )

  existing <- store_get_document_record(store, document_id)
  if (!is.null(existing)) {
    document <- document_from_capture(
      capture,
      existing$entry_id,
      actor_id,
      existing$received_at,
      document_id
    )
    saved <- store_save_document(store, document)
    entry_id <- existing$entry_id
  } else {
    entry <- store_find_entry_by_url(
      store,
      actor_id,
      capture$source_url,
      capture$canonical_url
    )
    if (is.null(entry)) {
      feed <- capture_feed()
      store_upsert_feed(store, feed)
      store_subscribe_feed(
        store,
        actor_id,
        feed$feed_id,
        folder = feed$folder
      )
      entry <- capture_entry(capture, feed, actor_id, received_at)
      store_upsert_entries(store, entry)
      entry_id <- entry$entry_id[[1]]
    } else {
      entry_id <- entry$entry_id
    }
    document <- document_from_capture(
      capture,
      entry_id,
      actor_id,
      received_at,
      document_id
    )
    saved <- store_save_document(store, document)
  }

  event <- list(
    event_id = rill_id("capture-event", actor_id, document_id),
    reader_id = actor_id,
    entry_id = entry_id,
    session_id = rill_id("capture-session", actor_id, capture$capture_id),
    event_type = "document_captured",
    happened_at = received_at,
    surface = "capture_endpoint",
    position = NA_integer_,
    payload = list(
      document_id = document_id,
      acquisition_method = "browser_capture",
      created = saved$created
    )
  )
  store_record_event(store, event)

  list(
    created = saved$created,
    capture_id = capture$capture_id,
    entry_id = entry_id,
    document_id = document_id,
    content_hash = document$content_hash,
    source_url = document$source_url
  )
}

capture_json_response <- function(status, body, headers = list()) {
  default_headers <- list(
    "Cache-Control" = "no-store",
    "Access-Control-Allow-Origin" = "*"
  )
  if (!identical(as.integer(status), 204L)) {
    default_headers[["Content-Type"]] <- "application/json; charset=utf-8"
  }
  list(
    status = as.integer(status),
    headers = c(default_headers, headers),
    body = if (identical(as.integer(status), 204L)) {
      raw()
    } else {
      jsonlite::toJSON(
        body,
        auto_unbox = TRUE,
        null = "null",
        na = "null"
      )
    }
  )
}

capture_bearer_token <- function(request) {
  authorization <- request$HTTP_AUTHORIZATION %||% ""
  if (!grepl("^Bearer[[:space:]]+.+$", authorization, ignore.case = TRUE)) {
    return("")
  }
  sub("^Bearer[[:space:]]+", "", authorization, ignore.case = TRUE)
}

capture_token_hash <- function(token) {
  if (!store_scalar_string(token)) {
    capture_abort(
      "A capture token must be a non-empty string.",
      class = "rill_capture_credential_invalid"
    )
  }
  rill_id("capture-token", token)
}

store_set_capture_credential <- function(
  store,
  reader_id,
  token,
  now = utc_now()
) {
  token_hash <- capture_token_hash(token)
  if (identical(store$mode, "postgres")) {
    conflict <- DBI::dbGetQuery(
      store$pool,
      paste(
        "SELECT reader_id FROM reader_capture_credentials",
        "WHERE token_hash = $1 AND reader_id <> $2"
      ),
      params = list(token_hash, reader_id)
    )
    if (nrow(conflict)) {
      capture_abort(
        "That capture token is already assigned to another Reader.",
        class = "rill_capture_credential_conflict"
      )
    }
    DBI::dbExecute(
      store$pool,
      paste(
        "INSERT INTO reader_capture_credentials",
        "(reader_id, token_hash, created_at, updated_at)",
        "VALUES ($1, $2, $3, $3)",
        "ON CONFLICT (reader_id) DO UPDATE SET",
        "token_hash = EXCLUDED.token_hash, updated_at = EXCLUDED.updated_at"
      ),
      params = list(reader_id, token_hash, now)
    )
    return(invisible(reader_id))
  }

  credentials <- store$memory$capture_credentials
  conflict <- credentials$token_hash == token_hash &
    credentials$reader_id != reader_id
  if (any(conflict)) {
    capture_abort(
      "That capture token is already assigned to another Reader.",
      class = "rill_capture_credential_conflict"
    )
  }
  index <- match(reader_id, credentials$reader_id)
  if (!is.na(index)) {
    credentials$token_hash[[index]] <- token_hash
    credentials$updated_at[[index]] <- now
    store$memory$capture_credentials <- credentials
    return(invisible(reader_id))
  }
  if (!reader_id %in% store$memory$readers$reader_id) {
    capture_abort(
      "A capture credential requires an existing Reader.",
      class = "rill_capture_credential_invalid"
    )
  }
  store$memory$capture_credentials <- rbind(
    credentials,
    data.frame(
      reader_id = reader_id,
      token_hash = token_hash,
      created_at = now,
      updated_at = now,
      stringsAsFactors = FALSE
    )
  )
  invisible(reader_id)
}

store_resolve_capture_reader <- function(store, token) {
  if (!store_scalar_string(token)) {
    return(reader_identity_resolution(NULL, status = "missing"))
  }
  token_hash <- capture_token_hash(token)
  if (identical(store$mode, "postgres")) {
    rows <- DBI::dbGetQuery(
      store$pool,
      paste(
        "SELECT c.reader_id, r.status FROM reader_capture_credentials c",
        "JOIN readers r ON r.reader_id = c.reader_id",
        "WHERE c.token_hash = $1"
      ),
      params = list(token_hash)
    )
    if (!nrow(rows)) {
      return(reader_identity_resolution(NULL, status = "missing"))
    }
    return(reader_identity_resolution(
      if (identical(rows$status[[1L]], "active")) {
        rows$reader_id[[1L]]
      } else {
        NULL
      },
      status = rows$status[[1L]]
    ))
  }

  credentials <- store$memory$capture_credentials
  index <- match(token_hash, credentials$token_hash)
  if (is.na(index)) {
    return(reader_identity_resolution(NULL, status = "missing"))
  }
  store_resolve_reader(store, credentials$reader_id[[index]])
}

capture_request_body <- function(request) {
  content_type <- tolower(request$CONTENT_TYPE %||% "")
  if (!grepl("^application/json(?:[[:space:]]*;|$)", content_type)) {
    capture_abort(
      "The request Content-Type must be application/json.",
      class = "rill_capture_unsupported_media"
    )
  }
  body <- request$rook.input$read()
  if (is.raw(body)) {
    if (length(body) > capture_max_body_bytes) {
      capture_abort(
        "The request body is larger than 5 MiB.",
        "rill_capture_too_large"
      )
    }
    body <- rawToChar(body)
  }
  if (!is.character(body) || length(body) != 1L) {
    capture_abort("The request body could not be read as JSON.")
  }
  if (nchar(body, type = "bytes") > capture_max_body_bytes) {
    capture_abort(
      "The request body is larger than 5 MiB.",
      "rill_capture_too_large"
    )
  }

  tryCatch(
    jsonlite::fromJSON(body, simplifyVector = FALSE),
    error = function(error) {
      capture_abort("The request body is not valid JSON.")
    }
  )
}

capture_http_handler <- function(base_handler, store, config) {
  force(base_handler)
  force(store)
  force(config)
  if (nzchar(config$capture_token %||% "")) {
    store_set_capture_credential(
      store,
      config$actor_id,
      config$capture_token
    )
  }

  function(request) {
    if (!identical(request$PATH_INFO %||% "", capture_endpoint_path)) {
      return(base_handler(request))
    }
    if (!nzchar(config$capture_token %||% "")) {
      return(capture_json_response(404L, list(error = "Not found.")))
    }

    method <- toupper(request$REQUEST_METHOD %||% "GET")
    if (identical(method, "OPTIONS")) {
      return(capture_json_response(
        204L,
        list(),
        headers = list(
          "Access-Control-Allow-Methods" = "POST, OPTIONS",
          "Access-Control-Allow-Headers" = "Authorization, Content-Type",
          "Access-Control-Max-Age" = "600"
        )
      ))
    }
    if (!identical(method, "POST")) {
      return(capture_json_response(
        405L,
        list(error = "Method not allowed."),
        headers = list("Allow" = "POST, OPTIONS")
      ))
    }
    reader <- tryCatch(
      store_resolve_capture_reader(store, capture_bearer_token(request)),
      error = function(error) error
    )
    if (inherits(reader, "error")) {
      telemetry_log(
        "error",
        "capture.reader_status_failed",
        list("error.type" = class(reader)[[1]])
      )
      return(capture_json_response(
        503L,
        list(error = "Capture is temporarily unavailable.")
      ))
    }
    if (identical(reader$status, "missing")) {
      return(capture_json_response(
        401L,
        list(error = "Invalid capture token."),
        headers = list("WWW-Authenticate" = "Bearer")
      ))
    }

    if (!identical(reader$status, "active")) {
      return(capture_json_response(
        403L,
        list(error = "Capture is disabled for this Reader.")
      ))
    }

    result <- tryCatch(
      capture_document(
        store,
        capture_request_body(request),
        reader$reader_id
      ),
      error = function(error) error
    )
    if (inherits(result, "error")) {
      status <- if (inherits(result, "rill_capture_too_large")) {
        413L
      } else if (inherits(result, "rill_capture_unsupported_media")) {
        415L
      } else if (inherits(result, "rill_document_conflict")) {
        409L
      } else if (
        inherits(result, "rill_capture_invalid") ||
          inherits(result, "rill_document_invalid")
      ) {
        422L
      } else {
        telemetry_log(
          "error",
          "capture.write_failed",
          list("error.type" = class(result)[[1]])
        )
        500L
      }
      message <- if (status == 500L) {
        "The capture could not be stored."
      } else {
        conditionMessage(result)
      }
      return(capture_json_response(status, list(error = message)))
    }

    capture_json_response(
      if (result$created) 201L else 200L,
      c(list(status = if (result$created) "created" else "replayed"), result)
    )
  }
}
