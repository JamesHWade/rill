canonicalize_json_value <- function(value) {
  if (!is.list(value)) {
    return(value)
  }

  value_names <- names(value)
  if (!is.null(value_names) && all(nzchar(value_names))) {
    value <- value[order(value_names)]
  }
  lapply(value, canonicalize_json_value)
}

canonical_json <- function(value) {
  jsonlite::toJSON(
    canonicalize_json_value(value),
    auto_unbox = TRUE,
    null = "null",
    na = "null",
    digits = NA
  )
}

document_optional_string <- function(value) {
  if (is.null(value) || !length(value) || all(is.na(value))) {
    return(NA_character_)
  }
  value <- as.character(value[[1]])
  if (!nzchar(value)) NA_character_ else value
}

document_record_hash <- function(document, include_reader = TRUE) {
  identity_fields <- document[c(
    "entry_id",
    "reader_id",
    "source_url",
    "canonical_url",
    "acquisition_method",
    "producer",
    "producer_version",
    "producer_record_id",
    "title",
    "author",
    "site",
    "published_at",
    "captured_at",
    "content_hash",
    "provenance"
  )]
  if (!include_reader) {
    identity_fields$reader_id <- NULL
  }
  rill_id("document-record", canonical_json(identity_fields))
}

document_http_url <- function(value, argument) {
  value <- document_optional_string(value)
  parsed <- tryCatch(
    httr2::url_parse(value),
    error = function(error) NULL
  )
  if (
    is.na(value) ||
      is.null(parsed) ||
      !tolower(parsed$scheme %||% "") %in% c("http", "https") ||
      !nzchar(parsed$hostname %||% "")
  ) {
    cli::cli_abort(
      "{.arg {argument}} must be a complete HTTP or HTTPS URL.",
      class = "rill_document_invalid"
    )
  }
  httr2::url_build(parsed)
}

rill_document_original_source_url <- function(document) {
  canonical_url <- document_optional_string(document$canonical_url)
  if (!is.na(canonical_url)) {
    return(canonical_url)
  }
  document_optional_string(document$source_url)
}

rill_document_limitations <- function(document) {
  switch(
    document$acquisition_method %||% "",
    sample = "Bundled demo content cannot support real-world claims.",
    feed_fallback = paste(
      "This reading copy contains stored feed content and may be incomplete."
    ),
    web_extraction = paste(
      "Automated extraction may omit or reorder material from the",
      "Original Source."
    ),
    browser_capture = paste(
      "This browser capture reflects the Original Source at capture time",
      "and may omit unavailable or interactive content."
    ),
    paste(
      "This reading copy may not include all material from the",
      "Original Source."
    )
  )
}

new_rill_document <- function(
  entry_id,
  source_url,
  markdown,
  acquisition_method,
  producer,
  reader_id = NA_character_,
  producer_version = NA_character_,
  producer_record_id = NA_character_,
  canonical_url = NA_character_,
  title = NA_character_,
  author = NA_character_,
  site = NA_character_,
  published_at = NA_character_,
  captured_at = utc_now(),
  received_at = utc_now(),
  provenance = list(),
  document_id = NULL
) {
  source_url <- document_http_url(source_url, "source_url")
  canonical_url <- document_optional_string(canonical_url)
  if (!is.na(canonical_url)) {
    canonical_url <- document_http_url(canonical_url, "canonical_url")
  }
  if (
    !is.character(markdown) ||
      length(markdown) != 1L ||
      is.na(markdown) ||
      !nzchar(trimws(markdown))
  ) {
    cli::cli_abort(
      "{.arg markdown} must be a single, non-empty string.",
      class = "rill_document_invalid"
    )
  }
  if (
    !is.character(entry_id) ||
      length(entry_id) != 1L ||
      is.na(entry_id) ||
      !nzchar(entry_id)
  ) {
    cli::cli_abort(
      "{.arg entry_id} must be a single, non-empty string.",
      class = "rill_document_invalid"
    )
  }

  acquisition_method <- document_optional_string(acquisition_method)
  producer <- document_optional_string(producer)
  if (is.na(acquisition_method) || is.na(producer)) {
    cli::cli_abort(
      "A document requires an acquisition method and producer.",
      class = "rill_document_invalid"
    )
  }
  if (!is.list(provenance)) {
    cli::cli_abort(
      "{.arg provenance} must be a list.",
      class = "rill_document_invalid"
    )
  }
  reader_id <- document_optional_string(reader_id)
  is_private <- identical(acquisition_method, "browser_capture")
  if (is_private && is.na(reader_id)) {
    cli::cli_abort(
      "A browser-captured Document requires a Reader owner.",
      class = "rill_document_invalid"
    )
  }
  if (!is_private && !is.na(reader_id)) {
    cli::cli_abort(
      "Only browser-captured Documents may have a Reader owner.",
      class = "rill_document_invalid"
    )
  }

  content_hash <- rill_id("document-content", markdown)
  identity_fields <- list(
    entry_id = entry_id,
    reader_id = reader_id,
    source_url = source_url,
    canonical_url = document_optional_string(canonical_url),
    acquisition_method = acquisition_method,
    producer = producer,
    producer_version = document_optional_string(producer_version),
    producer_record_id = document_optional_string(producer_record_id),
    title = document_optional_string(title),
    author = document_optional_string(author),
    site = document_optional_string(site),
    published_at = document_optional_string(published_at),
    captured_at = document_optional_string(captured_at),
    content_hash = content_hash,
    provenance = provenance
  )
  record_hash <- document_record_hash(identity_fields)
  document_id <- document_id %||% rill_id("document", record_hash)

  structure(
    c(
      list(
        document_id = document_id,
        entry_id = entry_id,
        reader_id = reader_id,
        source_url = source_url,
        canonical_url = document_optional_string(canonical_url),
        acquisition_method = acquisition_method,
        producer = producer,
        producer_version = document_optional_string(producer_version),
        producer_record_id = document_optional_string(producer_record_id),
        captured_at = document_optional_string(captured_at),
        received_at = document_optional_string(received_at),
        title = document_optional_string(title),
        author = document_optional_string(author),
        site = document_optional_string(site),
        published_at = document_optional_string(published_at),
        markdown = markdown,
        word_count = word_count(markdown),
        content_hash = content_hash,
        record_hash = record_hash,
        provenance = provenance,
        schema_version = 1L
      )
    ),
    class = c("rill_document", "list")
  )
}

document_provenance_json <- function(document) {
  canonical_json(document$provenance %||% list())
}

document_conflict_abort <- function(message) {
  cli::cli_abort(message, class = "rill_document_conflict")
}

document_from_store_row <- function(row) {
  record <- as.list(row[1, , drop = FALSE])
  provenance <- record$provenance %||% "{}"
  if (is.character(provenance)) {
    provenance <- tryCatch(
      jsonlite::fromJSON(provenance, simplifyVector = FALSE),
      error = function(error) list()
    )
  }
  record$provenance <- provenance
  record$reader_id <- document_optional_string(record$reader_id)
  record$schema_version <- as.integer(record$schema_version %||% 1L)
  record$word_count <- as.integer(record$word_count %||% 0L)
  structure(record, class = c("rill_document", "list"))
}
