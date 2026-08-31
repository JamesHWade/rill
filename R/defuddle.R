parse_markdown_frontmatter <- function(markdown) {
  lines <- strsplit(markdown, "\n", fixed = TRUE)[[1]]
  if (!length(lines) || trimws(lines[[1]]) != "---") {
    return(list(metadata = list(), markdown = markdown))
  }

  closing <- which(trimws(lines[-1]) %in% c("---", "..."))
  if (!length(closing)) {
    return(list(metadata = list(), markdown = markdown))
  }
  closing <- closing[[1]] + 1L

  yaml_text <- paste(lines[seq.int(2L, closing - 1L)], collapse = "\n")
  metadata <- tryCatch(
    yaml::yaml.load(yaml_text) %||% list(),
    error = function(error) list()
  )
  body <- if (closing < length(lines)) {
    paste(lines[seq.int(closing + 1L, length(lines))], collapse = "\n")
  } else {
    ""
  }

  list(metadata = metadata, markdown = body)
}

defuddle_endpoint <- function(base_url, source_url) {
  source_url <- validate_public_http_url(source_url)
  stripped <- sub("^https?://", "", source_url, ignore.case = TRUE)
  paste0(sub("/$", "", base_url), "/", stripped)
}

fetch_defuddled_markdown <- function(source_url, config) {
  backend <- normalize_defuddle_backend(config$defuddle_backend %||% "hosted")
  if (requireNamespace("otel", quietly = TRUE)) {
    try(
      otel::start_local_active_span(
        "article.extract",
        attributes = list("extractor.engine" = paste0("defuddle-", backend)),
        tracer = "rill",
        end_on_exit = TRUE
      ),
      silent = TRUE
    )
  }

  if (identical(backend, "local")) {
    return(fetch_defuddled_markdown_local(source_url, config))
  }

  fetch_defuddled_markdown_hosted(source_url, config)
}

fetch_defuddled_markdown_hosted <- function(source_url, config) {
  request <- httr2::request(defuddle_endpoint(
    config$defuddle_base_url,
    source_url
  )) |>
    httr2::req_user_agent(rill_user_agent()) |>
    httr2::req_timeout(30) |>
    httr2::req_retry(max_tries = 2)

  if (nzchar(config$defuddle_api_key)) {
    request <- httr2::req_url_query(request, key = config$defuddle_api_key)
  }

  response <- httr2::req_perform(request)
  httr2::resp_body_string(response)
}

fetch_defuddled_markdown_local <- function(
  source_url,
  config,
  runner = run_defuddle_cli
) {
  source_url <- validate_public_http_url(source_url)
  result <- runner(
    config$defuddle_command %||% "defuddle",
    c(
      "parse",
      source_url,
      "--md",
      "--frontmatter",
      "--user-agent",
      rill_user_agent()
    ),
    timeout = 30
  )

  if (!identical(result$status, 0L)) {
    detail <- trimws(result$stderr %||% result$stdout %||% "")
    detail <- substr(gsub("[\r\n]+", " ", detail), 1L, 500L)
    if (!nzchar(detail)) {
      detail <- paste0("The command exited with status ", result$status, ".")
    }
    cli::cli_abort(c(
      "Local Defuddle extraction failed.",
      "x" = "{detail}"
    ))
  }

  markdown <- result$stdout %||% ""
  if (!nzchar(trimws(markdown))) {
    cli::cli_abort("Local Defuddle returned an empty document.")
  }
  markdown
}

run_defuddle_cli <- function(command, args, timeout) {
  resolved <- unname(Sys.which(command))
  if (!nzchar(resolved)) {
    cli::cli_abort(c(
      "The local Defuddle executable {.file {command}} was not found.",
      "i" = "Install it with {.code npm install -g defuddle}, or set {.envvar DEFUDDLE_COMMAND}."
    ))
  }

  stderr_file <- tempfile("rill-defuddle-stderr-")
  on.exit(unlink(stderr_file), add = TRUE)
  stdout <- suppressWarnings(system2(
    resolved,
    args = vapply(args, shQuote, character(1)),
    stdout = TRUE,
    stderr = stderr_file,
    timeout = timeout
  ))
  stderr <- if (file.exists(stderr_file)) {
    paste(readLines(stderr_file, warn = FALSE), collapse = "\n")
  } else {
    ""
  }

  list(
    status = as.integer(attr(stdout, "status") %||% 0L),
    stdout = paste(stdout, collapse = "\n"),
    stderr = stderr
  )
}

word_count <- function(markdown) {
  words <- strsplit(trimws(gsub("[`#>*_~\\[\\]()]", " ", markdown)), "\\s+")[[
    1
  ]]
  as.integer(sum(nzchar(words)))
}

document_from_defuddle <- function(entry, config) {
  raw <- fetch_defuddled_markdown(entry$url, config)
  parsed <- parse_markdown_frontmatter(raw)
  metadata <- parsed$metadata
  backend <- normalize_defuddle_backend(config$defuddle_backend %||% "hosted")
  captured_at <- utc_now()

  new_rill_document(
    entry_id = entry$entry_id,
    source_url = entry$url,
    canonical_url = entry$canonical_url %||% NA_character_,
    acquisition_method = "web_extraction",
    producer = paste0("defuddle-", backend),
    producer_version = as.character(
      metadata$defuddle_version %||% metadata$version %||% NA_character_
    ),
    title = as.character(metadata$title %||% entry$title),
    author = as.character(metadata$author %||% entry$author %||% NA_character_),
    site = as.character(
      metadata$site %||% metadata$domain %||% entry$feed_title
    ),
    published_at = as.character(
      metadata$published %||%
        metadata$date %||%
        entry$published_at %||%
        NA_character_
    ),
    markdown = parsed$markdown,
    captured_at = captured_at,
    received_at = captured_at,
    provenance = list(
      backend = backend,
      extractor_metadata = metadata
    )
  )
}

document_fallback <- function(entry, reason = "feed-content") {
  content <- entry$feed_content %||%
    entry$summary %||%
    "No readable content was supplied by this feed."
  content <- plain_summary(content, max_chars = 20000L)

  captured_at <- utc_now()
  new_rill_document(
    entry_id = entry$entry_id,
    source_url = entry$url,
    canonical_url = entry$canonical_url %||% NA_character_,
    acquisition_method = "feed_fallback",
    producer = reason,
    title = entry$title,
    author = entry$author %||% NA_character_,
    site = entry$feed_title,
    published_at = entry$published_at %||% NA_character_,
    markdown = content,
    captured_at = captured_at,
    received_at = captured_at,
    provenance = list(kind = "feed_content_fallback")
  )
}

get_or_extract_document <- function(store, entry, config) {
  cached <- store_get_document(store, entry$entry_id)
  if (!is.null(cached)) {
    return(cached)
  }

  document <- tryCatch(
    document_from_defuddle(entry, config),
    error = function(error) {
      telemetry_log(
        "warn",
        "article.extract_failed",
        list("entry.id" = entry$entry_id, "error.type" = class(error)[[1]])
      )
      fallback <- document_fallback(entry, reason = "feed-fallback")
      fallback$extraction_error <- conditionMessage(error)
      fallback
    }
  )
  store_save_document(store, document)
  document
}

render_document <- function(document) {
  html <- commonmark::markdown_html(
    document$markdown %||% "",
    footnotes = TRUE,
    extensions = c(
      "table",
      "strikethrough",
      "autolink",
      "tagfilter",
      "tasklist"
    )
  )
  htmltools::HTML(sanitize_rendered_html(html))
}

sanitize_rendered_html <- function(html) {
  parsed <- xml2::read_html(paste0(
    "<div id='rill-sanitizer-root'>",
    html,
    "</div>"
  ))
  root <- xml2::xml_find_first(parsed, "//*[@id='rill-sanitizer-root']")
  blocked <- xml2::xml_find_all(
    root,
    ".//script | .//style | .//iframe | .//object | .//embed | .//form | .//input | .//button | .//svg | .//math | .//link | .//meta"
  )
  if (length(blocked)) {
    xml2::xml_remove(blocked)
  }

  nodes <- xml2::xml_find_all(root, ".//*")
  for (node in nodes) {
    attributes <- names(xml2::xml_attrs(node))
    unsafe_names <- attributes[
      grepl("^on", attributes, ignore.case = TRUE) |
        attributes %in% c("style", "srcdoc", "srcset")
    ]
    for (attribute in unsafe_names) {
      xml2::xml_attr(node, attribute) <- NULL
    }

    for (attribute in intersect(attributes, c("href", "src", "poster"))) {
      value <- trimws(xml2::xml_attr(node, attribute) %||% "")
      safe <- grepl(
        "^(https?:|mailto:|#|/|\\./|\\.\\./)",
        value,
        ignore.case = TRUE
      )
      safe_data_image <- identical(attribute, "src") &&
        grepl(
          "^data:image/(png|gif|jpeg|webp);base64,",
          value,
          ignore.case = TRUE
        )
      if (nzchar(value) && !safe && !safe_data_image) {
        xml2::xml_attr(node, attribute) <- NULL
      }
    }
  }

  paste(
    vapply(xml2::xml_children(root), as.character, character(1)),
    collapse = "\n"
  )
}
