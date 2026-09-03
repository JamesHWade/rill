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

first_metadata_value <- function(value, fallback = NA_character_) {
  as_values <- function(item) {
    if (is.null(item)) {
      return(character())
    }
    if (is.list(item)) {
      return(unlist(lapply(item, as_values), use.names = FALSE))
    }
    as.character(item)
  }

  values <- c(as_values(value), as_values(fallback))
  values <- values[!is.na(values) & nzchar(trimws(values))]
  if (length(values)) values[[1]] else NA_character_
}

first_publication_value <- function(value, fallback = NA_character_) {
  published <- first_metadata_value(value, fallback)
  if (is.na(published)) {
    return(published)
  }
  strsplit(
    published,
    ",\\s*(?=\\d{4}-\\d{2}-\\d{2}[T ])",
    perl = TRUE
  )[[1]][[1]]
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
    producer_version = first_metadata_value(
      metadata$defuddle_version,
      metadata$version
    ),
    title = first_metadata_value(metadata$title, entry$title),
    author = first_metadata_value(metadata$author, entry$author),
    site = first_metadata_value(
      metadata$site,
      metadata$domain %||%
        first_metadata_value(entry$source_feed_title, entry$feed_title)
    ),
    published_at = first_publication_value(
      metadata$published,
      metadata$date %||% entry$published_at
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
    site = first_metadata_value(entry$source_feed_title, entry$feed_title),
    published_at = entry$published_at %||% NA_character_,
    markdown = content,
    captured_at = captured_at,
    received_at = captured_at,
    provenance = list(kind = "feed_content_fallback")
  )
}

get_or_extract_document <- function(store, entry, config) {
  cached <- store_get_document(store, entry$entry_id)
  if (
    !is.null(cached) &&
      !(identical(cached$acquisition_method, "feed_fallback") &&
        identical(cached$producer, "orientation-feed-copy"))
  ) {
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

prepare_today_documents <- function(
  store,
  config,
  reader_id = config$actor_id,
  progress = function(index, total, title) invisible(NULL),
  now = Sys.time(),
  timezone = Sys.timezone()
) {
  entries <- store_list_entries(
    store,
    reader_id,
    view = "today",
    limit = 500L,
    now = now,
    timezone = timezone
  )
  documents <- store_list_documents(store, entries$entry_id)
  ready <- vapply(
    documents,
    \(document) !identical(document$acquisition_method, "feed_fallback"),
    logical(1)
  )
  cached_ids <- names(documents)[ready]
  pending <- entries[
    !as.character(entries$entry_id) %in% cached_ids,
    ,
    drop = FALSE
  ]
  errors <- character()
  prepared <- 0L

  for (index in seq_len(nrow(pending))) {
    entry <- as.list(pending[index, , drop = FALSE])
    progress(index, nrow(pending), entry$title)
    result <- tryCatch(
      {
        document <- document_from_defuddle(entry, config)
        store_save_document(store, document)
        document
      },
      error = function(error) error
    )
    if (inherits(result, "error")) {
      telemetry_log(
        "warn",
        "article.prepare_failed",
        list(
          "entry.id" = entry$entry_id,
          "error.type" = class(result)[[1]]
        )
      )
      errors[[entry$entry_id]] <- conditionMessage(result)
    } else {
      prepared <- prepared + 1L
    }
  }

  list(
    total = nrow(entries),
    cached = length(cached_ids),
    prepared = prepared,
    failed = length(errors),
    errors = errors
  )
}

format_prepare_today_status <- function(result) {
  parts <- character()
  if (result$prepared > 0L) {
    parts <- c(
      parts,
      paste(
        "Prepared",
        result$prepared,
        if (result$prepared == 1L) "reading copy" else "reading copies"
      )
    )
  }
  if (result$cached > 0L) {
    parts <- c(parts, paste(result$cached, "already ready"))
  }
  if (result$failed > 0L) {
    parts <- c(parts, paste(result$failed, "couldn't be prepared"))
  }
  if (!length(parts)) {
    return("No stories were published today")
  }
  paste(parts, collapse = " \u00b7 ")
}

render_document <- function(document) {
  markdown <- normalize_video_embeds(document$markdown %||% "")
  html <- commonmark::markdown_html(
    markdown,
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

normalize_video_embeds <- function(markdown) {
  pattern <- paste0(
    "(?is)<iframe\\b[^>]*>.*?</iframe\\s*>",
    "|<iframe\\b[^>]*/\\s*>"
  )
  locations <- gregexpr(pattern, markdown, perl = TRUE)[[1]]
  if (identical(locations[[1]], -1L)) {
    return(markdown)
  }

  matches <- regmatches(markdown, list(locations))[[1]]
  replacements <- vapply(
    matches,
    function(value) {
      parsed <- xml2::read_html(value)
      iframe <- xml2::xml_find_first(parsed, ".//iframe")
      embed_url <- video_embed_url(xml2::xml_attr(iframe, "src") %||% "")
      if (is.na(embed_url)) "" else paste0("![](", embed_url, ")")
    },
    character(1)
  )
  regmatches(markdown, list(locations)) <- list(replacements)
  markdown
}

video_embed_url <- function(value) {
  parsed <- tryCatch(
    httr2::url_parse(value),
    error = function(error) NULL
  )
  if (
    is.null(parsed) ||
      !identical(tolower(parsed$scheme %||% ""), "https") ||
      !nzchar(parsed$hostname %||% "")
  ) {
    return(NA_character_)
  }

  host <- tolower(parsed$hostname)
  path <- parsed$path %||% ""
  video_id <- NA_character_
  provider <- NA_character_

  if (
    host %in%
      c(
        "youtube.com",
        "www.youtube.com",
        "m.youtube.com",
        "youtube-nocookie.com",
        "www.youtube-nocookie.com"
      )
  ) {
    provider <- "youtube"
    if (identical(path, "/watch")) {
      video_id <- first_metadata_value(parsed$query$v)
    } else {
      matched <- regmatches(
        path,
        regexec(
          "^/(?:embed|shorts)/([A-Za-z0-9_-]{11})(?:/|$)",
          path
        )
      )[[1]]
      if (length(matched) == 2L) {
        video_id <- matched[[2]]
      }
    }
  } else if (identical(host, "youtu.be")) {
    provider <- "youtube"
    video_id <- sub("^/([^/]+).*$", "\\1", path)
  } else if (identical(host, "i.ytimg.com")) {
    provider <- "youtube"
    matched <- regmatches(
      path,
      regexec(
        "^/vi(?:_webp)?/([A-Za-z0-9_-]{11})(?:/|$)",
        path
      )
    )[[1]]
    if (length(matched) == 2L) {
      video_id <- matched[[2]]
    }
  } else if (host %in% c("vimeo.com", "www.vimeo.com", "player.vimeo.com")) {
    provider <- "vimeo"
    matched <- regmatches(path, regexec("^/(?:video/)?([0-9]+)(?:/|$)", path))[[
      1
    ]]
    if (length(matched) == 2L) {
      video_id <- matched[[2]]
    }
  }

  if (
    identical(provider, "youtube") &&
      !is.na(video_id) &&
      grepl("^[A-Za-z0-9_-]{11}$", video_id)
  ) {
    return(paste0("https://www.youtube-nocookie.com/embed/", video_id))
  }
  if (
    identical(provider, "vimeo") &&
      !is.na(video_id) &&
      grepl("^[0-9]+$", video_id)
  ) {
    return(paste0("https://player.vimeo.com/video/", video_id))
  }
  NA_character_
}

standardize_video_iframe <- function(node, source_url) {
  attributes <- names(xml2::xml_attrs(node))
  for (attribute in attributes) {
    xml2::xml_attr(node, attribute) <- NULL
  }
  xml2::xml_attr(node, "class") <- "video-embed"
  xml2::xml_attr(node, "src") <- source_url
  xml2::xml_attr(node, "title") <- "Embedded video"
  xml2::xml_attr(node, "loading") <- "lazy"
  xml2::xml_attr(node, "sandbox") <- paste(
    "allow-scripts",
    "allow-same-origin",
    "allow-presentation"
  )
  xml2::xml_attr(node, "allow") <- paste(
    "accelerometer; autoplay; encrypted-media; gyroscope;",
    "picture-in-picture; web-share"
  )
  xml2::xml_attr(node, "allowfullscreen") <- "allowfullscreen"
  xml2::xml_attr(node, "referrerpolicy") <- "strict-origin-when-cross-origin"
  invisible(node)
}

sanitize_rendered_html <- function(html) {
  parsed <- xml2::read_html(paste0(
    "<div id='rill-sanitizer-root'>",
    html,
    "</div>"
  ))
  root <- xml2::xml_find_first(parsed, "//*[@id='rill-sanitizer-root']")

  video_images <- xml2::xml_find_all(root, ".//img[@src]")
  for (node in video_images) {
    embed_url <- video_embed_url(xml2::xml_attr(node, "src"))
    if (!is.na(embed_url)) {
      iframe <- xml2::xml_add_sibling(
        node,
        xml2::xml_new_root("iframe"),
        .where = "before"
      )
      standardize_video_iframe(iframe, embed_url)
      xml2::xml_remove(node)
    }
  }

  iframes <- xml2::xml_find_all(root, ".//iframe")
  for (node in iframes) {
    embed_url <- video_embed_url(xml2::xml_attr(node, "src") %||% "")
    if (is.na(embed_url)) {
      xml2::xml_remove(node)
    } else {
      standardize_video_iframe(node, embed_url)
    }
  }

  blocked <- xml2::xml_find_all(
    root,
    paste(
      ".//script | .//style | .//object | .//embed | .//form | .//input |",
      ".//button | .//svg | .//math | .//link | .//meta"
    )
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
