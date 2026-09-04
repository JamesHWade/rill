validate_public_http_url <- function(url) {
  if (!is.character(url) || length(url) != 1L || is.na(url)) {
    cli::cli_abort("{.arg url} must be a single string.")
  }

  parsed <- tryCatch(
    httr2::url_parse(trimws(url)),
    error = function(error) NULL
  )
  if (is.null(parsed)) {
    cli::cli_abort(
      "{.arg url} must be a complete {.code http://} or {.code https://} URL."
    )
  }
  host <- tolower(parsed$hostname %||% "")
  scheme <- tolower(parsed$scheme %||% "")

  if (!scheme %in% c("http", "https") || !nzchar(host)) {
    cli::cli_abort(
      "{.arg url} must be a complete {.code http://} or {.code https://} URL."
    )
  }

  blocked <- host %in%
    c("localhost", "localhost.localdomain", "0.0.0.0") ||
    grepl("(^|\\.)local$", host) ||
    grepl("^127\\.", host) ||
    grepl("^10\\.", host) ||
    grepl("^192\\.168\\.", host) ||
    grepl("^169\\.254\\.", host) ||
    grepl("^172\\.(1[6-9]|2[0-9]|3[01])\\.", host) ||
    host %in% c("::1", "[::1]")

  if (blocked) {
    cli::cli_abort("{.arg url} must not refer to a private or local network.")
  }
  httr2::url_build(parsed)
}

feed_request <- function(url, etag = NULL, last_modified = NULL) {
  request <- httr2::request(validate_public_http_url(url)) |>
    httr2::req_user_agent(rill_user_agent()) |>
    httr2::req_timeout(20) |>
    httr2::req_retry(max_tries = 2)

  if (!is.null(etag) && !is.na(etag) && nzchar(etag)) {
    request <- httr2::req_headers(request, `If-None-Match` = etag)
  }
  if (
    !is.null(last_modified) && !is.na(last_modified) && nzchar(last_modified)
  ) {
    request <- httr2::req_headers(request, `If-Modified-Since` = last_modified)
  }

  response <- httr2::req_perform(request)
  final_url <- httr2::resp_url(response)
  validate_public_http_url(final_url)
  response
}

looks_like_feed <- function(response, body) {
  content_type <- tolower(httr2::resp_header(response, "content-type") %||% "")
  if (grepl("(rss|atom|rdf|xml)", content_type)) {
    return(TRUE)
  }
  grepl(
    "^\\s*<\\?xml|^\\s*<(rss|feed|rdf:RDF)(\\s|>)",
    body,
    ignore.case = TRUE
  )
}

discover_feed_url <- function(page_url, html) {
  document <- xml2::read_html(html)
  link <- xml2::xml_find_first(
    document,
    paste0(
      "//link[contains(translate(@type, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ',",
      " 'abcdefghijklmnopqrstuvwxyz'), 'rss') or ",
      "contains(translate(@type, 'ABCDEFGHIJKLMNOPQRSTUVWXYZ',",
      " 'abcdefghijklmnopqrstuvwxyz'), 'atom')][@href][1]"
    )
  )
  if (inherits(link, "xml_missing")) {
    cli::cli_abort("The page does not advertise an RSS or Atom feed.")
  }
  validate_public_http_url(xml2::url_absolute(
    xml2::xml_attr(link, "href"),
    page_url
  ))
}

xml_first_text <- function(node, xpath) {
  match <- xml2::xml_find_first(node, xpath)
  if (inherits(match, "xml_missing")) {
    return(NA_character_)
  }
  value <- trimws(xml2::xml_text(match))
  if (nzchar(value)) value else NA_character_
}

xml_first_attr <- function(node, xpath, attribute) {
  match <- xml2::xml_find_first(node, xpath)
  if (inherits(match, "xml_missing")) {
    return(NA_character_)
  }
  value <- trimws(xml2::xml_attr(match, attribute) %||% "")
  if (nzchar(value)) value else NA_character_
}

plain_summary <- function(value, max_chars = 360L) {
  if (is.null(value) || length(value) == 0L || is.na(value) || !nzchar(value)) {
    return(NA_character_)
  }
  text <- tryCatch(
    xml2::xml_text(xml2::read_html(paste0("<div>", value, "</div>"))),
    error = function(error) gsub("<[^>]+>", " ", value)
  )
  text <- trimws(gsub("\\s+", " ", text))
  if (nchar(text) > max_chars) {
    paste0(substr(text, 1L, max_chars - 1L), "\u2026")
  } else {
    text
  }
}

parse_feed_date <- function(value) {
  if (is.null(value) || length(value) == 0L || is.na(value) || !nzchar(value)) {
    return(NA_character_)
  }
  parsed <- suppressWarnings(parsedate::parse_date(value))
  if (is.na(parsed)) {
    return(NA_character_)
  }
  format(parsed, tz = "UTC", usetz = TRUE)
}

empty_entries <- function() {
  data.frame(
    entry_id = character(),
    feed_id = character(),
    external_id = character(),
    url = character(),
    canonical_url = character(),
    title = character(),
    author = character(),
    summary = character(),
    feed_content = character(),
    published_at = character(),
    inserted_at = character(),
    content_hash = character(),
    stringsAsFactors = FALSE
  )
}

parse_feed_document <- function(
  xml,
  feed_url,
  headers = list(),
  folder = "Unsorted"
) {
  document <- xml2::read_xml(xml)
  rss_items <- xml2::xml_find_all(
    document,
    "//*[local-name()='channel']/*[local-name()='item']"
  )
  atom_items <- xml2::xml_find_all(
    document,
    "/*[local-name()='feed']/*[local-name()='entry']"
  )
  items <- if (length(rss_items)) rss_items else atom_items
  is_atom <- !length(rss_items)

  feed_title <- if (is_atom) {
    xml_first_text(
      document,
      "/*[local-name()='feed']/*[local-name()='title'][1]"
    )
  } else {
    xml_first_text(
      document,
      "//*[local-name()='channel']/*[local-name()='title'][1]"
    )
  }
  site_url <- if (is_atom) {
    xml_first_attr(
      document,
      "/*[local-name()='feed']/*[local-name()='link'][@rel='alternate' or not(@rel)][1]",
      "href"
    )
  } else {
    xml_first_text(
      document,
      "//*[local-name()='channel']/*[local-name()='link'][1]"
    )
  }
  if (!is.na(site_url)) {
    site_url <- xml2::url_absolute(site_url, feed_url)
  }

  feed_id <- rill_id("feed", feed_url)
  entry_rows <- lapply(items, function(item) {
    title <- xml_first_text(item, "./*[local-name()='title'][1]")
    url <- if (is_atom) {
      xml_first_attr(
        item,
        "./*[local-name()='link'][@rel='alternate' or not(@rel)][1]",
        "href"
      )
    } else {
      xml_first_text(item, "./*[local-name()='link'][1]")
    }
    if (!is.na(url)) {
      url <- xml2::url_absolute(url, feed_url)
    }

    external_id <- xml_first_text(
      item,
      "./*[local-name()='guid' or local-name()='id'][1]"
    )
    published_raw <- xml_first_text(
      item,
      "./*[local-name()='pubDate' or local-name()='published' or local-name()='updated' or local-name()='date'][1]"
    )
    author <- xml_first_text(
      item,
      "./*[local-name()='author']/*[local-name()='name'][1] | ./*[local-name()='creator'][1] | ./*[local-name()='author'][1]"
    )
    content <- xml_first_text(
      item,
      "./*[local-name()='encoded'][1] | ./*[local-name()='content'][1] | ./*[local-name()='description'][1] | ./*[local-name()='summary'][1]"
    )

    if (is.na(url) || !nzchar(url)) {
      return(NULL)
    }
    external_id <- external_id %||% url
    if (is.na(external_id) || !nzchar(external_id)) {
      external_id <- url
    }
    if (is.na(title) || !nzchar(title)) {
      title <- "Untitled"
    }
    published_at <- parse_feed_date(published_raw)

    data.frame(
      entry_id = rill_id("entry", feed_id, external_id),
      feed_id = feed_id,
      external_id = external_id,
      url = url,
      canonical_url = NA_character_,
      title = title,
      author = author,
      summary = plain_summary(content),
      feed_content = content,
      published_at = published_at,
      inserted_at = utc_now(),
      content_hash = rill_id("content", content %||% "", title),
      stringsAsFactors = FALSE
    )
  })
  entry_rows <- Filter(Negate(is.null), entry_rows)
  entries <- if (length(entry_rows)) {
    do.call(rbind, entry_rows)
  } else {
    empty_entries()
  }

  feed <- list(
    feed_id = feed_id,
    feed_url = feed_url,
    site_url = site_url,
    title = feed_title %||% feed_url,
    folder = folder,
    etag = headers$etag %||% NA_character_,
    last_modified = headers$last_modified %||% NA_character_,
    poll_status = "ok"
  )

  list(feed = feed, entries = entries)
}

fetch_feed <- function(
  url,
  etag = NULL,
  last_modified = NULL,
  folder = "Unsorted"
) {
  if (requireNamespace("otel", quietly = TRUE)) {
    try(
      otel::start_local_active_span(
        "feed.fetch",
        tracer = "rill",
        end_on_exit = TRUE
      ),
      silent = TRUE
    )
  }
  response <- feed_request(url, etag = etag, last_modified = last_modified)
  status <- httr2::resp_status(response)
  if (identical(status, 304L)) {
    return(list(not_modified = TRUE))
  }

  body <- httr2::resp_body_string(response)
  final_url <- httr2::resp_url(response)
  if (!looks_like_feed(response, body)) {
    discovered_url <- discover_feed_url(final_url, body)
    response <- feed_request(discovered_url)
    body <- httr2::resp_body_string(response)
    final_url <- httr2::resp_url(response)
    if (!looks_like_feed(response, body)) {
      cli::cli_abort("The discovered URL did not return RSS or Atom XML.")
    }
  }

  parsed <- parse_feed_document(
    body,
    feed_url = final_url,
    headers = list(
      etag = httr2::resp_header(response, "etag") %||% NA_character_,
      last_modified = httr2::resp_header(response, "last-modified") %||%
        NA_character_
    ),
    folder = folder
  )
  parsed$not_modified <- FALSE
  parsed
}

ingest_feed_url <- function(store, reader_id, url, folder = NULL) {
  result <- fetch_feed(url, folder = folder %||% "Unsorted")
  store_upsert_feed(store, result$feed)
  store_subscribe_feed(
    store,
    reader_id,
    result$feed$feed_id,
    folder = folder
  )
  added <- store_upsert_entries(store, result$entries)
  list(feed = result$feed, added = added, not_modified = FALSE)
}

refresh_feed <- function(store, feed) {
  result <- fetch_feed(
    feed$feed_url,
    etag = feed$etag %||% NULL,
    last_modified = feed$last_modified %||% NULL,
    folder = feed$folder %||% "Unsorted"
  )
  if (isTRUE(result$not_modified)) {
    return(list(feed_id = feed$feed_id, added = 0L, not_modified = TRUE))
  }

  result$feed$feed_id <- feed$feed_id
  result$entries$feed_id <- feed$feed_id
  result$entries$entry_id <- vapply(
    seq_len(nrow(result$entries)),
    function(index) {
      rill_id("entry", feed$feed_id, result$entries$external_id[[index]])
    },
    character(1)
  )
  store_upsert_feed(store, result$feed)
  added <- store_upsert_entries(store, result$entries)
  list(
    feed_id = feed$feed_id,
    added = added,
    not_modified = FALSE
  )
}

refresh_all_feeds <- function(store) {
  feeds <- store_list_active_feeds(store)
  results <- lapply(seq_len(nrow(feeds)), function(index) {
    feed <- as.list(feeds[index, , drop = FALSE])
    tryCatch(
      refresh_feed(store, feed),
      error = function(error) {
        list(
          feed_id = feed$feed_id,
          added = 0L,
          error = conditionMessage(error)
        )
      }
    )
  })
  results
}
