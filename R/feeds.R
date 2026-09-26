validate_public_http_url <- function(url) {
  if (!is.character(url) || length(url) != 1L || is.na(url)) {
    cli::cli_abort(
      "{.arg url} must be a single string.",
      class = "rill_url_invalid"
    )
  }

  parsed <- tryCatch(
    httr2::url_parse(trimws(url)),
    error = function(error) NULL
  )
  if (is.null(parsed)) {
    cli::cli_abort(
      "{.arg url} must be a complete {.code http://} or {.code https://} URL.",
      class = "rill_url_invalid"
    )
  }
  host <- tolower(parsed$hostname %||% "")
  scheme <- tolower(parsed$scheme %||% "")

  if (!scheme %in% c("http", "https") || !nzchar(host)) {
    cli::cli_abort(
      "{.arg url} must be a complete {.code http://} or {.code https://} URL.",
      class = "rill_url_invalid"
    )
  }

  # curl resolves every `*.localhost` name to loopback and accepts numeric
  # hosts such as `2130706433` or `0x7f.1`, so numeric hosts must be canonical
  # public IPv4 addresses. IPv6 literals are refused outright.
  numeric_host <- grepl(
    "^(0x[0-9a-f]*|[0-9]+)([.](0x[0-9a-f]*|[0-9]+)){0,3}[.]?$",
    host
  )
  blocked <- host %in%
    c("localhost", "localhost.localdomain") ||
    grepl("(^|\\.)(local|localhost)\\.?$", host) ||
    grepl(":|^\\[", host) ||
    (numeric_host && !queue_preview_public_ipv4(host))

  if (blocked) {
    cli::cli_abort(
      "{.arg url} must not refer to a private or local network.",
      class = "rill_url_invalid"
    )
  }
  httr2::url_build(parsed)
}

# Many feeds declare their encoding only in the XML prolog, which httr2 ignores.
feed_body_string <- function(response) {
  body <- httr2::resp_body_raw(response)
  content_type <- httr2::resp_header(response, "content-type") %||% ""
  charset <- regmatches(
    content_type,
    regexec("charset=\"?([^;\"[:space:]]+)", content_type, ignore.case = TRUE)
  )[[1L]]
  encoding <- if (length(charset)) {
    charset[[2L]]
  } else {
    prolog <- rawToChar(utils::head(body[body != as.raw(0L)], 200L))
    declared <- regmatches(
      prolog,
      regexec(
        "^[^<]*<[?]xml[^>]*encoding=[\"']([A-Za-z0-9._-]+)",
        prolog,
        useBytes = TRUE
      )
    )[[1L]]
    if (length(declared)) declared[[2L]] else "UTF-8"
  }
  decode <- function(from) {
    tryCatch(
      iconv(readBin(body, character()), from = from, to = "UTF-8"),
      error = \(error) NA_character_
    )
  }
  text <- decode(encoding)
  if (is.na(text) && !length(charset)) {
    text <- decode("windows-1252")
  }
  if (is.na(text)) {
    cli::cli_abort(
      "The feed is not valid {encoding} text.",
      class = "rill_feed_encoding_invalid"
    )
  }
  text
}

# Retry only when the server asks for a short wait: httr2 otherwise sleeps for
# the full `Retry-After`, which would stall the app or hold the polling lock.
feed_retry_is_transient <- function(response) {
  status <- httr2::resp_status(response)
  after <- httr2::resp_retry_after(response)
  status %in% c(429L, 503L) && (is.na(after) || after <= 10)
}

feed_request <- function(
  url,
  etag = NULL,
  last_modified = NULL,
  max_redirects = 5L
) {
  # Follow redirects one hop at a time so that every destination is checked
  # before Rill requests it.
  for (hop in 0:max_redirects) {
    request <- httr2::request(validate_public_http_url(url)) |>
      httr2::req_user_agent(rill_user_agent()) |>
      httr2::req_timeout(20) |>
      httr2::req_options(followlocation = FALSE) |>
      httr2::req_retry(max_tries = 2, is_transient = feed_retry_is_transient)

    if (!is.null(etag) && !is.na(etag) && nzchar(etag)) {
      request <- httr2::req_headers(request, `If-None-Match` = etag)
    }
    if (
      !is.null(last_modified) && !is.na(last_modified) && nzchar(last_modified)
    ) {
      request <- httr2::req_headers(
        request,
        `If-Modified-Since` = last_modified
      )
    }

    response <- httr2::req_perform(request)
    location <- httr2::resp_header(response, "location")
    if (
      !httr2::resp_status(response) %in% c(301L, 302L, 303L, 307L, 308L) ||
        is.null(location)
    ) {
      return(response)
    }
    url <- xml2::url_absolute(location, httr2::resp_url(response))
  }
  cli::cli_abort("The feed redirected more than {max_redirects} times.")
}

looks_like_feed <- function(response, body) {
  is_obvious_html <- grepl(
    "^\\s*(?:<\\?xml[^>]*>\\s*)?(?:<!doctype\\s+html(?:\\s|>)|<html(?:\\s|>))",
    body,
    ignore.case = TRUE,
    perl = TRUE
  )
  if (is_obvious_html) {
    return(FALSE)
  }

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

# xml2 treats a string without `<` or `>` as a URL or file path and reads it.
# Response bodies and feed fields are untrusted, so always parse them as text.
read_markup <- function(text, as_html = FALSE) {
  bytes <- charToRaw(enc2utf8(text))
  if (as_html) {
    xml2::read_html(bytes, encoding = "UTF-8")
  } else {
    xml2::read_xml(bytes, encoding = "UTF-8")
  }
}

read_feed_xml <- function(xml) {
  tryCatch(
    if (is.character(xml)) read_markup(xml) else xml2::read_xml(xml),
    error = function(error) {
      can_repair <- is.character(xml) &&
        length(xml) == 1L &&
        !is.na(xml) &&
        grepl("]]>", xml, fixed = TRUE)
      if (!can_repair) {
        stop(error)
      }

      repaired_xml <- escape_text_terminators(xml)
      tryCatch(
        read_markup(repaired_xml),
        error = function(repair_error) stop(error)
      )
    }
  )
}

escape_text_terminators <- function(xml) {
  cdata <- gregexpr("(?s)<!\\[CDATA\\[.*?\\]\\]>", xml, perl = TRUE)[[1L]]
  if (cdata[[1L]] == -1L) {
    return(gsub("]]>", "]]&gt;", xml, fixed = TRUE))
  }

  lengths <- attr(cdata, "match.length")
  parts <- character(length(cdata) * 2L + 1L)
  cursor <- 1L
  for (i in seq_along(cdata)) {
    end <- cdata[[i]] + lengths[[i]] - 1L
    parts[[2L * i - 1L]] <- gsub(
      "]]>",
      "]]&gt;",
      substr(xml, cursor, cdata[[i]] - 1L),
      fixed = TRUE
    )
    parts[[2L * i]] <- substr(xml, cdata[[i]], end)
    cursor <- end + 1L
  }
  parts[[length(parts)]] <- gsub(
    "]]>",
    "]]&gt;",
    substr(xml, cursor, nchar(xml)),
    fixed = TRUE
  )
  paste0(parts, collapse = "")
}

discover_feed_url <- function(page_url, html) {
  document <- read_markup(html, as_html = TRUE)
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

# XPath unions return nodes in document order, not in the order written, so
# preferred fields are looked up one expression at a time.
xml_first_text_of <- function(node, xpaths) {
  for (xpath in xpaths) {
    value <- xml_first_text(node, xpath)
    if (!is.na(value)) {
      return(value)
    }
  }
  NA_character_
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

rfc822_zone_minutes <- function(zone) {
  if (grepl("^[+-][0-9]{4}$", zone)) {
    sign <- if (startsWith(zone, "-")) -1L else 1L
    hours <- as.integer(substr(zone, 2L, 3L))
    minutes <- as.integer(substr(zone, 4L, 5L))
    return(sign * (hours * 60L + minutes))
  }
  zones <- c(
    GMT = 0L,
    UT = 0L,
    UTC = 0L,
    Z = 0L,
    EST = -300L,
    EDT = -240L,
    CST = -360L,
    CDT = -300L,
    MST = -420L,
    MDT = -360L,
    PST = -480L,
    PDT = -420L
  )
  if (!nzchar(zone)) 0L else unname(zones[zone])
}

# parsedate ignores RFC 822 offsets such as `-0700`, which RSS pubDates use.
parse_rfc822_date <- function(value) {
  pattern <- paste0(
    "^\\s*(?:[A-Za-z]+,?\\s+)?([0-9]{1,2})\\s+([A-Za-z]{3})[A-Za-z]*\\.?\\s+",
    "([0-9]{4}|[0-9]{2})\\s+([0-9]{1,2}):([0-9]{2})(?::([0-9]{2}))?",
    "\\s*([+-][0-9]{4}|[A-Za-z]{1,3})?\\s*$"
  )
  parts <- regmatches(value, regexec(pattern, value, perl = TRUE))[[1L]]
  missing <- as.POSIXct(NA_real_, tz = "UTC")
  if (!length(parts)) {
    return(missing)
  }
  month <- match(tolower(parts[[3L]]), tolower(month.abb))
  offset <- rfc822_zone_minutes(toupper(parts[[8L]]))
  if (is.na(month) || is.na(offset)) {
    return(missing)
  }
  year <- as.integer(parts[[4L]])
  if (year < 100L) {
    year <- year + if (year < 50L) 2000L else 1900L
  }
  local <- ISOdatetime(
    year,
    month,
    as.integer(parts[[2L]]),
    as.integer(parts[[5L]]),
    as.integer(parts[[6L]]),
    if (nzchar(parts[[7L]])) as.integer(parts[[7L]]) else 0L,
    tz = "UTC"
  )
  local - offset * 60
}

parse_feed_date <- function(value) {
  if (is.null(value) || length(value) == 0L || is.na(value) || !nzchar(value)) {
    return(NA_character_)
  }
  parsed <- parse_rfc822_date(value)
  if (is.na(parsed)) {
    parsed <- suppressWarnings(parsedate::parse_date(value))
  }
  if (is.na(parsed)) {
    return(NA_character_)
  }
  # An explicit time keeps midnight values from parsing as bare dates.
  format(parsed, "%Y-%m-%d %H:%M:%S", tz = "UTC", usetz = TRUE)
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
    preview_image_url = character(),
    preview_image_alt = character(),
    published_at = character(),
    inserted_at = character(),
    content_hash = character(),
    stringsAsFactors = FALSE
  )
}

# RSS channels often carry an `atom:link rel="self"` before their `<link>`.
rss_link_xpath <- paste0(
  "./*[local-name()='link' and ",
  "namespace-uri()!='http://www.w3.org/2005/Atom'][1]"
)

parse_feed_document <- function(
  xml,
  feed_url,
  headers = list(),
  folder = "Unsorted"
) {
  document <- read_feed_xml(xml)
  is_atom <- identical(xml2::xml_name(document), "feed")
  channel <- xml2::xml_find_first(
    document,
    "/*[local-name()='rss' or local-name()='RDF']/*[local-name()='channel']"
  )
  if (!is_atom && inherits(channel, "xml_missing")) {
    cli::cli_abort(
      "The document is not an RSS or Atom feed.",
      class = "rill_feed_unsupported_document"
    )
  }
  items <- if (is_atom) {
    xml2::xml_find_all(
      document,
      "/*[local-name()='feed']/*[local-name()='entry']"
    )
  } else {
    xml2::xml_find_all(
      document,
      paste0(
        "/*[local-name()='rss']/*[local-name()='channel']/*[local-name()='item']",
        " | /*[local-name()='RDF']/*[local-name()='item']"
      )
    )
  }

  feed_title <- if (is_atom) {
    xml_first_text(
      document,
      "/*[local-name()='feed']/*[local-name()='title'][1]"
    )
  } else {
    xml_first_text(
      channel,
      "./*[local-name()='title'][1]"
    )
  }
  site_url <- if (is_atom) {
    xml_first_attr(
      document,
      "/*[local-name()='feed']/*[local-name()='link'][@rel='alternate' or not(@rel)][1]",
      "href"
    )
  } else {
    xml_first_text(channel, rss_link_xpath)
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
      xml_first_text(item, rss_link_xpath) %||%
        xml_first_text(
          item,
          paste0(
            "./*[local-name()='guid'][not(@isPermaLink='false')]",
            "[starts-with(normalize-space(.), 'http')][1]"
          )
        )
    }
    if (!is.na(url)) {
      url <- xml2::url_absolute(url, feed_url)
    }

    external_id <- xml_first_text(
      item,
      "./*[local-name()='guid' or local-name()='id'][1]"
    )
    if (is.na(external_id)) {
      external_id <- xml_first_text(
        item,
        "./@*[local-name()='about' and namespace-uri()='http://www.w3.org/1999/02/22-rdf-syntax-ns#']"
      )
    }
    published_raw <- xml_first_text_of(
      item,
      c(
        "./*[local-name()='pubDate'][1]",
        "./*[local-name()='published'][1]",
        "./*[local-name()='updated'][1]",
        "./*[local-name()='date'][1]"
      )
    )
    author <- xml_first_text_of(
      item,
      c(
        "./*[local-name()='author']/*[local-name()='name'][1]",
        "./*[local-name()='creator'][1]",
        "./*[local-name()='author'][not(*)][1]"
      )
    )
    # Media RSS also uses `content`, but carries no text.
    content <- xml_first_text_of(
      item,
      c(
        "./*[local-name()='encoded'][1]",
        paste0(
          "./*[local-name()='content' and ",
          "namespace-uri()!='http://search.yahoo.com/mrss/'][1]"
        ),
        "./*[local-name()='description'][1]",
        "./*[local-name()='summary'][1]"
      )
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
    preview <- entry_preview_image(content, url, item)

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
      preview_image_url = preview$url,
      preview_image_alt = preview$alt,
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
    title = if (is.na(feed_title)) feed_url else feed_title,
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
  telemetry_local_span("feed.fetch")
  response <- feed_request(url, etag = etag, last_modified = last_modified)
  status <- httr2::resp_status(response)
  if (identical(status, 304L)) {
    return(list(not_modified = TRUE))
  }

  body <- feed_body_string(response)
  final_url <- httr2::resp_url(response)
  if (!looks_like_feed(response, body)) {
    discovered_url <- discover_feed_url(final_url, body)
    response <- feed_request(discovered_url)
    body <- feed_body_string(response)
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
  result$entries$feed_id <- rep(feed$feed_id, nrow(result$entries))
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
