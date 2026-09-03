empty_opml_subscriptions <- function() {
  data.frame(
    title = character(),
    feed_url = character(),
    site_url = character(),
    folder = character(),
    stringsAsFactors = FALSE
  )
}

opml_attribute <- function(node, name) {
  value <- trimws(xml2::xml_attr(node, name) %||% "")
  if (nzchar(value)) value else NA_character_
}

opml_label <- function(value) {
  if (is.na(value) || !nzchar(value)) {
    return(NA_character_)
  }
  plain_summary(value, max_chars = 500L)
}

opml_folder <- function(node) {
  ancestors <- xml2::xml_find_all(
    node,
    "ancestor::*[local-name()='outline' and not(@xmlUrl)]"
  )
  if (!length(ancestors)) {
    return("Unsorted")
  }

  labels <- vapply(
    ancestors,
    function(ancestor) {
      label <- opml_attribute(ancestor, "title") %||%
        opml_attribute(ancestor, "text")
      opml_label(label)
    },
    character(1)
  )
  labels <- labels[!is.na(labels) & nzchar(labels)]
  if (length(labels)) paste(labels, collapse = " / ") else "Unsorted"
}

opml_is_commented <- function(node) {
  ancestors <- xml2::xml_find_all(
    node,
    "ancestor-or-self::*[local-name()='outline']"
  )
  values <- xml2::xml_attr(ancestors, "isComment")
  values[is.na(values)] <- ""
  values <- tolower(values)
  any(values == "true")
}

#' Read feed subscriptions from an OPML file
#'
#' `read_opml()` reads an OPML 1.0, 1.1, or 2.0 subscription list into a
#' data frame. Nested outline groups are represented as slash-separated folder
#' paths. Commented outlines are ignored.
#'
#' @param file A path to an OPML file.
#'
#' @return A data frame with `title`, `feed_url`, `site_url`, and `folder`
#'   columns.
#' @export
read_opml <- function(file) {
  if (!is.character(file) || length(file) != 1L || is.na(file)) {
    cli::cli_abort(
      "{.arg file} must be a single path.",
      class = "rill_error_opml"
    )
  }
  if (!file.exists(file)) {
    cli::cli_abort(
      "OPML file {.path {file}} does not exist.",
      class = "rill_error_opml"
    )
  }
  size <- file.info(file)$size
  if (!is.na(size) && size > 10 * 1024^2) {
    cli::cli_abort(
      "OPML file {.path {file}} is larger than 10 MB.",
      class = "rill_error_opml"
    )
  }

  document <- tryCatch(
    xml2::read_xml(file, options = "NONET"),
    error = function(error) {
      cli::cli_abort(
        "Can't parse {.path {file}} as XML.",
        parent = error,
        class = "rill_error_opml"
      )
    }
  )
  root <- xml2::xml_find_first(document, "/*[local-name()='opml']")
  if (inherits(root, "xml_missing")) {
    cli::cli_abort(
      "The file is not an OPML document.",
      class = "rill_error_opml"
    )
  }
  body <- xml2::xml_find_first(root, "./*[local-name()='body']")
  if (inherits(body, "xml_missing")) {
    cli::cli_abort(
      "The OPML document does not contain a body.",
      class = "rill_error_opml"
    )
  }

  nodes <- xml2::xml_find_all(
    body,
    ".//*[local-name()='outline' and @xmlUrl]"
  )
  if (length(nodes) > 10000L) {
    cli::cli_abort(
      "The OPML document contains more than 10,000 subscriptions.",
      class = "rill_error_opml"
    )
  }
  nodes <- nodes[!vapply(nodes, opml_is_commented, logical(1))]
  if (!length(nodes)) {
    return(empty_opml_subscriptions())
  }

  rows <- lapply(nodes, function(node) {
    feed_url <- opml_attribute(node, "xmlUrl")
    title <- opml_attribute(node, "title") %||%
      opml_attribute(node, "text") %||%
      feed_url
    data.frame(
      title = opml_label(title),
      feed_url = feed_url,
      site_url = opml_attribute(node, "htmlUrl"),
      folder = opml_folder(node),
      stringsAsFactors = FALSE
    )
  })
  subscriptions <- do.call(rbind, rows)
  subscriptions <- subscriptions[
    !is.na(subscriptions$feed_url) & nzchar(subscriptions$feed_url),
    ,
    drop = FALSE
  ]
  subscriptions <- subscriptions[
    !duplicated(subscriptions$feed_url),
    ,
    drop = FALSE
  ]
  rownames(subscriptions) <- NULL
  subscriptions
}

opml_http_url <- function(value) {
  parsed <- tryCatch(
    httr2::url_parse(trimws(value)),
    error = function(error) NULL
  )
  if (
    is.null(parsed) ||
      !tolower(parsed$scheme %||% "") %in% c("http", "https") ||
      !nzchar(parsed$hostname %||% "")
  ) {
    return(NA_character_)
  }
  httr2::url_build(parsed)
}

normalize_opml_subscriptions <- function(feeds, strict_urls = TRUE) {
  if (!is.data.frame(feeds) || !"feed_url" %in% names(feeds)) {
    cli::cli_abort(
      "{.arg feeds} must be a data frame with a {.field feed_url} column.",
      class = "rill_error_opml"
    )
  }
  feeds <- as.data.frame(feeds, stringsAsFactors = FALSE)
  if (!"title" %in% names(feeds)) {
    feeds$title <- feeds$feed_url
  }
  if (!"site_url" %in% names(feeds)) {
    feeds$site_url <- NA_character_
  }
  if (!"folder" %in% names(feeds)) {
    feeds$folder <- "Unsorted"
  }

  feeds <- feeds[c("title", "feed_url", "site_url", "folder")]
  feeds[] <- lapply(feeds, as.character)
  if (isTRUE(strict_urls)) {
    feeds$feed_url <- vapply(feeds$feed_url, opml_http_url, character(1))
    invalid <- which(is.na(feeds$feed_url))
    if (length(invalid)) {
      cli::cli_abort(
        "Every {.field feed_url} must be a complete HTTP or HTTPS URL.",
        class = "rill_error_opml"
      )
    }
  } else {
    feeds$feed_url <- trimws(feeds$feed_url)
  }
  feeds$title[is.na(feeds$title) | !nzchar(trimws(feeds$title))] <-
    feeds$feed_url[is.na(feeds$title) | !nzchar(trimws(feeds$title))]
  feeds$title <- trimws(feeds$title)
  feeds$folder[is.na(feeds$folder) | !nzchar(trimws(feeds$folder))] <-
    "Unsorted"
  feeds$folder <- trimws(feeds$folder)
  if (isTRUE(strict_urls)) {
    feeds$site_url <- vapply(
      feeds$site_url,
      function(url) {
        if (is.na(url) || !nzchar(trimws(url))) {
          return(NA_character_)
        }
        opml_http_url(url)
      },
      character(1)
    )
  }
  feeds <- feeds[!duplicated(feeds$feed_url), , drop = FALSE]
  feeds <- feeds[
    order(tolower(feeds$folder), tolower(feeds$title), feeds$feed_url),
    ,
    drop = FALSE
  ]
  rownames(feeds) <- NULL
  feeds
}

opml_folder_parts <- function(folder) {
  if (identical(tolower(folder), "unsorted")) {
    return(character())
  }
  parts <- trimws(strsplit(folder, "/", fixed = TRUE)[[1]])
  parts[nzchar(parts)]
}

#' Write feed subscriptions to an OPML file
#'
#' `write_opml()` writes feed subscriptions as an OPML 2.0 subscription list.
#' Slash-separated folder paths are written as nested outline groups.
#'
#' @param feeds A data frame containing a `feed_url` column and optional
#'   `title`, `site_url`, and `folder` columns.
#' @param file A path for the OPML output file.
#' @param title The title stored in the OPML document head.
#'
#' @return `file`, invisibly.
#' @export
write_opml <- function(feeds, file, title = "Rill subscriptions") {
  if (!is.character(file) || length(file) != 1L || is.na(file)) {
    cli::cli_abort(
      "{.arg file} must be a single path.",
      class = "rill_error_opml"
    )
  }
  if (!dir.exists(dirname(file))) {
    cli::cli_abort(
      "Directory {.path {dirname(file)}} does not exist.",
      class = "rill_error_opml"
    )
  }
  if (!is.character(title) || length(title) != 1L || is.na(title)) {
    cli::cli_abort(
      "{.arg title} must be a single string.",
      class = "rill_error_opml"
    )
  }
  feeds <- normalize_opml_subscriptions(feeds)

  document <- xml2::xml_new_root("opml", version = "2.0")
  head <- xml2::xml_add_child(document, "head")
  xml2::xml_add_child(head, "title", title)
  xml2::xml_add_child(
    head,
    "dateCreated",
    format(Sys.time(), "%a, %d %b %Y %H:%M:%S GMT", tz = "GMT")
  )
  xml2::xml_add_child(head, "docs", "https://opml.org/spec2.opml")
  body <- xml2::xml_add_child(document, "body")

  folder_nodes <- new.env(parent = emptyenv())
  for (index in seq_len(nrow(feeds))) {
    parent <- body
    folder_key <- ""
    for (part in opml_folder_parts(feeds$folder[[index]])) {
      folder_key <- paste(folder_key, part, sep = "\u241f")
      if (!exists(folder_key, envir = folder_nodes, inherits = FALSE)) {
        node <- xml2::xml_add_child(parent, "outline")
        xml2::xml_set_attrs(node, c(text = part, title = part))
        assign(folder_key, node, envir = folder_nodes)
      }
      parent <- get(folder_key, envir = folder_nodes, inherits = FALSE)
    }

    node <- xml2::xml_add_child(parent, "outline")
    attributes <- c(
      text = feeds$title[[index]],
      title = feeds$title[[index]],
      type = "rss",
      xmlUrl = feeds$feed_url[[index]]
    )
    if (!is.na(feeds$site_url[[index]])) {
      attributes <- c(attributes, htmlUrl = feeds$site_url[[index]])
    }
    xml2::xml_set_attrs(node, attributes)
  }

  xml2::write_xml(document, file, options = "format", encoding = "UTF-8")
  invisible(file)
}

opml_public_site_url <- function(value) {
  if (is.na(value) || !nzchar(trimws(value))) {
    return(NA_character_)
  }
  tryCatch(
    validate_public_http_url(value),
    error = function(error) NA_character_
  )
}

format_opml_import_status <- function(
  summary,
  demo_mode = FALSE,
  refresh_hint = FALSE
) {
  pieces <- character()
  if (summary$imported > 0L) {
    pieces <- c(
      pieces,
      paste(
        "Imported",
        summary$imported,
        if (summary$imported == 1L) "feed" else "feeds",
        if (isTRUE(demo_mode)) "for this session" else NULL
      )
    )
  }
  if (summary$stories > 0L) {
    pieces <- c(
      pieces,
      paste(
        summary$stories,
        "new",
        if (summary$stories == 1L) "story" else "stories"
      )
    )
  }
  if (summary$refresh_failed > 0L) {
    pieces <- c(
      pieces,
      paste(
        summary$refresh_failed,
        if (summary$refresh_failed == 1L) "feed" else "feeds",
        "couldn't refresh"
      )
    )
  }
  if (summary$failed > 0L) {
    pieces <- c(
      pieces,
      paste(
        summary$failed,
        if (summary$failed == 1L) "feed" else "feeds",
        "skipped"
      )
    )
  }
  if (!length(pieces)) {
    return("No feed subscriptions found in that OPML file")
  }
  message <- paste(pieces, collapse = " \u00b7 ")
  if (isTRUE(refresh_hint) && summary$imported > 0L) {
    message <- paste0(message, ". Refresh feeds to fetch stories.")
  }
  message
}

import_opml_subscriptions <- function(
  store,
  actor_id,
  subscriptions,
  refresh = TRUE,
  progress = NULL
) {
  subscriptions <- normalize_opml_subscriptions(
    subscriptions,
    strict_urls = FALSE
  )
  existing <- store_list_feeds(
    store,
    actor_id,
    source_kind = "subscription",
    active_only = FALSE
  )
  summary <- list(
    total = nrow(subscriptions),
    imported = 0L,
    added = 0L,
    updated = 0L,
    stories = 0L,
    refresh_failed = 0L,
    failed = 0L,
    errors = character()
  )

  for (index in seq_len(nrow(subscriptions))) {
    subscription <- subscriptions[index, , drop = FALSE]
    if (is.function(progress)) {
      progress(index, nrow(subscriptions), subscription$title[[1]])
    }

    feed_url <- tryCatch(
      validate_public_http_url(subscription$feed_url[[1]]),
      error = function(error) error
    )
    if (inherits(feed_url, "error")) {
      summary$failed <- summary$failed + 1L
      summary$errors <- c(summary$errors, conditionMessage(feed_url))
      next
    }

    existing_index <- match(feed_url, existing$feed_url)
    existing_feed <- if (is.na(existing_index)) {
      store_find_feed_by_url(store, feed_url)
    } else {
      existing[existing_index, , drop = FALSE]
    }
    is_new <- is.na(existing_index) ||
      !identical(existing$status[[existing_index]], "active")
    feed <- list(
      feed_id = if (is.null(existing_feed)) {
        rill_id("feed", feed_url)
      } else {
        existing_feed$feed_id[[1]]
      },
      feed_url = feed_url,
      site_url = if (is.null(existing_feed)) {
        opml_public_site_url(subscription$site_url[[1]])
      } else {
        existing_feed$site_url[[1]]
      },
      title = if (is.null(existing_feed)) {
        feed_url
      } else {
        existing_feed$title[[1]]
      },
      folder = subscription$folder[[1]],
      source_kind = "subscription",
      etag = if (is.null(existing_feed)) {
        NA_character_
      } else {
        existing_feed$etag[[1]]
      },
      last_modified = if (is.null(existing_feed)) {
        NA_character_
      } else {
        existing_feed$last_modified[[1]]
      },
      poll_status = if (is.null(existing_feed)) {
        "new"
      } else {
        existing_feed$poll_status[[1]]
      }
    )

    stored <- tryCatch(
      {
        if (is.null(existing_feed)) {
          store_upsert_feed(store, feed)
        }
        store_subscribe_feed(
          store,
          actor_id,
          feed$feed_id,
          folder = subscription$folder[[1]]
        )
        store_rename_feed(
          store,
          actor_id,
          feed$feed_id,
          subscription$title[[1]]
        )
        TRUE
      },
      error = function(error) error
    )
    if (inherits(stored, "error")) {
      summary$failed <- summary$failed + 1L
      summary$errors <- c(summary$errors, conditionMessage(stored))
      next
    }

    summary$imported <- summary$imported + 1L
    if (is_new) {
      summary$added <- summary$added + 1L
    } else {
      summary$updated <- summary$updated + 1L
    }

    if (isTRUE(refresh)) {
      refreshed <- tryCatch(
        refresh_feed(store, feed),
        error = function(error) error
      )
      if (inherits(refreshed, "error")) {
        summary$refresh_failed <- summary$refresh_failed + 1L
        summary$errors <- c(summary$errors, conditionMessage(refreshed))
        feed$poll_status <- "error"
        try(store_upsert_feed(store, feed), silent = TRUE)
      } else {
        summary$stories <- summary$stories + as.integer(refreshed$added %||% 0L)
      }
    }
  }

  summary
}
