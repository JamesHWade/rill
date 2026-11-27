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
      if (is.na(embed_url)) "" else paste0('<img src="', embed_url, '">')
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

  # Browsers and libxml2 disagree about where comments end (`<!-->`), so a
  # comment can hide markup that only becomes live in the browser.
  inert <- xml2::xml_find_all(
    root,
    ".//comment() | .//processing-instruction()"
  )
  if (length(inert)) {
    xml2::xml_remove(inert)
  }

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
      xml2::xml_remove(xml2::xml_contents(node))
      standardize_video_iframe(node, embed_url)
    }
  }

  # Elements whose contents browsers parse as raw text are dropped whole,
  # because their serialized children could close the element early.
  blocked <- xml2::xml_find_all(
    root,
    paste(
      ".//script | .//style | .//object | .//embed | .//form | .//input |",
      ".//button | .//svg | .//math | .//link | .//meta | .//base |",
      ".//noscript | .//noembed | .//noframes | .//template | .//xmp |",
      ".//plaintext | .//textarea | .//title | .//select | .//frame |",
      ".//frameset | .//applet"
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
