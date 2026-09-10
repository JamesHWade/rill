entry_preview_url <- function(value, base_url = NULL) {
  if (!store_scalar_string(value)) {
    return(NA_character_)
  }
  tryCatch(
    {
      value <- trimws(value)
      if (store_scalar_string(base_url)) {
        value <- xml2::url_absolute(value, base_url)
      }
      value <- validate_public_http_url(value)
      parsed <- httr2::url_parse(value)
      if (
        nzchar(parsed$username %||% "") ||
          nzchar(parsed$password %||% "")
      ) {
        return(NA_character_)
      }
      value
    },
    error = \(error) NA_character_
  )
}

entry_preview_image <- function(content, base_url, item = NULL) {
  candidates <- list()
  if (!is.null(item)) {
    nodes <- xml2::xml_find_all(
      item,
      paste0(
        ".//*[namespace-uri()='http://search.yahoo.com/mrss/' and ",
        "(local-name()='thumbnail' or ",
        "(local-name()='content' and (@medium='image' or starts-with(@type, 'image/'))))]",
        " | ./*[local-name()='enclosure' and starts-with(@type, 'image/')]",
        " | ./*[local-name()='link' and @rel='enclosure' and starts-with(@type, 'image/')]"
      )
    )
    candidates <- as.list(nodes)
  }
  if (store_scalar_string(content)) {
    html <- tryCatch(
      xml2::read_html(content),
      error = \(error) NULL
    )
    if (!is.null(html)) {
      candidates <- c(
        candidates,
        as.list(xml2::xml_find_all(html, "//img[@src]"))
      )
    }
  }
  for (node in utils::head(candidates, 30L)) {
    attributes <- xml2::xml_attrs(node)
    source <- intersect(c("src", "url", "href"), names(attributes))
    if (!length(source)) {
      next
    }
    src <- attributes[[source[[1L]]]]
    url <- entry_preview_url(src, base_url)
    if (is.na(url)) {
      next
    }
    size <- suppressWarnings(as.numeric(attributes[c("width", "height")]))
    if (
      (!is.na(size[[1L]]) && size[[1L]] < 160) ||
        (!is.na(size[[2L]]) && size[[2L]] < 90) ||
        (all(!is.na(size)) && (size[[1L]] / size[[2L]] > 4))
    ) {
      next
    }
    alt <- xml2::xml_attr(node, "alt")
    if (is.na(alt)) {
      alt <- ""
    }
    path <- httr2::url_parse(url)$path %||% ""
    if (
      grepl(
        "(^|[/_. -])(logo|avatar|icon|spacer|pixel|tracking|badge)([/_. -]|$)",
        paste(path, alt),
        ignore.case = TRUE
      )
    ) {
      next
    }
    return(list(url = url, alt = substr(alt, 1L, 300L)))
  }
  list(url = NA_character_, alt = NA_character_)
}

entry_preview_columns <- function(entries) {
  if (all(c("preview_image_url", "preview_image_alt") %in% names(entries))) {
    return(entries)
  }
  previews <- lapply(seq_len(nrow(entries)), function(index) {
    entry_preview_image(entries$feed_content[[index]], entries$url[[index]])
  })
  entries$preview_image_url <- vapply(previews, `[[`, character(1), "url")
  entries$preview_image_alt <- vapply(previews, `[[`, character(1), "alt")
  entries
}
