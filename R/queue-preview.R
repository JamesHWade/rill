queue_preview_public_ipv4 <- function(address) {
  if (!is.character(address) || length(address) != 1L || is.na(address)) {
    return(FALSE)
  }
  if (!grepl("^[0-9]+[.][0-9]+[.][0-9]+[.][0-9]+$", address)) {
    return(FALSE)
  }
  parts <- suppressWarnings(as.integer(strsplit(address, ".", fixed = TRUE)[[
    1L
  ]]))
  if (
    anyNA(parts) ||
      any(parts > 255L) ||
      !identical(paste(parts, collapse = "."), address)
  ) {
    return(FALSE)
  }
  first <- parts[[1L]]
  second <- parts[[2L]]
  !(first %in%
    c(0L, 10L, 127L) ||
    first >= 224L ||
    (first == 100L && second >= 64L && second <= 127L) ||
    (first == 169L && second == 254L) ||
    (first == 172L && second >= 16L && second <= 31L) ||
    (first == 192L &&
      (second == 168L ||
        (second == 0L && parts[[3L]] %in% c(0L, 2L)))) ||
    (first == 198L &&
      (second %in% c(18L, 19L) || (second == 51L && parts[[3L]] == 100L))) ||
    (first == 203L && second == 0L && parts[[3L]] == 113L))
}

queue_preview_resolve <- function(host) {
  curl::nslookup(host, ipv4_only = TRUE, multiple = TRUE)
}

queue_preview_destination <- function(url) {
  url <- entry_preview_url(url)
  if (is.na(url)) {
    return(NULL)
  }
  parsed <- httr2::url_parse(url)
  host <- tolower(parsed$hostname)
  port <- parsed$port %||%
    if (identical(parsed$scheme, "https")) "443" else "80"
  if (
    !as.character(port) %in% c("80", "443") ||
      !grepl("^[a-z0-9]([a-z0-9.-]*[a-z0-9])?$", host)
  ) {
    return(NULL)
  }
  addresses <- queue_preview_resolve(host)
  if (
    !length(addresses) ||
      !all(vapply(addresses, queue_preview_public_ipv4, logical(1)))
  ) {
    return(NULL)
  }
  list(url = url, resolve = paste(host, port, addresses[[1L]], sep = ":"))
}

queue_preview_stream <- function(url, callback, handle) {
  curl::curl_fetch_stream(url, callback, handle = handle)
}

queue_preview_download <- function(destination) {
  body <- raw()
  handle <- curl::new_handle(
    resolve = destination$resolve,
    ipresolve = 1L,
    proxy = "",
    noproxy = "*",
    followlocation = FALSE,
    timeout = 4,
    connecttimeout = 2,
    maxfilesize = 4 * 1024^2,
    useragent = rill_user_agent()
  )
  response <- queue_preview_stream(
    destination$url,
    function(chunk) {
      if (length(body) + length(chunk) > 4 * 1024^2) {
        cli::cli_abort(
          "Preview exceeds the image size limit.",
          class = "rill_preview_too_large"
        )
      }
      body <<- c(body, chunk)
      TRUE
    },
    handle = handle
  )
  response$content <- body
  response
}

queue_preview_raster_type <- function(body) {
  if (!is.raw(body) || length(body) < 12L || length(body) > 4 * 1024^2) {
    return(NULL)
  }
  if (identical(body[1:8], as.raw(c(137, 80, 78, 71, 13, 10, 26, 10)))) {
    return("image/png")
  }
  if (identical(body[1:3], as.raw(c(255, 216, 255)))) {
    return("image/jpeg")
  }
  if (
    identical(body[1:6], charToRaw("GIF87a")) ||
      identical(body[1:6], charToRaw("GIF89a"))
  ) {
    return("image/gif")
  }
  if (
    identical(body[1:4], charToRaw("RIFF")) &&
      identical(body[9:12], charToRaw("WEBP"))
  ) {
    return("image/webp")
  }
  NULL
}

queue_preview_fetch <- function(url) {
  for (attempt in seq_len(4L)) {
    destination <- queue_preview_destination(url)
    if (is.null(destination)) {
      return(NULL)
    }
    response <- queue_preview_download(destination)
    if (response$status_code %in% c(301L, 302L, 303L, 307L, 308L)) {
      location <- curl::parse_headers_list(response$headers)[["location"]]
      if (!store_scalar_string(location)) {
        return(NULL)
      }
      url <- entry_preview_url(location, destination$url)
      next
    }
    if (response$status_code != 200L) {
      return(NULL)
    }
    type <- queue_preview_raster_type(response$content)
    if (is.null(type)) {
      return(NULL)
    }
    return(list(content = response$content, type = type))
  }
  NULL
}

queue_preview_fetch_async <- function(url, session) {
  package_path <- getNamespaceInfo(asNamespace("rill"), "path")
  promises::promise(function(resolve, reject) {
    process <- callr::r_bg(
      function(url, package_path) {
        if (file.exists(file.path(package_path, "R", "queue-preview.R"))) {
          pkgload::load_all(
            package_path,
            export_all = FALSE,
            helpers = FALSE,
            quiet = TRUE
          )
        } else {
          loadNamespace("rill", lib.loc = dirname(package_path))
        }
        tryCatch(
          get("queue_preview_fetch", asNamespace("rill"))(url),
          error = function(error) NULL
        )
      },
      args = list(url, package_path),
      supervise = TRUE,
      user_profile = FALSE
    )
    started <- Sys.time()
    done <- FALSE
    remove_callback <- session$onSessionEnded(function() {
      done <<- TRUE
      process$kill()
      resolve(NULL)
    })
    poll <- function() {
      if (done) {
        return(invisible(NULL))
      }
      if (
        !process$is_alive() ||
          as.numeric(difftime(Sys.time(), started, units = "secs")) >= 8
      ) {
        done <<- TRUE
        remove_callback()
        value <- if (process$is_alive()) {
          process$kill()
          NULL
        } else {
          tryCatch(process$get_result(), error = function(error) NULL)
        }
        resolve(value)
      } else {
        later::later(poll, 0.05)
      }
    }
    poll()
  })
}

queue_preview_response <- function(image = NULL) {
  structure(
    list(
      status = if (is.null(image)) 404L else 200L,
      content_type = if (is.null(image)) "text/plain" else image$type,
      content = if (is.null(image)) "Preview unavailable" else image$content,
      headers = list(
        "Cache-Control" = "private, no-store",
        "X-Content-Type-Options" = "nosniff",
        "Content-Security-Policy" = "default-src 'none'",
        "Cross-Origin-Resource-Policy" = "same-origin"
      )
    ),
    class = "httpResponse"
  )
}

queue_preview_server <- function(
  store,
  reader_id,
  session,
  fetch = queue_preview_fetch_async
) {
  if (!is.function(session$registerDataObj)) {
    return(function(entry) NULL)
  }
  cache <- list()
  pending <- list()
  closed <- FALSE
  if (is.function(session$onSessionEnded)) {
    session$onSessionEnded(function() {
      closed <<- TRUE
      cache <<- list()
    })
  }
  tail <- promises::promise_resolve(NULL)
  authorized_url <- function(id) {
    tryCatch(
      {
        entry <- store_get_entry(store, reader_id, id)
        url <- entry_preview_url(entry$preview_image_url)
        if (is.na(url)) NULL else url
      },
      error = function(error) NULL
    )
  }
  base <- session$registerDataObj("queue-preview", NULL, function(data, req) {
    if (closed) {
      return(queue_preview_response())
    }
    query <- shiny::parseQueryString(req$QUERY_STRING %||% "")
    id <- query$entry_id
    if (!store_scalar_string(id) || nchar(id) > 200L) {
      return(queue_preview_response())
    }
    url <- authorized_url(id)
    if (is.null(url)) {
      return(queue_preview_response())
    }
    key <- digest::digest(url)
    if (!is.null(cache[[key]])) {
      return(queue_preview_response(cache[[key]]))
    }
    if (is.null(pending[[key]])) {
      work <- promises::then(tail, function(value) {
        if (closed || !identical(authorized_url(id), url)) {
          return(NULL)
        }
        fetch(url, session)
      })
      tail <<- promises::then(
        work,
        function(value) NULL,
        onRejected = function(error) NULL
      )
      pending[[key]] <<- promises::then(
        work,
        function(image) {
          pending[[key]] <<- NULL
          if (!closed && !is.null(image)) {
            cache[[key]] <<- image
            cache <<- utils::tail(cache, 4L)
          }
          image
        },
        onRejected = function(error) {
          pending[[key]] <<- NULL
          NULL
        }
      )
    }
    promises::then(pending[[key]], function(image) {
      if (closed || !identical(authorized_url(id), url)) {
        return(queue_preview_response())
      }
      queue_preview_response(image)
    })
  })
  function(entry) {
    if (
      is.na(entry_preview_url(entry$preview_image_url)) ||
        !store_scalar_string(base)
    ) {
      return(NULL)
    }
    paste0(
      base,
      "&entry_id=",
      utils::URLencode(entry$entry_id, reserved = TRUE)
    )
  }
}
