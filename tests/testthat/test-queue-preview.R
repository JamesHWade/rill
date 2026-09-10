testthat::test_that("preview destinations reject non-public IPv4 and IPv6 literals", {
  blocked <- c(
    "127.0.0.1",
    "10.1.2.3",
    "172.16.1.1",
    "192.168.1.1",
    "169.254.169.254",
    "100.64.0.1",
    "0.0.0.0",
    "224.0.0.1",
    "240.0.0.1",
    "198.18.0.1",
    "192.0.2.1",
    "::1",
    "fc00::1",
    "fe80::1",
    "::ffff:127.0.0.1",
    "127.1",
    "2130706433",
    "0177.0.0.1"
  )
  testthat::expect_all_false(vapply(
    blocked,
    queue_preview_public_ipv4,
    logical(1)
  ))
  testthat::expect_identical(queue_preview_public_ipv4("93.184.216.34"), TRUE)
  testthat::local_mocked_bindings(queue_preview_resolve = function(host) {
    "127.0.0.1"
  })
  testthat::expect_null(queue_preview_destination(
    "https://public-looking.example/image.png"
  ))
  testthat::local_mocked_bindings(queue_preview_resolve = function(host) {
    c("93.184.216.34", "10.0.0.1")
  })
  testthat::expect_null(queue_preview_destination(
    "https://public-looking.example/image.png"
  ))
  testthat::local_mocked_bindings(queue_preview_resolve = function(host) {
    "93.184.216.34"
  })
  testthat::expect_identical(
    queue_preview_destination("https://images.example/image.png")$resolve,
    "images.example:443:93.184.216.34"
  )
  testthat::expect_null(queue_preview_destination(
    "https://images.example:8080/image.png"
  ))
})

testthat::test_that("every preview redirect is rechecked before downloading", {
  calls <- character()
  testthat::local_mocked_bindings(
    queue_preview_resolve = function(host) {
      if (host == "images.example") "93.184.216.34" else "192.168.1.2"
    },
    queue_preview_download = function(destination) {
      calls <<- c(calls, destination$resolve)
      list(
        status_code = 302L,
        headers = charToRaw(
          "HTTP/1.1 302 Found\r\nLocation: https://private.example/image.png\r\n\r\n"
        )
      )
    }
  )
  testthat::expect_null(queue_preview_fetch("https://images.example/image.png"))
  testthat::expect_identical(calls, "images.example:443:93.184.216.34")
})

testthat::test_that("preview bytes must be bounded raster data", {
  png <- c(as.raw(c(137, 80, 78, 71, 13, 10, 26, 10)), raw(20L))
  testthat::expect_identical(queue_preview_raster_type(png), "image/png")
  testthat::expect_null(queue_preview_raster_type(charToRaw(
    '<svg xmlns="http://www.w3.org/2000/svg"></svg>'
  )))
  testthat::expect_null(queue_preview_raster_type(raw(4 * 1024^2 + 1)))
  testthat::expect_null(queue_preview_raster_type(raw(20L)))
  testthat::local_mocked_bindings(
    queue_preview_resolve = function(host) "93.184.216.34",
    queue_preview_download = function(destination) {
      list(status_code = 200L, content = png)
    }
  )
  testthat::expect_identical(
    queue_preview_fetch("https://images.example/photo"),
    list(content = png, type = "image/png")
  )
})

testthat::test_that("preview endpoints authorize entries and never accept a caller URL", {
  store <- rill_store(list(demo_mode = TRUE, actor_id = "preview-reader"))
  store$memory$entries$preview_image_url[[
    1L
  ]] <- "https://images.example/photo.png"
  handler <- NULL
  session <- list(registerDataObj = function(name, data, filterFunc) {
    handler <<- filterFunc
    "session/test/dataobj/queue-preview?nonce=test"
  })
  fetches <- character()
  image <- list(
    content = c(as.raw(c(137, 80, 78, 71, 13, 10, 26, 10)), raw(20L)),
    type = "image/png"
  )
  src <- queue_preview_server(
    store,
    "preview-reader",
    session,
    fetch = function(url, session) {
      fetches <<- c(fetches, url)
      promises::promise_resolve(image)
    }
  )
  testthat::expect_match(src(as.list(store$memory$entries[1L, ])), "^session/")
  testthat::expect_identical(
    handler(
      NULL,
      list(QUERY_STRING = "entry_id=missing&url=https://private.example")
    )$status,
    404L
  )
  response <- NULL
  later::with_temp_loop({
    promises::then(
      handler(
        NULL,
        list(
          QUERY_STRING = "entry_id=sample-entry-1&url=https://private.example"
        )
      ),
      function(value) response <<- value
    )
    for (index in seq_len(20L)) {
      later::run_now(0.01)
    }
  })
  testthat::expect_identical(response$status, 200L)
  testthat::expect_identical(response$content, image$content)
  testthat::expect_identical(fetches, "https://images.example/photo.png")
  queue_preview_server(store, "other-reader", session, fetch = function(...) {
    stop("Must not fetch")
  })
  testthat::expect_identical(
    handler(NULL, list(QUERY_STRING = "entry_id=sample-entry-1"))$status,
    404L
  )
})

testthat::test_that("oversized streamed bodies abort even without a declared length", {
  testthat::local_mocked_bindings(queue_preview_stream = function(
    url,
    callback,
    handle
  ) {
    callback(raw(2 * 1024^2))
    callback(raw(2 * 1024^2))
    callback(raw(1L))
    stop("The stream must have stopped before this point")
  })
  testthat::expect_error(
    queue_preview_download(list(
      url = "https://images.example/image",
      resolve = "images.example:443:93.184.216.34"
    )),
    class = "rill_preview_too_large"
  )
})

testthat::test_that("redirects cannot rebind the original hostname to a private address", {
  resolutions <- 0L
  downloads <- 0L
  testthat::local_mocked_bindings(
    queue_preview_resolve = function(host) {
      resolutions <<- resolutions + 1L
      if (resolutions == 1L) "93.184.216.34" else "127.0.0.1"
    },
    queue_preview_download = function(destination) {
      downloads <<- downloads + 1L
      testthat::expect_identical(
        destination$resolve,
        "images.example:443:93.184.216.34"
      )
      list(
        status_code = 302L,
        headers = charToRaw("HTTP/1.1 302 Found\r\nLocation: /next.png\r\n\r\n")
      )
    }
  )
  testthat::expect_null(queue_preview_fetch("https://images.example/photo.png"))
  testthat::expect_identical(downloads, 1L)
  testthat::expect_identical(resolutions, 2L)
  testthat::expect_null(queue_preview_destination("https://[fc00::1]/image"))
  testthat::expect_null(queue_preview_destination(
    "https://[::ffff:127.0.0.1]/image"
  ))
})

testthat::test_that("closing a session prevents queued preview fetches", {
  store <- rill_store(list(demo_mode = TRUE, actor_id = "preview-reader"))
  store$memory$entries$preview_image_url[[
    1L
  ]] <- "https://images.example/photo.png"
  handler <- NULL
  close <- NULL
  session <- list(
    registerDataObj = function(name, data, filterFunc) {
      handler <<- filterFunc
      "session/test/dataobj/queue-preview?nonce=test"
    },
    onSessionEnded = function(callback) close <<- callback
  )
  queue_preview_server(store, "preview-reader", session, fetch = function(...) {
    stop("Must not fetch after close")
  })
  response <- NULL
  later::with_temp_loop({
    promises::then(
      handler(NULL, list(QUERY_STRING = "entry_id=sample-entry-1")),
      function(value) response <<- value
    )
    close()
    for (index in seq_len(20L)) {
      later::run_now(0.01)
    }
  })
  testthat::expect_identical(response$status, 404L)
  testthat::expect_identical(
    handler(NULL, list(QUERY_STRING = "entry_id=sample-entry-1"))$status,
    404L
  )
})
