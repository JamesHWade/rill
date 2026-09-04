testthat::test_that("system status starts with an accessible recovery contract", {
  status <- htmltools::renderTags(rill_system_status_ui())$html
  skip <- htmltools::renderTags(rill_skip_link_ui())$html
  reader <- htmltools::renderTags(reader_pane_ui(list(
    agent_model = "openai/gpt-5.6-sol",
    agent_base_url = ""
  )))$html

  testthat::expect_match(status, 'id="rill-system-status"', fixed = TRUE)
  testthat::expect_match(status, 'role="status"', fixed = TRUE)
  testthat::expect_match(status, 'aria-live="polite"', fixed = TRUE)
  testthat::expect_match(status, "Opening Rill", fixed = TRUE)
  testthat::expect_match(status, "Reload Rill", fixed = TRUE)
  testthat::expect_match(
    skip,
    'href="#rill-primary-surface"',
    fixed = TRUE
  )
  testthat::expect_match(skip, "Skip to primary content", fixed = TRUE)
  testthat::expect_match(
    reader,
    'id="rill-primary-surface"',
    fixed = TRUE
  )
  testthat::expect_match(reader, 'tabindex="-1"', fixed = TRUE)
})

testthat::test_that("browser feedback distinguishes connection states", {
  javascript <- paste(
    readLines(rill_package_file("app", "www", "app.js"), warn = FALSE),
    collapse = "\n"
  )

  for (event in c(
    "shiny:connected",
    "shiny:disconnected",
    "shiny:busy",
    "shiny:idle",
    "shiny:recalculating",
    "shiny:recalculated"
  )) {
    testthat::expect_match(javascript, event, fixed = TRUE)
  }
  testthat::expect_match(
    javascript,
    'window.addEventListener("offline"',
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    'window.addEventListener("online"',
    fixed = TRUE
  )
  testthat::expect_match(javascript, "Your place is safe", fixed = TRUE)
  testthat::expect_match(javascript, "rillRecoverConnection", fixed = TRUE)
  testthat::expect_match(javascript, "rillUiAudit", fixed = TRUE)
  testthat::expect_match(javascript, "horizontalOverflow", fixed = TRUE)
})

testthat::test_that("native Shiny feedback receives durable semantics", {
  javascript <- paste(
    readLines(rill_package_file("app", "www", "app.js"), warn = FALSE),
    collapse = "\n"
  )

  testthat::expect_match(javascript, "enhanceNativeFeedback", fixed = TRUE)
  testthat::expect_match(javascript, ".shiny-notification-error", fixed = TRUE)
  testthat::expect_match(javascript, ".shiny-output-error", fixed = TRUE)
  testthat::expect_match(
    javascript,
    'urgent ? "alert" : "status"',
    fixed = TRUE
  )
  testthat::expect_match(javascript, "Dismiss notification", fixed = TRUE)
  testthat::expect_match(javascript, 'role", "progressbar"', fixed = TRUE)
  testthat::expect_match(javascript, "aria-busy", fixed = TRUE)
  testthat::expect_match(
    javascript,
    'popover.setAttribute("role", "dialog")',
    fixed = TRUE
  )
  testthat::expect_match(javascript, '"shown.bs.popover"', fixed = TRUE)
  testthat::expect_match(javascript, '"hidden.bs.popover"', fixed = TRUE)
  testthat::expect_match(javascript, "rill-input-validity", fixed = TRUE)
  testthat::expect_match(javascript, 'aria-invalid", "true"', fixed = TRUE)
})

testthat::test_that("system styles cover reflow and user display preferences", {
  styles <- paste(
    readLines(rill_package_file("app", "www", "styles.css"), warn = FALSE),
    collapse = "\n"
  )

  testthat::expect_match(styles, ".rill-skip-link", fixed = TRUE)
  testthat::expect_match(styles, ".rill-system-status", fixed = TRUE)
  testthat::expect_match(styles, ".shiny-notification-error", fixed = TRUE)
  testthat::expect_match(styles, ".shiny-output-error", fixed = TRUE)
  testthat::expect_match(styles, ".shiny-progress .progress", fixed = TRUE)
  testthat::expect_match(styles, ".modal-content", fixed = TRUE)
  testthat::expect_match(styles, "#shiny-notification-panel", fixed = TRUE)
  testthat::expect_match(styles, ".modal-footer .btn", fixed = TRUE)
  testthat::expect_match(
    styles,
    "@media (prefers-reduced-motion: reduce)",
    fixed = TRUE
  )
  testthat::expect_match(styles, "@media (forced-colors: active)", fixed = TRUE)
  testthat::expect_match(styles, "env(safe-area-inset-bottom)", fixed = TRUE)
})

testthat::test_that("reading typography keeps titles and separators compact", {
  styles <- paste(
    readLines(rill_package_file("app", "www", "styles.css"), warn = FALSE),
    collapse = "\n"
  )

  testthat::expect_match(
    styles,
    "clamp(34px, 3.5vw, 50px)",
    fixed = TRUE
  )
  testthat::expect_match(
    styles,
    "clamp(31px, 9vw, 40px)",
    fixed = TRUE
  )
  testthat::expect_match(styles, ":has(+ hr)", fixed = TRUE)
  testthat::expect_match(styles, ".reader-document hr", fixed = TRUE)
  testthat::expect_match(
    styles,
    "hr + :is(p, blockquote, ul, ol)",
    fixed = TRUE
  )
  testthat::expect_match(styles, "margin: 0.45em 0", fixed = TRUE)
})

testthat::test_that("core text colors meet WCAG AA contrast", {
  styles <- paste(
    readLines(rill_package_file("app", "www", "styles.css"), warn = FALSE),
    collapse = "\n"
  )
  token <- function(name, occurrence = 1L) {
    pattern <- paste0("--", name, ": #[0-9a-f]{6}")
    matches <- gregexpr(pattern, styles, perl = TRUE)
    declarations <- regmatches(styles, matches)[[1]]
    sub(".*: ", "", declarations[[occurrence]])
  }
  luminance <- function(color) {
    channels <- grDevices::col2rgb(color) / 255
    channels <- ifelse(
      channels <= 0.04045,
      channels / 12.92,
      ((channels + 0.055) / 1.055)^2.4
    )
    sum(c(0.2126, 0.7152, 0.0722) * channels)
  }
  contrast <- function(foreground, background) {
    values <- sort(c(luminance(foreground), luminance(background)))
    (values[[2]] + 0.05) / (values[[1]] + 0.05)
  }
  pairs <- list(
    c(token("ink"), token("paper")),
    c(token("muted"), token("queue")),
    c(token("sidebar-muted"), token("duck-egg")),
    c(token("on-primary"), token("green")),
    c(token("danger"), token("paper")),
    c(token("ink", 2L), token("paper", 2L)),
    c(token("muted", 2L), token("paper", 2L)),
    c(token("on-primary", 2L), token("green", 2L))
  )

  ratios <- vapply(
    pairs,
    \(pair) contrast(pair[[1]], pair[[2]]),
    numeric(1)
  )
  testthat::expect_gte(min(ratios), 4.5)
})

testthat::test_that("Escape preserves compact Reading before leaving it", {
  javascript <- paste(
    readLines(rill_package_file("app", "www", "app.js"), warn = FALSE),
    collapse = "\n"
  )
  shortcut <- strsplit(
    javascript,
    "function handleShortcut(event)",
    fixed = TRUE
  )[[1]][[2]]

  compact_reader <- regexpr(
    'compactSurface === "reader"',
    shortcut,
    fixed = TRUE
  )[[1]]
  queue_open <- regexpr("window.rillOpenQueue()", shortcut, fixed = TRUE)[[1]]
  reader_close <- regexpr("window.rillCloseReader\\(\\)", shortcut)[[1]]

  testthat::expect_match(
    javascript,
    "function handleAskRillEscape(event)",
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    'document.addEventListener("keydown", handleAskRillEscape, true)',
    fixed = TRUE
  )
  testthat::expect_gt(compact_reader, 0L)
  testthat::expect_gt(queue_open, compact_reader)
  testthat::expect_gt(reader_close, queue_open)
})

testthat::test_that("reading telemetry follows the visible reader surface", {
  javascript <- paste(
    readLines(rill_package_file("app", "www", "app.js"), warn = FALSE),
    collapse = "\n"
  )

  testthat::expect_match(
    javascript,
    "function readingTelemetryActive()",
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    "if (!readingTelemetryActive()) return;",
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    "!readingTelemetryPaused",
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    "pauseReadingTelemetryClock(true);",
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    "accumulatedReadingMs +=",
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    'send("dwell_heartbeat", { dwell_seconds: seconds })',
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    "resumeReadingTelemetryClock();",
    fixed = TRUE
  )
})

testthat::test_that("the skip link follows the visible compact surface", {
  javascript <- paste(
    readLines(rill_package_file("app", "www", "app.js"), warn = FALSE),
    collapse = "\n"
  )

  testthat::expect_match(
    javascript,
    "function handleSkipLink(event)",
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    'target.closest(".rill-skip-link")',
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    "focusCompactSurface(surface, hasReader)",
    fixed = TRUE
  )
})
