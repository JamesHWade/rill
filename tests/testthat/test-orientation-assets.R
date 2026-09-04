testthat::test_that("Orientation browser actions preserve their source surface", {
  javascript <- paste(
    readLines(rill_package_file("app", "www", "app.js"), warn = FALSE),
    collapse = "\n"
  )

  testthat::expect_match(
    javascript,
    'surface = "story_list",',
    fixed = TRUE
  )
  testthat::expect_match(javascript, "provenance = null", fixed = TRUE)
  for (field in c(
    "document_id",
    "card_id",
    "revision_id",
    "basis_hash",
    "rationale_hash",
    "orientation_id"
  )) {
    testthat::expect_match(javascript, paste0('"', field, '"'), fixed = TRUE)
  }
  testthat::expect_match(
    javascript,
    '"dismiss_orientation_card"',
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    "window.rillBrowseQueue",
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    '"browse_orientation_queue"',
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    '"rill-browse-queue-ready"',
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    "function revealOrientationQueue(_message)",
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    "window.rillShowOrientation",
    fixed = TRUE
  )
  testthat::expect_match(javascript, "pendingReaderFocus", fixed = TRUE)
  testthat::expect_match(
    javascript,
    "window.rillOpenAskRill",
    fixed = TRUE
  )
  testthat::expect_match(javascript, "Close Ask Rill", fixed = TRUE)
  testthat::expect_match(javascript, "restoreAgentFocus", fixed = TRUE)
  testthat::expect_match(javascript, "ensureAskRillSettled", fixed = TRUE)
  testthat::expect_match(javascript, "function focusReader()", fixed = TRUE)
  testthat::expect_match(
    javascript,
    "function recoverReaderFocus()",
    fixed = TRUE
  )
  testthat::expect_match(javascript, ".article-header h1", fixed = TRUE)
  testthat::expect_match(
    javascript,
    "pendingOrientationDismissal",
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    "function restoreOrientationDismissFocus()",
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    'nextCard.querySelector(".orientation-read")',
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    "focusStory(null, {",
    fixed = TRUE
  )
  testthat::expect_no_match(
    javascript,
    ".orientation-dismiss, .orientation-evidence summary",
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    "const hasOrientation = Boolean(orientation);",
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    'scrollIntoView({ block: "nearest" })',
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    "revision_id: revisionId",
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    "rationale_hash: rationaleHash",
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    "function setSurfaceCovered(element, covered)",
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    "function setSidebarCovered(sidebar, covered)",
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    "navigationHadFocus = setSidebarCovered",
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    "const queueHadFocus = setSidebarCovered(",
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    'compactSurface !== "queue"',
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    'setSurfaceCovered(readerPane, compactSurface !== "reader")',
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    'element.setAttribute("aria-hidden", "true")',
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    'element.toggleAttribute("inert", previous.inert)',
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    'element.removeAttribute("aria-hidden")',
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    'document.addEventListener("hide.bs.modal", restoreOrientationModalFocus)',
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    'document.addEventListener("hidden.bs.modal", restoreOrientationModalFocus)',
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    'document.querySelectorAll(".orientation-evaluated time[datetime]")',
    fixed = TRUE
  )
  testthat::expect_no_match(javascript, "syncAgentSidebar", fixed = TRUE)
})

testthat::test_that("Orientation participates in the compact surface router", {
  styles <- paste(
    readLines(rill_package_file("app", "www", "styles.css"), warn = FALSE),
    collapse = "\n"
  )
  compact_styles <- gsub("[[:space:]]+", " ", styles)

  testthat::expect_match(styles, ".orientation-canvas", fixed = TRUE)
  testthat::expect_match(styles, ".orientation-evidence", fixed = TRUE)
  testthat::expect_match(styles, ".orientation-return", fixed = TRUE)
  testthat::expect_match(
    styles,
    ".app-shell:not(.has-reader) .reader-pane > .bslib-sidebar-layout",
    fixed = TRUE
  )
  testthat::expect_match(
    styles,
    "grid-template-columns: minmax(0, 1fr) 0 !important",
    fixed = TRUE
  )
  testthat::expect_match(
    styles,
    '.app-shell[data-compact-surface="queue"] .story-pane',
    fixed = TRUE
  )
  testthat::expect_match(
    compact_styles,
    paste(
      '.app-shell[data-compact-surface="reader"]',
      ".reader-pane {"
    ),
    fixed = TRUE
  )
  testthat::expect_match(
    compact_styles,
    paste(
      ".reading-shell > .bslib-sidebar-layout",
      "> .bslib-sidebar-resize-handle"
    ),
    fixed = TRUE
  )
})

testthat::test_that("compact surfaces retain reader state across navigation", {
  javascript <- paste(
    readLines(rill_package_file("app", "www", "app.js"), warn = FALSE),
    collapse = "\n"
  )
  styles <- paste(
    readLines(rill_package_file("app", "www", "styles.css"), warn = FALSE),
    collapse = "\n"
  )

  testthat::expect_match(
    javascript,
    'window.matchMedia("(max-width: 767.98px)")',
    fixed = TRUE
  )
  testthat::expect_match(javascript, "window.rillOpenLibrary", fixed = TRUE)
  testthat::expect_match(javascript, "window.rillCloseLibrary", fixed = TRUE)
  testthat::expect_match(javascript, "window.rillOpenQueue", fixed = TRUE)
  testthat::expect_match(
    javascript,
    "window.rillReturnToReading",
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    'shell.dataset.compactSurface = compactSurface',
    fixed = TRUE
  )
  testthat::expect_match(styles, "@media (max-width: 767.98px)", fixed = TRUE)
  testthat::expect_match(styles, "min-height: 44px", fixed = TRUE)
})

testthat::test_that("Ask Rill overlays Reading until both panes fit", {
  javascript <- paste(
    readLines(rill_package_file("app", "www", "app.js"), warn = FALSE),
    collapse = "\n"
  )
  styles <- paste(
    readLines(rill_package_file("app", "www", "styles.css"), warn = FALSE),
    collapse = "\n"
  )

  testthat::expect_match(
    javascript,
    'window.matchMedia("(max-width: 1499.98px)")',
    fixed = TRUE
  )
  testthat::expect_match(
    javascript,
    "setSurfaceCovered(main, expanded && overlaidAgentMode.matches)",
    fixed = TRUE
  )
  testthat::expect_match(
    styles,
    "@media (min-width: 576px) and (max-width: 1499.98px)",
    fixed = TRUE
  )
  testthat::expect_match(styles, "position: absolute", fixed = TRUE)
})
