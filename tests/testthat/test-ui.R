testthat::test_that("selected story cards expose their current state", {
  entry <- as.list(sample_rill_data()$entries[1, , drop = FALSE])
  entry$feed_title <- "The R Blog"
  entry$read_at <- NA_character_
  entry$starred <- FALSE

  card <- story_card(entry, index = 1L, selected = TRUE)
  html <- htmltools::renderTags(card)$html

  testthat::expect_match(html, 'aria-current="true"', fixed = TRUE)
  testthat::expect_match(
    html,
    paste0('data-entry-id="', entry$entry_id, '"'),
    fixed = TRUE
  )
  testthat::expect_match(html, "A calmer way to keep up with R", fixed = TRUE)
})

testthat::test_that("selected read stories explain why they remain in the queue", {
  entry <- as.list(sample_rill_data()$entries[1, , drop = FALSE])
  entry$feed_title <- "The R Blog"
  entry$read_at <- "2026-08-30 12:00:00 UTC"
  entry$starred <- FALSE

  card <- story_card(entry, index = 1L, selected = TRUE)
  html <- htmltools::renderTags(card)$html

  testthat::expect_match(html, "story-state", fixed = TRUE)
  testthat::expect_match(html, ">Read<", fixed = TRUE)
})

testthat::test_that("empty queues suggest the relevant next action", {
  starred <- htmltools::renderTags(empty_story_list("starred"))$html
  scoped <- htmltools::renderTags(
    empty_story_list("unread", "The R Blog")
  )$html
  today <- htmltools::renderTags(empty_story_list("today"))$html
  week <- htmltools::renderTags(empty_story_list("week"))$html
  month <- htmltools::renderTags(empty_story_list("month"))$html

  testthat::expect_match(starred, "No starred stories yet", fixed = TRUE)
  testthat::expect_match(starred, "Press F", fixed = TRUE)
  testthat::expect_match(
    scoped,
    "No unread stories from The R Blog",
    fixed = TRUE
  )
  testthat::expect_match(today, "Nothing published today", fixed = TRUE)
  testthat::expect_match(week, "Nothing published this week", fixed = TRUE)
  testthat::expect_match(month, "Nothing published this month", fixed = TRUE)
})

testthat::test_that("invalid story times render without a label", {
  testthat::expect_identical(format_story_time(NA_character_), "")
  testthat::expect_identical(format_story_time("not-a-time"), "")
})

testthat::test_that("feed management exposes OPML import and export", {
  html <- htmltools::renderTags(feed_tools_ui())$html

  testthat::expect_match(html, "Manage feeds", fixed = TRUE)
  testthat::expect_match(
    html,
    'id="feed_organization_control"',
    fixed = TRUE
  )
  testthat::expect_match(html, 'id="import_opml"', fixed = TRUE)
  testthat::expect_match(html, "Import OPML", fixed = TRUE)
  testthat::expect_match(html, 'id="export_opml"', fixed = TRUE)
  testthat::expect_match(html, "Export OPML", fixed = TRUE)

  selected <- htmltools::renderTags(feed_organization_control_ui(list(
    title = "R Weekly",
    folder = "Research",
    source_kind = "subscription"
  )))$html
  testthat::expect_match(selected, 'id="feed_folder"', fixed = TRUE)
  testthat::expect_match(selected, 'id="move_feed"', fixed = TRUE)
  testthat::expect_match(selected, 'id="unsubscribe_feed"', fixed = TRUE)

  capture <- htmltools::renderTags(feed_organization_control_ui(list(
    title = "Local captures",
    folder = "Captured",
    source_kind = "capture"
  )))$html
  testthat::expect_no_match(capture, 'id="unsubscribe_feed"', fixed = TRUE)
})

testthat::test_that("populated story output remains a scroll container", {
  styles <- paste(
    readLines(rill_package_file("app", "www", "styles.css"), warn = FALSE),
    collapse = "\n"
  )

  testthat::expect_match(
    styles,
    "div.story-list.shiny-html-output",
    fixed = TRUE
  )
  testthat::expect_match(styles, "display: block !important", fixed = TRUE)
})

testthat::test_that("the reading queue offers story sort dimensions", {
  html <- htmltools::renderTags(story_sidebar_ui())$html

  testthat::expect_match(html, 'id="story_sort"', fixed = TRUE)
  testthat::expect_match(html, 'value="newest" selected', fixed = TRUE)
  testthat::expect_match(html, 'value="oldest"', fixed = TRUE)
  testthat::expect_match(html, 'value="recently_added"', fixed = TRUE)
  testthat::expect_match(html, 'value="feed"', fixed = TRUE)
  testthat::expect_match(html, 'value="title"', fixed = TRUE)
})

testthat::test_that("navigation offers local calendar views", {
  html <- htmltools::renderTags(
    navigation_sidebar_ui(list(app_name = "Rill", demo_mode = FALSE))
  )$html

  testthat::expect_match(html, 'value="today"', fixed = TRUE)
  testthat::expect_match(html, "Today", fixed = TRUE)
  testthat::expect_match(html, 'value="week"', fixed = TRUE)
  testthat::expect_match(html, "This week", fixed = TRUE)
  testthat::expect_match(html, 'value="month"', fixed = TRUE)
  testthat::expect_match(html, "This month", fixed = TRUE)
})

testthat::test_that("the private gate offers a complete sign-out path", {
  html <- htmltools::renderTags(navigation_sidebar_ui(list(
    app_name = "Rill",
    demo_mode = FALSE,
    identity_mode = "oidc_proxy",
    oidc_client_id = "test-client",
    oidc_issuer = "https://reader.us.auth0.com/",
    oidc_logout_redirect_url = "https://reader.example/"
  )))$html

  testthat::expect_match(
    html,
    paste0(
      'href="/oauth2/sign_out?rd=https%3A%2F%2Freader.us.auth0.com',
      "%2Fv2%2Flogout%3Fclient_id%3Dtest-client%26returnTo%3D",
      'https%253A%252F%252Freader.example%252F"'
    ),
    fixed = TRUE
  )
})

testthat::test_that("the in-app Auth0 gate loads OAuth and signs out completely", {
  local_auth0_identity()
  withr::local_envvar(
    AUTH0_REDIRECT_URI = "https://rill.share.connect.posit.cloud/"
  )
  config <- rill_config()
  dependencies <- htmltools::findDependencies(rill_ui(config))
  html <- htmltools::renderTags(navigation_sidebar_ui(config))$html

  testthat::expect_contains(
    vapply(dependencies, `[[`, character(1), "name"),
    "shinyOAuth"
  )
  testthat::expect_match(
    html,
    paste0(
      'href="https://reader.us.auth0.com/v2/logout?client_id=test-client',
      '&amp;returnTo=https%3A%2F%2Frill.share.connect.posit.cloud%2F"'
    ),
    fixed = TRUE
  )
})

testthat::test_that("Orientation settings disclose their Data Destination", {
  store <- local_orientation_backend_store("memory", "reader-1")
  config <- orientation_destination_test_config()
  needs_confirmation <- orientation_destination_state(
    store,
    "reader-1",
    config
  )
  confirmation <- orientation_destination_confirmation_ui(needs_confirmation)
  confirmation_html <- htmltools::renderTags(confirmation)$html
  settings_html <- htmltools::renderTags(
    orientation_destination_settings_ui(needs_confirmation)
  )$html

  testthat::expect_match(settings_html, "OpenAI", fixed = TRUE)
  testthat::expect_match(
    settings_html,
    "https://api.openai.com/v1",
    fixed = TRUE
  )
  testthat::expect_match(settings_html, "Confirmation needed", fixed = TRUE)
  testthat::expect_match(settings_html, 'id="orientation_enable"', fixed = TRUE)
  testthat::expect_match(
    confirmation_html,
    "only bounded unread Document reading copies",
    fixed = TRUE
  )
  testthat::expect_match(
    confirmation_html,
    "selected copies disclose that those Documents are currently unread",
    fixed = TRUE
  )
  testthat::expect_match(
    confirmation_html,
    paste(
      "the rest of your Library, the Reading History event log, Reader",
      "Memory, or credentials"
    ),
    fixed = TRUE
  )
  testthat::expect_match(
    confirmation_html,
    "cannot enforce its retention, deletion, or training practices",
    fixed = TRUE
  )
  testthat::expect_match(
    confirmation_html,
    'id="orientation_confirm"',
    fixed = TRUE
  )
  testthat::expect_identical(
    confirmation$attribs$`aria-labelledby`,
    "orientation-confirmation-title"
  )
  testthat::expect_identical(
    confirmation$attribs$`data-rill-orientation-confirmation`,
    ""
  )
  testthat::expect_identical(confirmation$attribs$role, "dialog")
  dialog_roles <- gregexpr('role="dialog"', confirmation_html, fixed = TRUE)
  testthat::expect_identical(sum(dialog_roles[[1L]] > 0L), 1L)
  testthat::expect_match(
    confirmation_html,
    "https://provider.example/privacy",
    fixed = TRUE
  )
  testthat::expect_match(
    confirmation_html,
    "opens in a new tab",
    fixed = TRUE
  )

  confirmed <- confirm_orientation_destination(store, "reader-1", config)
  confirmed_html <- htmltools::renderTags(
    orientation_destination_settings_ui(confirmed)
  )$html
  testthat::expect_match(confirmed_html, "Orientation on", fixed = TRUE)
  testthat::expect_match(
    confirmed_html,
    'id="orientation_disable"',
    fixed = TRUE
  )

  missing_policy <- orientation_destination_state(
    store,
    "reader-2",
    orientation_destination_test_config(policy_url = "")
  )
  missing_policy_html <- htmltools::renderTags(
    orientation_destination_settings_ui(missing_policy)
  )$html
  testthat::expect_match(
    missing_policy_html,
    "Provider terms required",
    fixed = TRUE
  )
  testthat::expect_no_match(
    missing_policy_html,
    'id="orientation_enable"',
    fixed = TRUE
  )

  missing_endpoint <- orientation_destination_state(
    store,
    "reader-3",
    orientation_destination_test_config(
      model = "aws_bedrock/anthropic.claude",
      base_url = ""
    )
  )
  missing_endpoint_html <- htmltools::renderTags(
    orientation_destination_settings_ui(missing_endpoint)
  )$html
  testthat::expect_match(
    missing_endpoint_html,
    "Endpoint required",
    fixed = TRUE
  )
  testthat::expect_match(
    missing_endpoint_html,
    "RILL_AGENT_BASE_URL",
    fixed = TRUE
  )
  testthat::expect_no_match(
    missing_endpoint_html,
    'id="orientation_enable"',
    fixed = TRUE
  )
})

testthat::test_that("today offers a reading-copy preparation action", {
  html <- htmltools::renderTags(prepare_today_button())$html

  testthat::expect_match(html, 'id="prepare_today"', fixed = TRUE)
  testthat::expect_match(html, "Prepare", fixed = TRUE)
  testthat::expect_match(
    html,
    'aria-label="Prepare today’s reading copies"',
    fixed = TRUE
  )
})

testthat::test_that("reading status actions explain their scope", {
  html <- htmltools::renderTags(read_actions_ui("The R Blog"))$html

  testthat::expect_match(html, 'id="mark_all_read"', fixed = TRUE)
  testthat::expect_match(html, 'id="mark_older_read"', fixed = TRUE)
  testthat::expect_match(html, "The R Blog", fixed = TRUE)
  testthat::expect_match(html, "Open history stays unchanged", fixed = TRUE)
})

testthat::test_that("the reader offers an explicit unread action", {
  html <- htmltools::renderTags(mark_unread_button())$html

  testthat::expect_match(html, 'id="mark_unread"', fixed = TRUE)
  testthat::expect_match(html, "Mark unread", fixed = TRUE)
})

testthat::test_that("the reader includes source-bounded Ask Rill chat", {
  html <- htmltools::renderTags(reader_pane_ui(list(
    agent_model = "openai"
  )))$html

  testthat::expect_match(html, 'id="reader_chat"', fixed = TRUE)
  testthat::expect_match(html, "enable-cancel", fixed = TRUE)
  testthat::expect_match(html, 'id="reader_agent_status"', fixed = TRUE)
  testthat::expect_match(html, 'data-open-mobile="closed"', fixed = TRUE)
  testthat::expect_match(html, "Ask Rill about this story", fixed = TRUE)
  testthat::expect_match(
    html,
    "question and selected reading copy to OpenAI",
    fixed = TRUE
  )
})

testthat::test_that("Orientation presents a source-grounded reading path", {
  store <- local_orientation_backend_store("memory", "reader-1")
  candidates <- orientation_candidates(store, "reader-1", limit = 3L)
  boundary <- orientation_boundary(candidates)
  cards <- lapply(seq_len(2L), function(index) {
    candidate <- candidates[[index]]
    list(
      role = c("anchor", "contrast")[[index]],
      frame = c("unresolved_question", "counterpoint")[[index]],
      document_id = candidate$document$document_id,
      entry_id = candidate$entry$entry_id,
      interpretation = paste("Interpretation", index),
      why_now = paste("Why now", index),
      evidence = "Rill keeps the source feed"
    )
  })
  orientation <- new_rill_orientation(
    reader_id = "reader-1",
    boundary = boundary,
    question = "What must stay separate?",
    introduction = "Read these as one short path.",
    cards = cards,
    agent_run_id = "run-1",
    evaluated_at = as.POSIXct("2026-09-02 16:00:00", tz = "UTC")
  )

  rendered <- orientation_ui(
    orientation,
    candidates,
    processing_note = paste(
      "Produced from bounded reading copies sent to OpenAI."
    )
  )
  html <- htmltools::renderTags(rendered)$html
  query <- htmltools::tagQuery(rendered)

  testthat::expect_match(html, 'id="rill-orientation"', fixed = TRUE)
  testthat::expect_match(html, "What must stay separate?", fixed = TRUE)
  testthat::expect_match(html, "Read these as one short path", fixed = TRUE)
  testthat::expect_match(html, "Interpretation 1", fixed = TRUE)
  testthat::expect_match(html, "Why now 1", fixed = TRUE)
  testthat::expect_match(html, "Inspect Source Evidence [01]", fixed = TRUE)
  testthat::expect_match(html, "Rill keeps the source feed", fixed = TRUE)
  testthat::expect_match(
    html,
    orientation$cards[[1L]]$document_id,
    fixed = TRUE
  )
  document_codes <- query$find(
    ".orientation-evidence-metadata dd code"
  )$selectedTags()
  source_links <- query$find(
    ".orientation-evidence-metadata dd a"
  )$selectedTags()
  testthat::expect_length(document_codes, 2L)
  testthat::expect_identical(
    document_codes[[1L]]$children[[1L]],
    orientation$cards[[1L]]$document_id
  )
  testthat::expect_length(source_links, 2L)
  testthat::expect_identical(
    source_links[[1L]]$attribs$href,
    candidates[[1L]]$document$source_url
  )
  testthat::expect_identical(source_links[[1L]]$attribs$target, "_blank")
  testthat::expect_identical(
    source_links[[1L]]$attribs$rel,
    "noopener noreferrer"
  )
  testthat::expect_match(html, "Original Source", fixed = TRUE)
  testthat::expect_match(html, "Acquisition", fixed = TRUE)
  testthat::expect_match(html, "Limitations", fixed = TRUE)
  testthat::expect_match(
    html,
    "Bundled demo content cannot support real-world claims.",
    fixed = TRUE
  )
  testthat::expect_match(html, "Browse the full unread queue", fixed = TRUE)
  testthat::expect_match(html, "rillSelectEntry", fixed = TRUE)
  testthat::expect_match(html, "orientation", fixed = TRUE)
  testthat::expect_match(html, "rillDismissOrientation", fixed = TRUE)
  testthat::expect_match(html, orientation$revision_id, fixed = TRUE)
  testthat::expect_match(
    html,
    orientation$cards[[1L]]$rationale_hash,
    fixed = TRUE
  )
  testthat::expect_match(html, "Not for me", fixed = TRUE)
  testthat::expect_match(html, 'aria-live="polite"', fixed = TRUE)
  testthat::expect_match(
    html,
    "Produced from bounded reading copies sent to OpenAI",
    fixed = TRUE
  )
  testthat::expect_match(
    html,
    "No material Library changes since this evaluation.",
    fixed = TRUE
  )
  evaluated_summaries <- query$find(
    ".orientation-evaluated summary"
  )$selectedTags()
  evaluated_changes <- query$find(
    ".orientation-evaluated p"
  )$selectedTags()
  testthat::expect_length(evaluated_summaries, 1L)
  testthat::expect_match(
    evaluated_summaries[[1L]]$children[[1L]],
    "Evaluated ",
    fixed = TRUE
  )
  testthat::expect_length(evaluated_changes, 1L)
  testthat::expect_identical(
    evaluated_changes[[1L]]$children[[1L]],
    "No material Library changes since this evaluation."
  )
})

testthat::test_that("Orientation identifies material boundary changes", {
  store <- local_orientation_backend_store("memory", "reader-1")
  candidates <- orientation_candidates(store, "reader-1", limit = 3L)
  orientation <- new_rill_orientation(
    reader_id = "reader-1",
    boundary = orientation_boundary(candidates[1:2]),
    question = "What changed?",
    introduction = "Compare the current boundary.",
    cards = list(),
    agent_run_id = "run-1",
    evaluated_at = as.POSIXct("2026-09-02 16:00:00", tz = "UTC")
  )

  html <- htmltools::renderTags(
    orientation_ui(orientation, candidates[2:3])
  )$html

  testthat::expect_match(html, "1 new eligible Document", fixed = TRUE)
  testthat::expect_match(html, "1 no longer eligible", fixed = TRUE)
})

testthat::test_that("Orientation describes non-membership boundary changes", {
  boundary <- list(
    hash = "evaluated",
    document_ids = "document-1",
    candidate_count = 1L
  )
  current <- boundary
  current$hash <- "current"

  testthat::expect_identical(
    orientation_boundary_change(boundary, current),
    "Since then: The bounded evaluation inputs changed."
  )
})

testthat::test_that("Orientation keeps the last evaluation visible", {
  orientation <- list(
    boundary = list(
      hash = "evaluated",
      document_ids = "document-1",
      candidate_count = 1L
    ),
    evaluated_at = as.POSIXct("2026-09-02 16:00:00", tz = "UTC")
  )
  current <- orientation$boundary
  current$hash <- "current"

  due <- htmltools::renderTags(
    orientation_evaluated_basis(orientation, current)
  )$html
  reevaluating <- htmltools::renderTags(
    orientation_evaluated_basis(orientation, current, preparing = TRUE)
  )$html

  testthat::expect_match(due, "Update due · evaluated", fixed = TRUE)
  testthat::expect_match(
    reevaluating,
    "Reevaluating · last evaluated",
    fixed = TRUE
  )
  testthat::expect_match(
    due,
    'datetime="2026-09-02T16:00:00Z"',
    fixed = TRUE
  )
  testthat::expect_match(due, "Sep 2 at 4:00 PM UTC", fixed = TRUE)

  withr::local_timezone("America/Los_Angeles")
  pacific <- htmltools::renderTags(
    orientation_evaluated_basis(orientation, current)
  )$html
  testthat::expect_identical(pacific, due)
})

testthat::test_that("a quiet Orientation leaves the ordinary queue primary", {
  orientation <- new_rill_orientation(
    reader_id = "reader-1",
    boundary = list(
      hash = "empty-boundary",
      document_ids = character(),
      candidate_count = 0L,
      candidates = list()
    ),
    question = NULL,
    introduction = NULL,
    status = "Nothing material has cleared the threshold.",
    cards = list(),
    agent_run_id = "run-1"
  )

  html <- htmltools::renderTags(orientation_ui(orientation, list()))$html
  preparing_html <- htmltools::renderTags(
    orientation_ui(NULL, list(), preparing = TRUE)
  )$html

  testthat::expect_match(
    html,
    "Nothing material has cleared the threshold",
    fixed = TRUE
  )
  testthat::expect_match(
    html,
    "No current Orientation selection",
    fixed = TRUE
  )
  testthat::expect_match(html, "Browse unread stories", fixed = TRUE)
  testthat::expect_match(
    preparing_html,
    "Browse unread stories",
    fixed = TRUE
  )
  testthat::expect_no_match(html, 'id="rill-orientation"', fixed = TRUE)
  testthat::expect_no_match(
    preparing_html,
    'id="rill-orientation"',
    fixed = TRUE
  )
  testthat::expect_no_match(html, "orientation-step", fixed = TRUE)

  queue_html <- htmltools::renderTags(
    orientation_queue_status_ui(
      orientation,
      list(),
      processing_note = "Automatic Orientation is on."
    )
  )$html
  testthat::expect_match(
    queue_html,
    'aria-label="Orientation status"',
    fixed = TRUE
  )
  testthat::expect_match(
    queue_html,
    "Nothing material has cleared the threshold",
    fixed = TRUE
  )
  testthat::expect_match(queue_html, "evaluated ", fixed = TRUE)
  testthat::expect_no_match(queue_html, "orientation-step", fixed = TRUE)
})

testthat::test_that("the application shell uses the Rill duck mark", {
  config <- rill_config()
  ui <- rill_ui(config)
  marks <- htmltools::tagQuery(ui)$find(
    ".brand-mark .theme-logo"
  )$selectedTags()

  testthat::expect_length(marks, 2L)
  testthat::expect_setequal(
    vapply(marks, function(mark) mark$attribs$src, character(1)),
    c("rill-assets/rill-duck.png", "rill-assets/rill-duck-dark.png")
  )
  favicon <- Filter(
    \(tag) identical(tag$attribs$rel, "icon"),
    htmltools::tagQuery(ui)$find("link")$selectedTags()
  )
  testthat::expect_length(favicon, 1L)
  testthat::expect_identical(
    favicon[[1L]]$attribs$href,
    "rill-assets/rill-duck.png"
  )
})

testthat::test_that("appearance control offers system, light, and dark modes", {
  control <- appearance_control_ui()
  html <- htmltools::renderTags(control)$html

  testthat::expect_match(html, "Appearance", fixed = TRUE)
  testthat::expect_match(html, 'value="system" checked', fixed = TRUE)
  testthat::expect_match(html, 'value="light"', fixed = TRUE)
  testthat::expect_match(html, 'value="dark"', fixed = TRUE)
})

testthat::test_that("the application panes use native resizable sidebars", {
  query <- htmltools::tagQuery(rill_ui(rill_config()))
  layouts <- query$find(".bslib-sidebar-layout")$selectedTags()
  sidebars <- query$find(".bslib-sidebar-input")$selectedTags()

  testthat::expect_length(layouts, 3L)
  testthat::expect_length(sidebars, 3L)
  testthat::expect_true(all(vapply(
    sidebars,
    function(sidebar) "data-resizable" %in% names(sidebar$attribs),
    logical(1)
  )))
  testthat::expect_setequal(
    vapply(sidebars, function(sidebar) sidebar$attribs$id, character(1)),
    c("navigation_sidebar", "story_sidebar", "reader_agent_sidebar")
  )
})
