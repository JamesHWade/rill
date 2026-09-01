testthat::test_that("the memory store tracks reading state", {
  store <- rill_store(list(demo_mode = TRUE))
  actor_id <- "test-reader"
  entry_id <- store$memory$entries$entry_id[[1]]

  unread <- store_list_entries(store, actor_id, view = "unread")
  testthat::expect_equal(nrow(unread), 6L)

  store_mark_opened(store, actor_id, entry_id)
  unread <- store_list_entries(store, actor_id, view = "unread")
  testthat::expect_equal(nrow(unread), 5L)

  testthat::expect_identical(
    store_toggle_state(store, actor_id, entry_id, "starred"),
    TRUE
  )
  starred <- store_list_entries(store, actor_id, view = "starred")
  testthat::expect_identical(starred$entry_id, entry_id)
})

testthat::test_that("marking unread preserves evidence that a story was opened", {
  store <- rill_store(list(demo_mode = TRUE))
  actor_id <- "test-reader"
  entry_id <- store$memory$entries$entry_id[[1]]

  store_mark_opened(store, actor_id, entry_id)
  state <- store$memory$state[store$memory$state$entry_id == entry_id, ]
  opened_at <- state$last_opened_at[[1]]

  changed <- store_mark_unread(store, actor_id, entry_id)
  state <- store$memory$state[store$memory$state$entry_id == entry_id, ]

  testthat::expect_identical(changed, TRUE)
  testthat::expect_identical(state$read_at, NA_character_)
  testthat::expect_identical(state$read_reason, NA_character_)
  testthat::expect_identical(state$last_opened_at, opened_at)
  testthat::expect_in(
    entry_id,
    store_list_entries(store, actor_id, view = "unread")$entry_id
  )
})

testthat::test_that("bulk read state records its reason and respects scope", {
  store <- rill_store(list(demo_mode = TRUE))
  actor_id <- "test-reader"
  feed_id <- store$memory$feeds$feed_id[[1]]
  feed_entries <- store$memory$entries$entry_id[
    store$memory$entries$feed_id == feed_id
  ]
  recent_id <- feed_entries[[1]]
  old_id <- feed_entries[[2]]
  now <- as.POSIXct("2026-08-31 12:00:00", tz = "UTC")
  store$memory$entries$published_at[
    store$memory$entries$entry_id == recent_id
  ] <- "2026-08-31 11:00:00 UTC"
  store$memory$entries$published_at[
    store$memory$entries$entry_id == old_id
  ] <- "2026-08-29 11:00:00 UTC"

  store_mark_opened(store, actor_id, old_id)
  store_mark_unread(store, actor_id, old_id)
  opened_at <- store$memory$state$last_opened_at[
    store$memory$state$entry_id == old_id
  ]

  marked_old <- store_mark_entries_read(
    store,
    actor_id,
    feed_id = feed_id,
    before = now - 24 * 60 * 60,
    reason = "bulk_older_than_day"
  )
  state <- store$memory$state[store$memory$state$entry_id == old_id, ]

  testthat::expect_identical(marked_old, old_id)
  testthat::expect_identical(state$read_reason, "bulk_older_than_day")
  testthat::expect_identical(state$last_opened_at, opened_at)

  marked_all <- store_mark_entries_read(
    store,
    actor_id,
    feed_id = feed_id,
    reason = "bulk_all"
  )
  testthat::expect_identical(marked_all, recent_id)
  testthat::expect_equal(
    nrow(store_list_entries(store, actor_id, view = "unread")),
    4L
  )
})

testthat::test_that("reader feed labels survive source refreshes", {
  store <- rill_store(list(demo_mode = TRUE))
  actor_id <- "test-reader"
  feed_id <- store$memory$feeds$feed_id[[1]]
  refreshed_feed <- as.list(store$memory$feeds[1, , drop = FALSE])

  store_rename_feed(store, actor_id, feed_id, "R news")
  refreshed_feed$title <- "The upstream R blog"
  store_upsert_feed(store, refreshed_feed)

  feed <- store_list_feeds(store, actor_id)
  feed <- feed[feed$feed_id == feed_id, , drop = FALSE]
  testthat::expect_identical(feed$title, "R news")
  testthat::expect_identical(feed$source_title, "The upstream R blog")
  testthat::expect_identical(feed$display_title, "R news")

  entries <- store_list_entries(store, actor_id, view = "all")
  entries <- entries[entries$feed_id == feed_id, , drop = FALSE]
  testthat::expect_identical(unique(entries$feed_title), "R news")
  testthat::expect_identical(
    unique(entries$source_feed_title),
    "The upstream R blog"
  )

  store_rename_feed(store, actor_id, feed_id, "The upstream R blog")
  restored <- store_list_feeds(store, actor_id)
  restored <- restored[restored$feed_id == feed_id, , drop = FALSE]
  testthat::expect_identical(restored$title, "The upstream R blog")
  testthat::expect_identical(restored$display_title, NA_character_)

  other_reader <- store_list_feeds(store, "other-reader")
  other_reader <- other_reader[other_reader$feed_id == feed_id, , drop = FALSE]
  testthat::expect_identical(other_reader$title, "The upstream R blog")
})

testthat::test_that("the memory store sorts stories across queue dimensions", {
  store <- rill_store(list(demo_mode = TRUE))
  store$memory$entries$published_at <- sprintf(
    "2026-08-%02d 12:00:00 UTC",
    30:25
  )
  store$memory$entries$inserted_at <- sprintf(
    "2026-08-%02d 12:00:00 UTC",
    25:30
  )

  newest <- store_list_entries(store, "reader", view = "all")
  oldest <- store_list_entries(
    store,
    "reader",
    view = "all",
    sort = "oldest"
  )
  recently_added <- store_list_entries(
    store,
    "reader",
    view = "all",
    sort = "recently_added"
  )
  by_feed <- store_list_entries(
    store,
    "reader",
    view = "all",
    sort = "feed"
  )
  by_title <- store_list_entries(
    store,
    "reader",
    view = "all",
    sort = "title"
  )

  testthat::expect_identical(
    newest$entry_id,
    paste0("sample-entry-", 1:6)
  )
  testthat::expect_identical(
    oldest$entry_id,
    paste0("sample-entry-", 6:1)
  )
  testthat::expect_identical(
    recently_added$entry_id,
    paste0("sample-entry-", 6:1)
  )
  testthat::expect_identical(
    tolower(by_feed$feed_title),
    sort(tolower(by_feed$feed_title))
  )
  testthat::expect_identical(
    tolower(by_title$title),
    sort(tolower(by_title$title))
  )
})

testthat::test_that("unknown story sorts fall back to newest", {
  store <- rill_store(list(demo_mode = TRUE))

  newest <- store_list_entries(store, "reader", view = "all")
  unknown <- store_list_entries(
    store,
    "reader",
    view = "all",
    sort = "not-a-sort"
  )

  testthat::expect_identical(unknown$entry_id, newest$entry_id)
})

testthat::test_that("alphabetical SQL sorts use deterministic collation", {
  testthat::expect_match(entry_sort_sql("feed"), 'COLLATE "C"', fixed = TRUE)
  testthat::expect_match(entry_sort_sql("title"), 'COLLATE "C"', fixed = TRUE)
})

testthat::test_that("calendar views use local calendar boundaries", {
  now <- as.POSIXct("2026-08-19 14:30:00", tz = "America/Detroit")
  today <- entry_view_window("today", now, "America/Detroit")
  week <- entry_view_window("week", now, "America/Detroit")
  month <- entry_view_window("month", now, "America/Detroit")

  testthat::expect_identical(
    format(
      today$since,
      tz = "America/Detroit"
    ),
    "2026-08-19"
  )
  testthat::expect_identical(
    format(
      week$since,
      tz = "America/Detroit"
    ),
    "2026-08-17"
  )
  testthat::expect_identical(
    format(
      month$since,
      tz = "America/Detroit"
    ),
    "2026-08-01"
  )
  testthat::expect_identical(
    format(today$before, tz = "America/Detroit"),
    "2026-08-20"
  )
  testthat::expect_identical(
    format(week$before, tz = "America/Detroit"),
    "2026-08-24"
  )
  testthat::expect_identical(
    format(month$before, tz = "America/Detroit"),
    "2026-09-01"
  )
  testthat::expect_null(entry_view_since("all", now, "America/Detroit"))
})

testthat::test_that("the memory store filters local calendar views", {
  store <- rill_store(list(demo_mode = TRUE))
  future <- store$memory$entries[6, , drop = FALSE]
  future$entry_id <- "future-entry"
  future$external_id <- "future-entry"
  store$memory$entries <- rbind(store$memory$entries, future)
  store$memory$entries$published_at <- c(
    "2026-08-19 09:00:00 UTC",
    "2026-08-18 09:00:00 UTC",
    "2026-08-16 09:00:00 UTC",
    "2026-08-01 09:00:00 UTC",
    "2026-07-31 09:00:00 UTC",
    NA_character_,
    "2026-09-01 09:00:00 UTC"
  )
  store$memory$entries$inserted_at <- c(
    rep("2026-07-01 09:00:00 UTC", 5),
    "2026-08-19 08:00:00 UTC",
    "2026-07-01 09:00:00 UTC"
  )
  now <- as.POSIXct("2026-08-19 12:00:00", tz = "UTC")

  today <- store_list_entries(
    store,
    "reader",
    view = "today",
    now = now,
    timezone = "UTC"
  )
  week <- store_list_entries(
    store,
    "reader",
    view = "week",
    now = now,
    timezone = "UTC"
  )
  month <- store_list_entries(
    store,
    "reader",
    view = "month",
    now = now,
    timezone = "UTC"
  )

  testthat::expect_setequal(today$entry_id, paste0("sample-entry-", c(1, 6)))
  testthat::expect_setequal(week$entry_id, paste0("sample-entry-", c(1, 2, 6)))
  testthat::expect_setequal(
    month$entry_id,
    paste0("sample-entry-", c(1, 2, 3, 4, 6))
  )
})

testthat::test_that("PostgreSQL URLs become explicit connection arguments", {
  args <- postgres_connection_args(paste0(
    "postgresql://reader:p%40ss@db.example:5433/rill?",
    "sslmode=require&channel_binding=require"
  ))

  testthat::expect_identical(args$dbname, "rill")
  testthat::expect_identical(args$host, "db.example")
  testthat::expect_identical(args$port, "5433")
  testthat::expect_identical(args$user, "reader")
  testthat::expect_identical(args$password, "p@ss")
  testthat::expect_identical(args$sslmode, "require")
  testthat::expect_identical(args$channel_binding, "require")
})

testthat::test_that("state fields are validated", {
  store <- rill_store(list(demo_mode = TRUE))

  testthat::expect_snapshot(
    store_toggle_state(store, "reader", "entry", "archived"),
    error = TRUE
  )
})

testthat::test_that("the schema has an immutable document head boundary", {
  schema <- paste(
    readLines(rill_package_file("sql", "001_init.sql"), warn = FALSE),
    collapse = "\n"
  )

  testthat::expect_match(
    schema,
    "CREATE TABLE IF NOT EXISTS documents",
    fixed = TRUE
  )
  testthat::expect_match(
    schema,
    "CREATE TABLE IF NOT EXISTS entry_document_heads",
    fixed = TRUE
  )
  testthat::expect_match(schema, "record_hash text NOT NULL", fixed = TRUE)
  testthat::expect_match(
    schema,
    "CREATE TABLE IF NOT EXISTS subscription_preferences",
    fixed = TRUE
  )
  testthat::expect_match(schema, "display_title text", fixed = TRUE)
  testthat::expect_match(schema, "read_reason text", fixed = TRUE)
})
