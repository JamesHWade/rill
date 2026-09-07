testthat::test_that("Groups overlap without duplicating Entries or reading state", {
  for (backend in c("memory", "postgres")) {
    store <- local_orientation_backend_store(backend, "reader")
    feeds <- store_list_feeds(store, "reader")$feed_id
    first <- store_create_group(store, "reader", "First")
    second <- store_create_group(store, "reader", "Second")
    empty <- store_create_group(store, "reader", "Empty")
    store_update_group_memberships(store, "reader", feeds, character())
    store_update_group_memberships(store, "reader", feeds[1:2], first, "add")
    store_update_group_memberships(store, "reader", feeds[2:3], second, "add")
    any <- store_list_entries(store, "reader", group_ids = c(first, second))
    all <- store_list_entries(
      store,
      "reader",
      group_ids = c(first, second),
      group_match = "all"
    )
    testthat::expect_setequal(any$feed_id, feeds)
    testthat::expect_equal(anyDuplicated(any$entry_id), 0L)
    testthat::expect_setequal(all$feed_id, feeds[[2]])
    testthat::expect_equal(
      nrow(store_list_entries(store, "reader", group_ids = empty)),
      0L
    )
    testthat::expect_equal(
      nrow(store_list_entries(store, "reader", group_ids = second, limit = 1L)),
      1L
    )
    testthat::expect_equal(
      nrow(store_list_entries(
        store,
        "reader",
        group_ids = empty,
        group_match = "all"
      )),
      0L
    )
    testthat::expect_length(
      store_mark_entries_read(
        store,
        "reader",
        group_ids = empty,
        group_match = "all",
        reason = "bulk_all"
      ),
      0L
    )
    marked <- store_mark_entries_read(
      store,
      "reader",
      group_ids = c(first, second),
      group_match = "all",
      reason = "bulk_all"
    )
    testthat::expect_setequal(marked, all$entry_id)
    testthat::expect_setequal(
      store_list_entries(store, "reader", group_ids = first)$feed_id,
      feeds[[1]]
    )
    testthat::expect_setequal(
      store_list_entries(store, "reader", group_ids = second)$feed_id,
      feeds[[3]]
    )
    store_update_group_memberships(store, "reader", feeds[1:2], first, "remove")
    testthat::expect_setequal(
      store_list_entries(store, "reader", ungrouped = TRUE)$feed_id,
      feeds[[1]]
    )
    testthat::expect_equal(
      nrow(store_list_entries(
        store,
        "reader",
        group_ids = first,
        view = "all"
      )),
      0L
    )
    testthat::expect_contains(
      store_list_groups(store, "reader")$group_id,
      first
    )
    store_rename_group(store, "reader", first, "Renamed")
    testthat::expect_contains(
      store_list_groups(store, "reader")$name,
      "Renamed"
    )
    store_delete_group(store, "reader", second)
    testthat::expect_equal(
      nrow(store_list_entries(store, "reader", ungrouped = TRUE, view = "all")),
      nrow(any)
    )
    testthat::expect_setequal(
      store_list_entries(store, "reader", view = "all")$entry_id,
      any$entry_id
    )
  }
})

testthat::test_that("Group mutations are atomic and Reader scoped", {
  for (backend in c("memory", "postgres")) {
    store <- local_orientation_backend_store(backend, "reader")
    store_ensure_reader(store, "other")
    feeds <- store_list_feeds(store, "reader")$feed_id
    store_subscribe_feed(store, "other", feeds[[1]])
    group <- store_create_group(store, "reader", "Private")
    other <- store_create_group(store, "other", "Private")
    testthat::expect_identical(identical(group, other), FALSE)
    before <- store_group_memberships(store, "reader")
    testthat::expect_error(
      store_update_group_memberships(store, "reader", feeds[1:2], other),
      class = "rill_group_missing"
    )
    testthat::expect_error(
      store_update_group_memberships(
        store,
        "reader",
        c(feeds[[1]], "missing"),
        group
      ),
      class = "rill_subscription_inactive"
    )
    testthat::expect_equal(store_group_memberships(store, "reader"), before)
    testthat::expect_error(
      store_rename_group(store, "other", group, "Stolen"),
      class = "rill_group_missing"
    )
    store_delete_group(store, "other", group)
    testthat::expect_contains(
      store_list_groups(store, "reader")$group_id,
      group
    )
    store_update_group_memberships(store, "reader", feeds[[1]], group)
    testthat::expect_equal(
      nrow(store_list_entries(store, "other", group_ids = group)),
      0L
    )
    store_unsubscribe_feed(store, "reader", feeds[[1]])
    testthat::expect_equal(
      nrow(store_list_entries(store, "reader", group_ids = group)),
      0L
    )
    store_subscribe_feed(store, "reader", feeds[[1]])
    testthat::expect_setequal(
      store_list_entries(store, "reader", group_ids = group)$feed_id,
      feeds[[1]]
    )
    store_rename_group(store, "reader", group, "New name")
    fresh <- store_create_group(store, "reader", "Private")
    testthat::expect_identical(identical(fresh, group), FALSE)
  }
})

testthat::test_that("migration 013 preserves existing folders and inactive memberships", {
  store <- local_orientation_backend_store("postgres", "reader")
  store_ensure_reader(store, "other")
  feeds <- store_list_feeds(store, "reader")$feed_id
  store_subscribe_feed(
    store,
    "other",
    feeds[[1]],
    folder = "Research / Methods"
  )
  store_move_feed(store, "reader", feeds[[1]], "Research / Methods")
  store_move_feed(store, "reader", feeds[[2]], "Unsorted")
  store_unsubscribe_feed(store, "reader", feeds[[1]])
  DBI::dbExecute(store$pool, "DROP TABLE subscription_groups")
  DBI::dbExecute(store$pool, "DROP TABLE feed_groups")
  DBI::dbExecute(
    store$pool,
    "DROP FUNCTION subscription_folder_group() CASCADE"
  )
  DBI::dbExecute(
    store$pool,
    "DELETE FROM schema_migrations WHERE migration_id = '013_feed_groups'"
  )
  before <- DBI::dbGetQuery(
    store$pool,
    "SELECT reader_id, feed_id, folder, status FROM subscriptions ORDER BY reader_id, feed_id"
  )
  store_apply_schema(store)
  after <- DBI::dbGetQuery(
    store$pool,
    "SELECT reader_id, feed_id, folder, status FROM subscriptions ORDER BY reader_id, feed_id"
  )
  testthat::expect_identical(after, before)
  groups <- store_list_groups(store, "reader")
  own <- groups$group_id[groups$name == "Research / Methods"]
  other <- store_list_groups(store, "other")$group_id
  testthat::expect_length(own, 1L)
  testthat::expect_identical(identical(own, other), FALSE)
  memberships <- store_group_memberships(store, "reader")
  testthat::expect_setequal(
    memberships$feed_id[memberships$group_id == own],
    feeds[[1]]
  )
  testthat::expect_equal(
    nrow(store_list_entries(store, "reader", group_ids = own)),
    0L
  )
  store_subscribe_feed(store, "reader", feeds[[1]])
  testthat::expect_setequal(
    store_list_entries(store, "reader", group_ids = own)$feed_id,
    feeds[[1]]
  )
  store_apply_schema(store)
  testthat::expect_equal(store_group_memberships(store, "reader"), memberships)
})
