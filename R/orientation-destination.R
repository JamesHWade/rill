orientation_destination_record <- function(store, reader_id) {
  if (identical(store$mode, "postgres")) {
    rows <- DBI::dbGetQuery(
      store$pool,
      paste(
        "SELECT * FROM orientation_destination_settings",
        "WHERE reader_id = $1"
      ),
      params = list(reader_id)
    )
    if (!nrow(rows)) {
      return(NULL)
    }
    return(as.list(rows[1L, , drop = FALSE]))
  }
  store$memory$orientation_destination_settings[[reader_id]] %||% NULL
}

validate_orientation_destination_setting <- function(setting) {
  required <- c(
    "reader_id",
    "enabled",
    "destination_id",
    "destination_name",
    "destination_kind",
    "updated_at"
  )
  if (!is.list(setting) || any(!required %in% names(setting))) {
    cli::cli_abort(
      "The Orientation destination setting is incomplete.",
      class = "rill_orientation_destination_invalid"
    )
  }
  valid_scalar_string <- function(value) {
    is.character(value) &&
      length(value) == 1L &&
      !is.na(value) &&
      nzchar(value)
  }
  kind_valid <- valid_scalar_string(setting$destination_kind) &&
    setting$destination_kind %in% c("external", "installation")
  policy_url <- rill_agent_http_url(
    setting$policy_url %||% "",
    "policy_url",
    allow_blank = TRUE
  )
  setting$policy_url <- policy_url
  external_enabled_without_confirmation <- kind_valid &&
    identical(setting$destination_kind, "external") &&
    isTRUE(setting$enabled) &&
    (is.null(setting$confirmed_at) ||
      length(setting$confirmed_at) != 1L ||
      is.na(setting$confirmed_at))
  external_enabled_without_policy <- kind_valid &&
    identical(setting$destination_kind, "external") &&
    isTRUE(setting$enabled) &&
    is.na(policy_url)
  if (
    !valid_scalar_string(setting$reader_id) ||
      !valid_scalar_string(setting$destination_id) ||
      !valid_scalar_string(setting$destination_name) ||
      !is.logical(setting$enabled) ||
      length(setting$enabled) != 1L ||
      is.na(setting$enabled) ||
      !kind_valid ||
      external_enabled_without_confirmation ||
      external_enabled_without_policy
  ) {
    cli::cli_abort(
      "The Orientation destination setting violates its consent contract.",
      class = "rill_orientation_destination_invalid"
    )
  }
  setting
}

store_save_orientation_destination <- function(store, setting) {
  setting <- validate_orientation_destination_setting(setting)
  if (identical(store$mode, "postgres")) {
    DBI::dbExecute(
      store$pool,
      paste(
        "INSERT INTO orientation_destination_settings (",
        paste(
          "reader_id, enabled, destination_id, destination_name,",
          "destination_kind, policy_url, confirmed_at, updated_at"
        ),
        ") VALUES ($1, $2, $3, $4, $5, $6, $7, $8)",
        "ON CONFLICT (reader_id) DO UPDATE SET",
        paste(
          "enabled = EXCLUDED.enabled,",
          "destination_id = EXCLUDED.destination_id,",
          "destination_name = EXCLUDED.destination_name,",
          "destination_kind = EXCLUDED.destination_kind,",
          "policy_url = EXCLUDED.policy_url,",
          "confirmed_at = EXCLUDED.confirmed_at,",
          "updated_at = EXCLUDED.updated_at"
        )
      ),
      params = list(
        setting$reader_id,
        setting$enabled,
        setting$destination_id,
        setting$destination_name,
        setting$destination_kind,
        setting$policy_url %||% as.character(NA),
        setting$confirmed_at %||% as.POSIXct(NA, tz = "UTC"),
        setting$updated_at
      )
    )
    return(invisible(setting))
  }
  store$memory$orientation_destination_settings[[setting$reader_id]] <- setting
  invisible(setting)
}

orientation_destination_state <- function(store, reader_id, config) {
  destination <- rill_agent_data_destination_details(
    config$agent_model,
    base_url = config$agent_base_url %||% "",
    policy_url = config$agent_policy_url %||% ""
  )
  record <- orientation_destination_record(store, reader_id)
  matches <- !is.null(record) &&
    identical(record$destination_id, destination$id)
  externally_confirmed <- matches &&
    !is.null(record$confirmed_at) &&
    length(record$confirmed_at) == 1L &&
    !is.na(record$confirmed_at)
  confirmed <- identical(destination$kind, "installation") ||
    externally_confirmed
  available <- isTRUE(config$orientation_enabled) &&
    !isTRUE(config$demo_mode)
  endpoint_ready <- isTRUE(destination$endpoint_ready)
  policy_ready <- identical(destination$kind, "installation") ||
    !is.na(destination$policy_url)
  preference_enabled <- matches && isTRUE(record$enabled)

  list(
    reader_id = reader_id,
    available = available,
    enabled = available &&
      endpoint_ready &&
      policy_ready &&
      preference_enabled &&
      confirmed,
    preference_enabled = preference_enabled,
    confirmed = confirmed,
    endpoint_ready = endpoint_ready,
    policy_ready = policy_ready,
    needs_endpoint_configuration = available && !endpoint_ready,
    needs_configuration = available && (!endpoint_ready || !policy_ready),
    needs_confirmation = available &&
      identical(destination$kind, "external") &&
      endpoint_ready &&
      policy_ready &&
      !confirmed,
    confirmed_at = if (externally_confirmed) record$confirmed_at else NULL,
    destination = destination,
    demo_mode = isTRUE(config$demo_mode)
  )
}

orientation_destination_state_token <- function(state) {
  rill_id(
    "orientation-destination-state",
    state$reader_id,
    state$destination$id,
    state$enabled,
    state$preference_enabled,
    state$confirmed,
    state$endpoint_ready,
    state$policy_ready,
    state$confirmed_at %||% ""
  )
}

confirm_orientation_destination <- function(
  store,
  reader_id,
  config,
  confirmed_at = Sys.time()
) {
  state <- orientation_destination_state(store, reader_id, config)
  if (!isTRUE(state$available)) {
    cli::cli_abort(
      "Automatic Orientation is unavailable in this installation.",
      class = "rill_orientation_unavailable"
    )
  }
  destination <- state$destination
  if (!isTRUE(state$endpoint_ready)) {
    cli::cli_abort(
      paste(
        "Configure an explicit model endpoint before enabling automatic",
        "Orientation."
      ),
      class = "rill_orientation_endpoint_required"
    )
  }
  if (!isTRUE(state$policy_ready)) {
    cli::cli_abort(
      paste(
        "Configure an inspectable provider policy before enabling automatic",
        "Orientation."
      ),
      class = "rill_orientation_policy_required"
    )
  }
  now <- as.POSIXct(confirmed_at, tz = "UTC")
  store_save_orientation_destination(
    store,
    list(
      reader_id = reader_id,
      enabled = TRUE,
      destination_id = destination$id,
      destination_name = destination$name,
      destination_kind = destination$kind,
      policy_url = destination$policy_url,
      confirmed_at = if (identical(destination$kind, "external")) now else NULL,
      updated_at = now
    )
  )
  orientation_destination_state(store, reader_id, config)
}

set_orientation_enabled <- function(
  store,
  reader_id,
  enabled,
  config,
  updated_at = Sys.time()
) {
  state <- orientation_destination_state(store, reader_id, config)
  if (isTRUE(enabled) && !isTRUE(state$available)) {
    cli::cli_abort(
      "Automatic Orientation is unavailable in this installation.",
      class = "rill_orientation_unavailable"
    )
  }
  if (
    isTRUE(enabled) &&
      !isTRUE(state$endpoint_ready)
  ) {
    cli::cli_abort(
      paste(
        "Configure an explicit model endpoint before enabling automatic",
        "Orientation."
      ),
      class = "rill_orientation_endpoint_required"
    )
  }
  if (
    isTRUE(enabled) &&
      !isTRUE(state$policy_ready)
  ) {
    cli::cli_abort(
      paste(
        "Configure an inspectable provider policy before enabling automatic",
        "Orientation."
      ),
      class = "rill_orientation_policy_required"
    )
  }
  if (
    isTRUE(enabled) &&
      identical(state$destination$kind, "external") &&
      !isTRUE(state$confirmed)
  ) {
    cli::cli_abort(
      paste(
        "Confirm the external Data Destination before enabling automatic",
        "Orientation."
      ),
      class = "rill_orientation_confirmation_required"
    )
  }

  record <- orientation_destination_record(store, reader_id)
  now <- as.POSIXct(updated_at, tz = "UTC")
  if (!isTRUE(enabled) && !is.null(record)) {
    record$enabled <- FALSE
    record$updated_at <- now
    store_save_orientation_destination(store, record)
  } else {
    destination <- state$destination
    store_save_orientation_destination(
      store,
      list(
        reader_id = reader_id,
        enabled = isTRUE(enabled),
        destination_id = destination$id,
        destination_name = destination$name,
        destination_kind = destination$kind,
        policy_url = destination$policy_url,
        confirmed_at = state$confirmed_at,
        updated_at = now
      )
    )
  }
  orientation_destination_state(store, reader_id, config)
}
