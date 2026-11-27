store_ensure_reader <- function(
  store,
  reader_id,
  now = utc_now(),
  connection = NULL
) {
  if (identical(store$mode, "postgres")) {
    database <- if (is.null(connection)) store$pool else connection
    DBI::dbExecute(
      database,
      paste(
        "INSERT INTO readers (reader_id, status, created_at, updated_at)",
        "VALUES ($1, 'active', $2, $2)",
        "ON CONFLICT (reader_id) DO NOTHING"
      ),
      params = list(reader_id, now)
    )
    return(invisible(reader_id))
  }
  readers <- store$memory$readers
  if (!reader_id %in% readers$reader_id) {
    store$memory$readers <- rbind(
      readers,
      data.frame(
        reader_id = reader_id,
        status = "active",
        created_at = now,
        updated_at = now,
        disabled_at = NA_character_,
        stringsAsFactors = FALSE
      )
    )
  }
  invisible(reader_id)
}

store_resolve_reader <- function(store, reader_id) {
  if (identical(store$mode, "postgres")) {
    rows <- DBI::dbGetQuery(
      store$pool,
      "SELECT status FROM readers WHERE reader_id = $1",
      params = list(reader_id)
    )
    if (!nrow(rows)) {
      return(reader_identity_resolution(NULL, status = "missing"))
    }
    status <- rows$status[[1L]]
    return(reader_identity_resolution(
      if (identical(status, "active")) reader_id else NULL,
      status = status
    ))
  }
  readers <- store$memory$readers
  index <- match(reader_id, readers$reader_id)
  if (is.na(index)) {
    return(reader_identity_resolution(NULL, status = "missing"))
  }
  status <- readers$status[[index]]
  reader_identity_resolution(
    if (identical(status, "active")) reader_id else NULL,
    status = status
  )
}

store_bootstrap_private_reader_identity <- function(store, config) {
  now <- utc_now()
  store_ensure_reader(store, config$actor_id, now)
  for (subject in config$allowed_oidc_subjects) {
    store_admit_reader_identity(
      store,
      issuer = config$oidc_issuer,
      subject = subject,
      reader_id = config$actor_id,
      responsible_id = "configuration",
      reason = "configured private Reader allowlist",
      now = now
    )
  }
  invisible(NULL)
}

store_resolve_reader_identity <- function(store, principal, now = utc_now()) {
  if (!nzchar(principal$subject)) {
    return(reader_identity_resolution(NULL, status = "missing"))
  }
  if (identical(store$mode, "postgres")) {
    return(pool::poolWithTransaction(store$pool, function(connection) {
      store_lock_reader_identity(
        connection,
        principal$issuer,
        principal$subject
      )
      identity <- DBI::dbGetQuery(
        connection,
        paste(
          "SELECT i.reader_id, i.revoked_at, r.status",
          "FROM reader_external_identities i",
          "JOIN readers r ON r.reader_id = i.reader_id",
          "WHERE i.issuer = $1 AND i.subject = $2"
        ),
        params = list(principal$issuer, principal$subject)
      )
      if (!nrow(identity)) {
        admission <- DBI::dbGetQuery(
          connection,
          paste(
            "INSERT INTO reader_admission_requests (",
            paste(
              "issuer, subject, status, email, display_name, first_seen_at,",
              "last_seen_at, attempt_count"
            ),
            ") VALUES ($1, $2, 'pending', $3, $4, $5, $5, 1)",
            "ON CONFLICT (issuer, subject) DO UPDATE SET",
            "last_seen_at = EXCLUDED.last_seen_at,",
            paste(
              "attempt_count =",
              "reader_admission_requests.attempt_count + 1,"
            ),
            paste(
              "email = COALESCE(EXCLUDED.email,",
              "reader_admission_requests.email),"
            ),
            paste(
              "display_name = COALESCE(EXCLUDED.display_name,",
              "reader_admission_requests.display_name)"
            ),
            "RETURNING status"
          ),
          params = list(
            principal$issuer,
            principal$subject,
            principal$email %||% NA_character_,
            principal$display_name %||% NA_character_,
            now
          )
        )
        return(reader_identity_resolution(
          NULL,
          status = admission$status[[1L]]
        ))
      }
      DBI::dbExecute(
        connection,
        paste(
          "UPDATE reader_external_identities SET",
          "email = COALESCE($3, email),",
          "display_name = COALESCE($4, display_name),",
          "last_seen_at = $5, updated_at = $5",
          "WHERE issuer = $1 AND subject = $2"
        ),
        params = list(
          principal$issuer,
          principal$subject,
          principal$email %||% NA_character_,
          principal$display_name %||% NA_character_,
          now
        )
      )
      status <- if (!is.na(identity$revoked_at[[1L]])) {
        "revoked"
      } else {
        identity$status[[1L]]
      }
      reader_identity_resolution(
        if (identical(status, "active")) identity$reader_id[[1L]] else NULL,
        status = status
      )
    }))
  }
  identities <- store$memory$reader_identities
  identity_index <- which(
    identities$issuer == principal$issuer &
      identities$subject == principal$subject
  )
  if (!length(identity_index)) {
    admissions <- store$memory$reader_admissions
    admission_index <- which(
      admissions$issuer == principal$issuer &
        admissions$subject == principal$subject
    )
    if (!length(admission_index)) {
      admissions <- rbind(
        admissions,
        data.frame(
          issuer = principal$issuer,
          subject = principal$subject,
          status = "pending",
          email = principal$email %||% NA_character_,
          display_name = principal$display_name %||% NA_character_,
          first_seen_at = now,
          last_seen_at = now,
          attempt_count = 1L,
          decided_at = NA_character_,
          stringsAsFactors = FALSE
        )
      )
    } else {
      index <- admission_index[[1L]]
      admissions$last_seen_at[[index]] <- now
      admissions$attempt_count[[index]] <-
        admissions$attempt_count[[index]] + 1L
      if (!is.null(principal$email)) {
        admissions$email[[index]] <- principal$email
      }
      if (!is.null(principal$display_name)) {
        admissions$display_name[[index]] <- principal$display_name
      }
    }
    store$memory$reader_admissions <- admissions
    status <- if (length(admission_index)) {
      admissions$status[[admission_index[[1L]]]]
    } else {
      "pending"
    }
    return(reader_identity_resolution(NULL, status = status))
  }
  index <- identity_index[[1L]]
  identities$last_seen_at[[index]] <- now
  if (!is.null(principal$email)) {
    identities$email[[index]] <- principal$email
  }
  if (!is.null(principal$display_name)) {
    identities$display_name[[index]] <- principal$display_name
  }
  store$memory$reader_identities <- identities
  reader_id <- identities$reader_id[[index]]
  readers <- store$memory$readers
  reader_index <- match(reader_id, readers$reader_id)
  status <- if (!is.na(identities$revoked_at[[index]])) {
    "revoked"
  } else {
    readers$status[[reader_index]]
  }
  reader_identity_resolution(
    if (identical(status, "active")) reader_id else NULL,
    status = status
  )
}

store_lock_reader_identity <- function(connection, issuer, subject) {
  DBI::dbGetQuery(
    connection,
    paste(
      "SELECT pg_advisory_xact_lock(",
      "hashtext($1::text), hashtext($2::text))"
    ),
    params = list(issuer, subject)
  )
  invisible(NULL)
}

store_revalidate_reader_identity <- function(store, principal, reader_id) {
  if (
    is.null(principal) ||
      !nzchar(principal$issuer %||% "") ||
      !nzchar(principal$subject %||% "")
  ) {
    return(reader_identity_resolution(NULL, status = "missing"))
  }
  if (identical(store$mode, "postgres")) {
    identity <- DBI::dbGetQuery(
      store$pool,
      paste(
        "SELECT i.reader_id, i.revoked_at, r.status",
        "FROM reader_external_identities i",
        "JOIN readers r ON r.reader_id = i.reader_id",
        "WHERE i.issuer = $1 AND i.subject = $2"
      ),
      params = list(principal$issuer, principal$subject)
    )
    if (!nrow(identity)) {
      return(reader_identity_resolution(NULL, status = "missing"))
    }
    mapped_reader_id <- identity$reader_id[[1L]]
    status <- if (!identical(mapped_reader_id, reader_id)) {
      "changed"
    } else if (!is.na(identity$revoked_at[[1L]])) {
      "revoked"
    } else {
      identity$status[[1L]]
    }
    return(reader_identity_resolution(
      if (identical(status, "active")) mapped_reader_id else NULL,
      status = status
    ))
  }
  identities <- store$memory$reader_identities
  identity_index <- which(
    identities$issuer == principal$issuer &
      identities$subject == principal$subject
  )
  if (!length(identity_index)) {
    return(reader_identity_resolution(NULL, status = "missing"))
  }
  identity_index <- identity_index[[1L]]
  mapped_reader_id <- identities$reader_id[[identity_index]]
  readers <- store$memory$readers
  reader_index <- match(mapped_reader_id, readers$reader_id)
  status <- if (!identical(mapped_reader_id, reader_id)) {
    "changed"
  } else if (!is.na(identities$revoked_at[[identity_index]])) {
    "revoked"
  } else if (is.na(reader_index)) {
    "missing"
  } else {
    readers$status[[reader_index]]
  }
  reader_identity_resolution(
    if (identical(status, "active")) mapped_reader_id else NULL,
    status = status
  )
}

store_get_reader_identity <- function(store, issuer, subject) {
  if (identical(store$mode, "postgres")) {
    rows <- DBI::dbGetQuery(
      store$pool,
      paste(
        "SELECT issuer, subject, reader_id, email, display_name,",
        "created_at, last_seen_at, revoked_at",
        "FROM reader_external_identities",
        "WHERE issuer = $1 AND subject = $2"
      ),
      params = list(issuer, subject)
    )
    if (!nrow(rows)) {
      return(NULL)
    }
    return(as.list(rows[1L, , drop = FALSE]))
  }
  identities <- store$memory$reader_identities
  index <- which(identities$issuer == issuer & identities$subject == subject)
  if (!length(index)) {
    return(NULL)
  }
  as.list(identities[index[[1L]], , drop = FALSE])
}

store_admit_reader_identity <- function(
  store,
  issuer,
  subject,
  reader_id,
  responsible_id,
  reason,
  now = utc_now(),
  require_active = FALSE
) {
  if (identical(store$mode, "postgres")) {
    return(pool::poolWithTransaction(store$pool, function(connection) {
      store_lock_reader_identity(connection, issuer, subject)
      existing <- DBI::dbGetQuery(
        connection,
        paste(
          "SELECT i.reader_id, i.revoked_at, r.status",
          "FROM reader_external_identities i",
          "JOIN readers r ON r.reader_id = i.reader_id",
          "WHERE i.issuer = $1 AND i.subject = $2",
          "FOR UPDATE OF i, r"
        ),
        params = list(issuer, subject)
      )
      if (nrow(existing) && !identical(existing$reader_id[[1L]], reader_id)) {
        cli::cli_abort(
          "The external identity belongs to another Reader.",
          class = "rill_reader_identity_conflict"
        )
      }
      if (
        require_active &&
          nrow(existing) &&
          !is.na(existing$revoked_at[[1L]])
      ) {
        cli::cli_abort(
          "The Reader identity is revoked.",
          class = "rill_reader_identity_revoked"
        )
      }
      if (
        require_active &&
          nrow(existing) &&
          !identical(existing$status[[1L]], "active")
      ) {
        cli::cli_abort(
          "The Reader is not active.",
          class = "rill_reader_not_active"
        )
      }
      store_ensure_reader(store, reader_id, now, connection)
      created <- 0L
      if (!nrow(existing)) {
        admission <- DBI::dbGetQuery(
          connection,
          paste(
            "SELECT email, display_name FROM reader_admission_requests",
            "WHERE issuer = $1 AND subject = $2"
          ),
          params = list(issuer, subject)
        )
        email <- if (nrow(admission)) admission$email[[1L]] else NA_character_
        display_name <- if (nrow(admission)) {
          admission$display_name[[1L]]
        } else {
          NA_character_
        }
        created <- DBI::dbExecute(
          connection,
          paste(
            "INSERT INTO reader_external_identities (",
            paste(
              "issuer, subject, reader_id, email, display_name, created_at,",
              "updated_at"
            ),
            ") VALUES ($1, $2, $3, $4, $5, $6, $6)",
            "ON CONFLICT (issuer, subject) DO NOTHING"
          ),
          params = list(
            issuer,
            subject,
            reader_id,
            email,
            display_name,
            now
          )
        )
        existing <- DBI::dbGetQuery(
          connection,
          paste(
            "SELECT reader_id FROM reader_external_identities",
            "WHERE issuer = $1 AND subject = $2"
          ),
          params = list(issuer, subject)
        )
        if (!identical(existing$reader_id[[1L]], reader_id)) {
          cli::cli_abort(
            "The external identity belongs to another Reader.",
            class = "rill_reader_identity_conflict"
          )
        }
      }
      DBI::dbExecute(
        connection,
        paste(
          "UPDATE reader_admission_requests",
          paste(
            "SET decided_at = CASE WHEN status = 'approved'",
            "THEN decided_at ELSE $3 END,"
          ),
          "status = 'approved'",
          "WHERE issuer = $1 AND subject = $2"
        ),
        params = list(issuer, subject, now)
      )
      if (created > 0L) {
        next_event <- DBI::dbGetQuery(
          connection,
          paste(
            "SELECT nextval(pg_get_serial_sequence(",
            "'reader_identity_events', 'event_sequence'",
            "))::text AS event_sequence"
          )
        )
        event_sequence <- next_event$event_sequence[[1L]]
        DBI::dbExecute(
          connection,
          paste(
            "INSERT INTO reader_identity_events (",
            paste(
              "event_sequence, event_id, reader_id, issuer, subject, action,",
              "responsible_id, reason, happened_at"
            ),
            ") OVERRIDING SYSTEM VALUE VALUES (",
            "$1::bigint, $2, $3, $4, $5, 'identity_attached', $6, $7, $8",
            ")"
          ),
          params = list(
            event_sequence,
            rill_id(
              "reader-identity-event",
              reader_id,
              issuer,
              subject,
              "identity_attached",
              event_sequence
            ),
            reader_id,
            issuer,
            subject,
            responsible_id,
            reason,
            now
          )
        )
      }
      invisible(reader_id)
    }))
  }
  identities <- store$memory$reader_identities
  identity_index <- which(
    identities$issuer == issuer & identities$subject == subject
  )
  if (
    length(identity_index) &&
      !identical(identities$reader_id[[identity_index[[1L]]]], reader_id)
  ) {
    cli::cli_abort(
      "The external identity belongs to another Reader.",
      class = "rill_reader_identity_conflict"
    )
  }
  if (
    require_active &&
      length(identity_index) &&
      !is.na(identities$revoked_at[[identity_index[[1L]]]])
  ) {
    cli::cli_abort(
      "The Reader identity is revoked.",
      class = "rill_reader_identity_revoked"
    )
  }
  if (require_active && length(identity_index)) {
    readers <- store$memory$readers
    reader_index <- match(reader_id, readers$reader_id)
    if (
      is.na(reader_index) ||
        !identical(readers$status[[reader_index]], "active")
    ) {
      cli::cli_abort(
        "The Reader is not active.",
        class = "rill_reader_not_active"
      )
    }
  }
  store_ensure_reader(store, reader_id, now)
  admissions <- store$memory$reader_admissions
  admission_index <- which(
    admissions$issuer == issuer & admissions$subject == subject
  )
  if (!length(identity_index)) {
    email <- if (length(admission_index)) {
      admissions$email[[admission_index[[1L]]]]
    } else {
      NA_character_
    }
    display_name <- if (length(admission_index)) {
      admissions$display_name[[admission_index[[1L]]]]
    } else {
      NA_character_
    }
    store$memory$reader_identities <- rbind(
      identities,
      data.frame(
        issuer = issuer,
        subject = subject,
        reader_id = reader_id,
        email = email,
        display_name = display_name,
        created_at = now,
        last_seen_at = NA_character_,
        revoked_at = NA_character_,
        stringsAsFactors = FALSE
      )
    )
    events <- store$memory$reader_identity_events
    event_sequence <- if (nrow(events)) {
      max(events$event_sequence) + 1L
    } else {
      1L
    }
    store$memory$reader_identity_events <- rbind(
      events,
      data.frame(
        event_sequence = event_sequence,
        event_id = rill_id(
          "reader-identity-event",
          reader_id,
          issuer,
          subject,
          "identity_attached",
          event_sequence
        ),
        reader_id = reader_id,
        action = "identity_attached",
        responsible_id = responsible_id,
        reason = reason,
        happened_at = now,
        stringsAsFactors = FALSE
      )
    )
  }
  if (length(admission_index)) {
    index <- admission_index[[1L]]
    if (!identical(admissions$status[[index]], "approved")) {
      admissions$decided_at[[index]] <- now
    }
    admissions$status[[index]] <- "approved"
    store$memory$reader_admissions <- admissions
  }
  invisible(reader_id)
}

store_disable_reader <- function(
  store,
  reader_id,
  responsible_id,
  reason,
  now = utc_now()
) {
  if (identical(store$mode, "postgres")) {
    return(pool::poolWithTransaction(store$pool, function(connection) {
      updated <- DBI::dbGetQuery(
        connection,
        paste(
          "UPDATE readers SET status = 'disabled', disabled_at = $2,",
          "updated_at = $2",
          "WHERE reader_id = $1 AND status = 'active'",
          "RETURNING reader_id"
        ),
        params = list(reader_id, now)
      )
      if (!nrow(updated)) {
        existing <- DBI::dbGetQuery(
          connection,
          "SELECT status FROM readers WHERE reader_id = $1",
          params = list(reader_id)
        )
        if (!nrow(existing)) {
          cli::cli_abort(
            "Reader {.val {reader_id}} does not exist.",
            class = "rill_reader_not_found"
          )
        }
        return(invisible(FALSE))
      }
      DBI::dbExecute(
        connection,
        paste(
          "INSERT INTO reader_identity_events (",
          paste(
            "event_id, reader_id, action, responsible_id, reason,",
            "happened_at"
          ),
          ") VALUES ($1, $2, 'reader_disabled', $3, $4, $5)"
        ),
        params = list(
          rill_id(
            "reader-identity-event",
            reader_id,
            "reader_disabled",
            responsible_id,
            now
          ),
          reader_id,
          responsible_id,
          reason,
          now
        )
      )
      invisible(TRUE)
    }))
  }
  readers <- store$memory$readers
  index <- match(reader_id, readers$reader_id)
  if (is.na(index)) {
    cli::cli_abort(
      "Reader {.val {reader_id}} does not exist.",
      class = "rill_reader_not_found"
    )
  }
  if (identical(readers$status[[index]], "disabled")) {
    return(invisible(FALSE))
  }
  readers$status[[index]] <- "disabled"
  readers$disabled_at[[index]] <- now
  readers$updated_at[[index]] <- now
  store$memory$readers <- readers
  events <- store$memory$reader_identity_events
  store$memory$reader_identity_events <- rbind(
    events,
    data.frame(
      event_sequence = nrow(events) + 1L,
      event_id = rill_id(
        "reader-identity-event",
        reader_id,
        "reader_disabled",
        responsible_id,
        now
      ),
      reader_id = reader_id,
      action = "reader_disabled",
      responsible_id = responsible_id,
      reason = reason,
      happened_at = now,
      stringsAsFactors = FALSE
    )
  )
  invisible(TRUE)
}

store_list_reader_identity_events <- function(store, reader_id) {
  columns <- c("action", "responsible_id", "reason")
  if (identical(store$mode, "postgres")) {
    return(DBI::dbGetQuery(
      store$pool,
      paste(
        "SELECT action, responsible_id, reason",
        "FROM reader_identity_events",
        "WHERE reader_id = $1",
        "ORDER BY happened_at, event_sequence"
      ),
      params = list(reader_id)
    ))
  }
  events <- store$memory$reader_identity_events
  events <- events[order(events$happened_at, events$event_sequence), ]
  events <- events[events$reader_id == reader_id, columns, drop = FALSE]
  rownames(events) <- NULL
  events
}
