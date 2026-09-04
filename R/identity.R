identity_denied_response <- function(config) {
  sign_out_href <- htmltools::htmlEscape(identity_sign_out_href(config))
  content <- paste0(
    "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\">",
    "<meta name=\"viewport\" content=\"width=device-width\">",
    "<title>Access denied - Rill</title></head><body>",
    "<main><h1>Access denied</h1>",
    "<p>This identity is not permitted to open this Rill Library.</p>",
    "<p>Contact the Rill operator if access was expected.</p>",
    "<p><a href=\"",
    sign_out_href,
    "\">",
    "Sign out and try another identity</a></p></main>",
    "</body></html>"
  )
  structure(
    list(
      status = 403L,
      content_type = "text/html; charset=UTF-8",
      content = content,
      headers = list(
        "Cache-Control" = "no-store",
        "Content-Security-Policy" = "default-src 'none'; style-src 'unsafe-inline'",
        "X-Content-Type-Options" = "nosniff"
      )
    ),
    class = "httpResponse"
  )
}

identity_health_response <- function() {
  structure(
    list(
      status = 200L,
      content_type = "text/plain; charset=UTF-8",
      content = "ok\n",
      headers = list("Cache-Control" = "no-store")
    ),
    class = "httpResponse"
  )
}

reader_identity_adapter <- function(config, store) {
  force(store)
  if (identical(config$identity_mode, "local")) {
    reader_id <- config$actor_id
    store_ensure_reader(store, reader_id)
    return(new_reader_identity_adapter(
      "local",
      config,
      store,
      resolve = \(request) store_resolve_reader(store, reader_id)
    ))
  }
  if (identical(config$identity_mode, "oidc_proxy")) {
    store_bootstrap_private_reader_identity(store, config)
    return(new_reader_identity_adapter(
      "oidc_proxy",
      config,
      store,
      resolve = function(request) {
        principal <- identity_proxy_principal(request, config)
        resolution <- store_resolve_reader_identity(store, principal)
        resolution$session_identity <- principal[c("issuer", "subject")]
        resolution
      },
      session_status = function(resolution) {
        store_revalidate_reader_identity(
          store,
          resolution$session_identity,
          resolution$reader_id
        )
      }
    ))
  }
  if (identical(config$identity_mode, "auth0")) {
    store_bootstrap_private_reader_identity(store, config)
    return(new_reader_identity_adapter(
      "auth0",
      config,
      store,
      resolve = function(token) {
        principal <- identity_auth0_principal(token, config)
        if (!principal$subject %in% config$allowed_oidc_subjects) {
          return(reader_identity_resolution(NULL, status = "missing"))
        }
        resolution <- store_resolve_reader_identity(store, principal)
        resolution$session_identity <- principal[c("issuer", "subject")]
        resolution
      },
      session_status = function(resolution) {
        store_revalidate_reader_identity(
          store,
          resolution$session_identity,
          resolution$reader_id
        )
      },
      oauth_client = identity_auth0_client(config)
    ))
  }
  cli::cli_abort(
    "No Reader Identity adapter exists for {.val {config$identity_mode}}.",
    class = "rill_identity_config_invalid"
  )
}

new_reader_identity_adapter <- function(
  kind,
  config,
  store,
  resolve,
  session_status = function(resolution) {
    store_resolve_reader(store, resolution$reader_id)
  },
  oauth_client = NULL
) {
  force(store)
  structure(
    list(
      kind = kind,
      config = config,
      resolve = resolve,
      session_status = session_status,
      oauth_client = oauth_client
    ),
    class = "rill_reader_identity_adapter"
  )
}

identity_oidc_provider <- function(...) {
  shinyOAuth::oauth_provider_oidc_discover(...)
}

identity_auth0_provider <- function(issuer) {
  identity_oidc_provider(
    issuer = issuer,
    name = "auth0"
  )
}

identity_oauth_client <- function(...) {
  shinyOAuth::oauth_client(...)
}

identity_oauth_module_server <- function(...) {
  shinyOAuth::oauth_module_server(...)
}

identity_auth0_client <- function(config) {
  identity_oauth_client(
    provider = identity_auth0_provider(config$oidc_issuer),
    client_id = config$auth0_client_id,
    client_secret = config$auth0_client_secret,
    redirect_uri = config$auth0_redirect_uri,
    scopes = c("openid", "profile", "email")
  )
}

identity_auth0_principal <- function(token, config) {
  userinfo <- tryCatch(token@userinfo, error = \(error) list())
  structure(
    list(
      issuer = config$oidc_issuer,
      subject = identity_claim_value(userinfo$sub) %||% "",
      email = identity_claim_value(userinfo$email),
      display_name = identity_claim_value(
        userinfo$name %||% userinfo$nickname
      )
    ),
    class = "rill_identity"
  )
}

reader_identity_resolution <- function(reader_id, status = "active") {
  structure(
    list(status = status, reader_id = reader_id),
    class = "rill_reader_identity_resolution"
  )
}

reader_identity_resolve <- function(adapter, request) {
  adapter$resolve(request)
}

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

identity_claim_value <- function(value) {
  if (
    !is.character(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(trimws(value))
  ) {
    return(NULL)
  }
  trimws(value)
}

identity_proxy_principal <- function(request, config) {
  if (!identity_proxy_request_is_trusted(request)) {
    return(structure(
      list(issuer = config$oidc_issuer, subject = ""),
      class = "rill_identity"
    ))
  }
  subject <- identity_claim_value(request$HTTP_X_FORWARDED_USER) %||% ""
  structure(
    list(
      issuer = config$oidc_issuer,
      subject = subject,
      email = identity_claim_value(request$HTTP_X_FORWARDED_EMAIL),
      display_name = identity_claim_value(
        request$HTTP_X_FORWARDED_PREFERRED_USERNAME
      )
    ),
    class = "rill_identity"
  )
}

identity_proxy_request_is_trusted <- function(request) {
  remote_addr <- identity_claim_value(request$REMOTE_ADDR)
  !is.null(remote_addr) &&
    remote_addr %in% c("127.0.0.1", "::1", "::ffff:127.0.0.1")
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

store_get_reader_admission <- function(store, issuer, subject) {
  if (identical(store$mode, "postgres")) {
    rows <- DBI::dbGetQuery(
      store$pool,
      paste(
        "SELECT status, email, display_name, attempt_count, decided_at",
        "FROM reader_admission_requests",
        "WHERE issuer = $1 AND subject = $2"
      ),
      params = list(issuer, subject)
    )
    if (!nrow(rows)) {
      return(NULL)
    }
    return(as.list(rows[1L, , drop = FALSE]))
  }
  admissions <- store$memory$reader_admissions
  index <- which(admissions$issuer == issuer & admissions$subject == subject)
  if (!length(index)) {
    return(NULL)
  }
  as.list(admissions[index[[1L]], , drop = FALSE])
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
  now = utc_now()
) {
  if (identical(store$mode, "postgres")) {
    return(pool::poolWithTransaction(store$pool, function(connection) {
      store_lock_reader_identity(connection, issuer, subject)
      store_ensure_reader(store, reader_id, now, connection)
      existing <- DBI::dbGetQuery(
        connection,
        paste(
          "SELECT reader_id FROM reader_external_identities",
          "WHERE issuer = $1 AND subject = $2"
        ),
        params = list(issuer, subject)
      )
      if (nrow(existing) && !identical(existing$reader_id[[1L]], reader_id)) {
        cli::cli_abort(
          "The external identity belongs to another Reader.",
          class = "rill_reader_identity_conflict"
        )
      }
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

identity_http_handler <- function(base_handler, adapter) {
  force(base_handler)
  force(adapter)

  if (!identical(adapter$kind, "oidc_proxy")) {
    return(base_handler)
  }

  function(request) {
    if (identical(request$PATH_INFO %||% "", "/_rill/health")) {
      return(identity_health_response())
    }
    resolution <- reader_identity_resolve(adapter, request)
    if (!identical(resolution$status, "active")) {
      return(identity_denied_response(adapter$config))
    }
    base_handler(request)
  }
}

identity_server_handler <- function(base_server, adapter) {
  force(base_server)
  force(adapter)

  if (identical(adapter$kind, "auth0")) {
    return(function(input, output, session) {
      auth <- identity_oauth_module_server(
        "rill_identity",
        client = adapter$oauth_client,
        auto_redirect = TRUE
      )
      started <- FALSE
      denied <- FALSE

      shiny::observe(
        {
          if (!isTRUE(auth$authenticated) || started || denied) {
            return()
          }
          resolution <- reader_identity_resolve(adapter, auth$token)
          if (!identical(resolution$status, "active")) {
            denied <<- TRUE
            identity_show_denied_modal(adapter$config)
            return()
          }
          started <<- TRUE
          reader_identity_guard_session(adapter, resolution, session)
          base_server(input, output, session, resolution$reader_id)
        },
        domain = session
      )

      shiny::observe(
        {
          if (started && !isTRUE(auth$authenticated)) {
            session$close()
          }
        },
        domain = session
      )
    })
  }

  function(input, output, session) {
    resolution <- reader_identity_resolve(adapter, session$request)
    if (!identical(resolution$status, "active")) {
      session$close()
      return(invisible(NULL))
    }
    reader_identity_guard_session(adapter, resolution, session)
    base_server(input, output, session, resolution$reader_id)
  }
}

identity_show_denied_modal <- function(config) {
  shiny::showModal(shiny::modalDialog(
    title = "Access denied",
    shiny::tags$p(
      "This identity is not permitted to open this Rill Library."
    ),
    shiny::tags$p("Contact the Rill operator if access was expected."),
    footer = shiny::tags$a(
      class = "btn btn-primary",
      href = identity_sign_out_href(config),
      "Sign out and try another identity"
    ),
    easyClose = FALSE
  ))
}

reader_identity_guard_session <- function(adapter, resolution, session) {
  if (!is.function(session$onSessionEnded)) {
    return(invisible(NULL))
  }
  guard <- shiny::observe(
    {
      shiny::invalidateLater(1000, session)
      current <- tryCatch(
        adapter$session_status(resolution),
        error = \(error) NULL
      )
      if (is.null(current) || !identical(current$status, "active")) {
        session$close()
      }
    },
    domain = session
  )
  invisible(guard)
}

identity_sign_out_href <- function(config) {
  if (identical(config$identity_mode, "auth0")) {
    return(paste0(
      "https://",
      config$auth0_domain,
      "/v2/logout?client_id=",
      utils::URLencode(config$auth0_client_id, reserved = TRUE),
      "&returnTo=",
      utils::URLencode(config$auth0_redirect_uri, reserved = TRUE)
    ))
  }
  issuer <- sub("/?$", "/", config$oidc_issuer)
  provider_logout <- paste0(
    issuer,
    "v2/logout?client_id=",
    utils::URLencode(config$oidc_client_id, reserved = TRUE),
    "&returnTo=",
    utils::URLencode(config$oidc_logout_redirect_url, reserved = TRUE)
  )
  paste0(
    "/oauth2/sign_out?rd=",
    utils::URLencode(provider_logout, reserved = TRUE, repeated = TRUE)
  )
}

identity_sign_out_ui <- function(config) {
  if (!(config$identity_mode %||% "local") %in% c("oidc_proxy", "auth0")) {
    return(NULL)
  }
  shiny::tags$a(
    class = "identity-sign-out",
    href = identity_sign_out_href(config),
    bsicons::bs_icon("box-arrow-right"),
    "Sign out"
  )
}
