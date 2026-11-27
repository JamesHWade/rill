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

store_expire_reader_admissions <- function(store, now = utc_now()) {
  if (identical(store$mode, "postgres")) {
    expired <- DBI::dbExecute(
      store$pool,
      paste(
        "DELETE FROM reader_admission_requests ar",
        "WHERE ar.last_seen_at <= $1::timestamptz - INTERVAL '30 days'",
        "AND NOT EXISTS (",
        "SELECT 1 FROM reader_external_identities i",
        "WHERE i.issuer = ar.issuer AND i.subject = ar.subject)"
      ),
      params = list(now)
    )
    return(invisible(expired))
  }
  admissions <- store$memory$reader_admissions
  identities <- store$memory$reader_identities
  linked <- vapply(
    seq_len(nrow(admissions)),
    function(index) {
      any(
        identities$issuer == admissions$issuer[[index]] &
          identities$subject == admissions$subject[[index]]
      )
    },
    logical(1)
  )
  last_seen_at <- as.POSIXct(admissions$last_seen_at, tz = "UTC")
  expires_at <- last_seen_at + 30 * 24 * 60 * 60
  expired <- !linked &
    !is.na(expires_at) &
    expires_at <= as.POSIXct(now, tz = "UTC")
  store$memory$reader_admissions <- admissions[!expired, , drop = FALSE]
  invisible(sum(expired))
}

store_list_reader_admissions <- function(
  store,
  status = NULL,
  now = utc_now()
) {
  columns <- c(
    "issuer",
    "subject",
    "status",
    "email",
    "display_name",
    "first_seen_at",
    "last_seen_at",
    "attempt_count",
    "decided_at"
  )
  store_expire_reader_admissions(store, now)
  rows <- if (identical(store$mode, "postgres")) {
    DBI::dbGetQuery(
      store$pool,
      paste(
        "SELECT issuer, subject, status, email, display_name, first_seen_at,",
        "last_seen_at, attempt_count, decided_at",
        "FROM reader_admission_requests",
        "ORDER BY last_seen_at DESC, issuer, subject"
      )
    )
  } else {
    store$memory$reader_admissions[columns]
  }
  if (!is.null(status)) {
    rows <- rows[rows$status == status, , drop = FALSE]
  }
  rows$request_id <- vapply(
    seq_len(nrow(rows)),
    function(index) {
      rill_id(
        "reader-admission",
        rows$issuer[[index]],
        rows$subject[[index]]
      )
    },
    character(1)
  )
  rows
}

reader_admission_summary <- function(rows) {
  rows[c(
    "request_id",
    "email",
    "display_name",
    "status",
    "attempt_count",
    "first_seen_at",
    "last_seen_at",
    "decided_at"
  )]
}

store_approve_reader_admission <- function(
  store,
  request_id,
  responsible_id,
  reason,
  now = utc_now()
) {
  admissions <- store_list_reader_admissions(store)
  index <- match(request_id, admissions$request_id)
  if (is.na(index)) {
    cli::cli_abort(
      "No Reader admission matches {.val {request_id}}.",
      class = "rill_reader_admission_not_found"
    )
  }
  admission <- admissions[index, , drop = FALSE]
  identity <- store_get_reader_identity(
    store,
    admission$issuer[[1L]],
    admission$subject[[1L]]
  )
  reader_id <- if (is.null(identity)) {
    rill_id(
      "reader",
      admission$issuer[[1L]],
      admission$subject[[1L]]
    )
  } else {
    identity$reader_id
  }
  store_admit_reader_identity(
    store,
    issuer = admission$issuer[[1L]],
    subject = admission$subject[[1L]],
    reader_id = reader_id,
    responsible_id = responsible_id,
    reason = reason,
    now = now,
    require_active = TRUE
  )
  list(
    request_id = request_id,
    reader_id = reader_id,
    status = "approved",
    email = admission$email[[1L]],
    display_name = admission$display_name[[1L]]
  )
}
