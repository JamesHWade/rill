identity_denied_response <- function(config, status = "denied") {
  sign_out_href <- htmltools::htmlEscape(identity_sign_out_href(config))
  pending <- identical(status, "pending")
  content <- paste0(
    "<!DOCTYPE html><html lang=\"en\"><head><meta charset=\"utf-8\">",
    "<meta name=\"viewport\" content=\"width=device-width\">",
    "<title>",
    if (pending) "Access requested" else "Access denied",
    " - Rill</title></head><body>",
    "<main><h1>",
    if (pending) "Access requested" else "Access denied",
    "</h1>",
    if (pending) {
      paste0(
        "<p>Your sign-in worked. Rill recorded an access request for the ",
        "operator to approve.</p>",
        "<p>After approval, reload this page.</p>"
      )
    } else {
      paste0(
        "<p>This identity is not permitted to open this Rill Library.</p>",
        "<p>Contact the Rill operator if access was expected.</p>"
      )
    },
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
    name = "auth0",
    issuer_match = "host"
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
      return(identity_denied_response(adapter$config, resolution$status))
    }
    base_handler(request)
  }
}

identity_server_handler <- function(
  base_server,
  adapter,
  on_authenticated = NULL
) {
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
            identity_show_denied_modal(adapter$config, resolution$status)
            return()
          }
          started <<- TRUE
          reader_identity_guard_session(adapter, resolution, session)
          base_server(input, output, session, resolution$reader_id)
          if (is.function(on_authenticated)) {
            on_authenticated(resolution, session)
          }
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
    if (is.function(on_authenticated)) {
      on_authenticated(resolution, session)
    }
  }
}

identity_show_denied_modal <- function(config, status = "denied") {
  pending <- identical(status, "pending")
  shiny::showModal(shiny::modalDialog(
    title = if (pending) "Access requested" else "Access denied",
    shiny::tags$span(`data-rill-access-status` = status, hidden = NA),
    shiny::tags$p(
      if (pending) {
        paste(
          "Your sign-in worked. Rill recorded an access request for the",
          "operator to approve."
        )
      } else {
        "This identity is not permitted to open this Rill Library."
      }
    ),
    shiny::tags$p(
      if (pending) {
        "After approval, reload this page."
      } else {
        "Contact the Rill operator if access was expected."
      }
    ),
    footer = shiny::tags$a(
      class = "btn btn-primary",
      href = identity_sign_out_href(config),
      "Sign out and try another identity"
    ),
    easyClose = FALSE
  ))
}

reader_identity_guard_session <- function(adapter, resolution, session) {
  if (is.environment(session$userData)) {
    session$userData$rill_reader_authorize <- function() {
      current <- adapter$session_status(resolution)
      identical(current$status, "active") &&
        identical(current$reader_id, resolution$reader_id)
    }
  }
  if (!is.function(session$onSessionEnded)) {
    return(invisible(NULL))
  }
  guard <- shiny::observe(
    {
      shiny::invalidateLater(rill_session_poll_interval_ms, session)
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
