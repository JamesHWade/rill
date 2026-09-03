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

identity_proxy_principal <- function(request, config) {
  subject <- request$HTTP_X_FORWARDED_USER %||% ""
  if (!is.character(subject) || length(subject) != 1L || is.na(subject)) {
    subject <- ""
  }
  structure(
    list(
      issuer = config$oidc_issuer,
      subject = trimws(subject)
    ),
    class = "rill_identity"
  )
}

identity_principal_is_allowed <- function(principal, config) {
  nzchar(principal$issuer) &&
    principal$subject %in% config$allowed_oidc_subjects
}

identity_http_handler <- function(base_handler, config) {
  force(base_handler)
  force(config)

  if (!identical(config$identity_mode, "oidc_proxy")) {
    return(base_handler)
  }

  function(request) {
    if (identical(request$PATH_INFO %||% "", "/_rill/health")) {
      return(identity_health_response())
    }
    principal <- identity_proxy_principal(request, config)
    if (!identity_principal_is_allowed(principal, config)) {
      return(identity_denied_response(config))
    }
    base_handler(request)
  }
}

identity_server_handler <- function(base_server, config) {
  force(base_server)
  force(config)

  if (!identical(config$identity_mode, "oidc_proxy")) {
    return(base_server)
  }

  function(input, output, session) {
    principal <- identity_proxy_principal(session$request, config)
    if (!identity_principal_is_allowed(principal, config)) {
      session$close()
      return(invisible(NULL))
    }
    base_server(input, output, session)
  }
}

identity_sign_out_href <- function(config) {
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
  if (!identical(config$identity_mode, "oidc_proxy")) {
    return(NULL)
  }
  shiny::tags$a(
    class = "identity-sign-out",
    href = identity_sign_out_href(config),
    bsicons::bs_icon("box-arrow-right"),
    "Sign out"
  )
}
