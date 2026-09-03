identity_test_request <- function(
  subject = NULL,
  email = NULL,
  display_name = NULL,
  path = "/",
  method = "GET",
  authorization = NULL,
  remote_addr = "127.0.0.1"
) {
  values <- list(
    PATH_INFO = path,
    REQUEST_METHOD = method,
    SCRIPT_NAME = "",
    QUERY_STRING = "",
    SERVER_NAME = "localhost",
    SERVER_PORT = "80",
    HTTP_HOST = "localhost",
    REMOTE_ADDR = remote_addr
  )
  if (!is.null(subject)) {
    values$HTTP_X_FORWARDED_USER <- subject
  }
  if (!is.null(email)) {
    values$HTTP_X_FORWARDED_EMAIL <- email
  }
  if (!is.null(display_name)) {
    values$HTTP_X_FORWARDED_PREFERRED_USERNAME <- display_name
  }
  if (!is.null(authorization)) {
    values$HTTP_AUTHORIZATION <- authorization
  }
  list2env(values, parent = emptyenv())
}

local_proxy_identity <- function(
  subjects = "auth0|reader",
  capture_token = NA_character_
) {
  withr::local_envvar(
    c(
      DATABASE_URL = "",
      RILL_ACTOR_ID = "private-reader",
      RILL_CAPTURE_TOKEN = capture_token,
      RILL_IDENTITY_MODE = "oidc_proxy",
      RILL_ALLOWED_OIDC_SUBJECTS = subjects,
      OAUTH2_PROXY_CLIENT_ID = "test-client",
      OAUTH2_PROXY_OIDC_ISSUER_URL = "https://reader.us.auth0.com/",
      OAUTH2_PROXY_REDIRECT_URL = "https://reader.example/oauth2/callback"
    ),
    .local_envir = parent.frame()
  )
}
