testthat::test_that("the web role forwards only the verified OIDC subject", {
  directory <- withr::local_tempdir()
  bin <- file.path(directory, "bin")
  dir.create(bin)
  proxy_arguments <- file.path(directory, "proxy-arguments")
  curl_arguments <- file.path(directory, "curl-arguments")
  writeLines(
    c(
      "#!/bin/sh",
      "trap 'exit 0' TERM INT",
      "while :; do /bin/sleep 1; done"
    ),
    file.path(bin, "Rscript")
  )
  writeLines(
    c(
      "#!/bin/sh",
      "printf '%s\\n' \"$@\" > \"$RILL_TEST_CURL_ARGUMENTS\"",
      "exit 0"
    ),
    file.path(bin, "curl")
  )
  writeLines(
    c(
      "#!/bin/sh",
      "printf '%s\\n' \"$@\" > \"$RILL_TEST_PROXY_ARGUMENTS\"",
      "exit 1"
    ),
    file.path(bin, "oauth2-proxy")
  )
  Sys.chmod(list.files(bin, full.names = TRUE), mode = "0755")
  entrypoint <- testthat::test_path("..", "..", "docker", "entrypoint.sh")
  testthat::skip_if_not(
    file.exists(entrypoint),
    "Docker entrypoint is not included in the built R package"
  )
  bash <- if (file.exists("/opt/homebrew/bin/bash")) {
    "/opt/homebrew/bin/bash"
  } else {
    Sys.which("bash")
  }
  environment <- c(
    paste0("PATH=", bin, ":/usr/bin:/bin"),
    "OAUTH2_PROXY_CLIENT_ID=test-client",
    "OAUTH2_PROXY_CLIENT_SECRET=test-secret",
    "OAUTH2_PROXY_COOKIE_SECRET=test-cookie-secret",
    "OAUTH2_PROXY_OIDC_ISSUER_URL=https://reader.us.auth0.com/",
    "OAUTH2_PROXY_REDIRECT_URL=https://reader.example/oauth2/callback",
    "RILL_ACTOR_ID=private-reader",
    "RILL_ALLOWED_OIDC_SUBJECTS=google-reader,github-reader",
    "RILL_CAPTURE_TOKEN=test-capture-token",
    paste0("RILL_TEST_CURL_ARGUMENTS=", curl_arguments),
    paste0("RILL_TEST_PROXY_ARGUMENTS=", proxy_arguments)
  )

  output <- suppressWarnings(system2(
    bash,
    c(shQuote(entrypoint), "web"),
    env = environment,
    stdout = TRUE,
    stderr = TRUE
  ))
  testthat::expect_identical(
    file.exists(proxy_arguments),
    TRUE,
    info = paste(output, collapse = "\n")
  )
  arguments <- readLines(proxy_arguments, warn = FALSE)

  required <- c(
    "--http-address=0.0.0.0:10000",
    "--upstream=http://127.0.0.1:3838",
    "--proxy-websockets=true",
    "--code-challenge-method=S256",
    "--insecure-oidc-skip-nonce=false",
    "--user-id-claim=sub",
    "--skip-auth-strip-headers=true",
    "--pass-user-headers=true",
    "--session-store-type=cookie",
    "--session-cookie-minimal=true",
    "--pass-access-token=false",
    "--pass-authorization-header=false",
    "--whitelist-domain=reader.us.auth0.com"
  )
  testthat::expect_setequal(intersect(arguments, required), required)
  testthat::expect_length(
    grep("^--backend-logout-url=", arguments, value = TRUE),
    0L
  )
  testthat::expect_in(
    "http://127.0.0.1:3838/_rill/health",
    readLines(curl_arguments, warn = FALSE)
  )
})

testthat::test_that("the web role rejects proxy configuration overrides", {
  entrypoint <- testthat::test_path("..", "..", "docker", "entrypoint.sh")
  testthat::skip_if_not(
    file.exists(entrypoint),
    "Docker entrypoint is not included in the built R package"
  )
  bash <- if (file.exists("/opt/homebrew/bin/bash")) {
    "/opt/homebrew/bin/bash"
  } else {
    Sys.which("bash")
  }

  output <- suppressWarnings(system2(
    bash,
    c(shQuote(entrypoint), "web", "--pass-access-token=true"),
    stdout = TRUE,
    stderr = TRUE
  ))

  testthat::expect_identical(attr(output, "status"), 64L)
  testthat::expect_match(
    output,
    "The web command does not accept arguments.",
    fixed = TRUE
  )
})
