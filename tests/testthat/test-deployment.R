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

testthat::test_that("hourly Feed polling is explicit and kill-switched", {
  workflow <- testthat::test_path(
    "..",
    "..",
    ".github",
    "workflows",
    "poll-feeds.yaml"
  )
  testthat::skip_if_not(
    file.exists(workflow),
    "GitHub workflow is not included in the built R package"
  )
  contents <- paste(readLines(workflow, warn = FALSE), collapse = "\n")

  testthat::expect_match(contents, 'cron: "17 * * * *"', fixed = TRUE)
  testthat::expect_match(
    contents,
    "vars.RILL_POLLING_ENABLED == 'true'",
    fixed = TRUE
  )
  testthat::expect_match(
    contents,
    "DATABASE_URL: ${{ secrets.RILL_DATABASE_URL }}",
    fixed = TRUE
  )
  testthat::expect_no_match(contents, "OPENAI_API_KEY", fixed = TRUE)
  testthat::expect_no_match(contents, "AUTH0_CLIENT_SECRET", fixed = TRUE)
})

testthat::test_that("the Connect Cloud manifest uses its supported R runtime", {
  manifest_path <- testthat::test_path("..", "..", "manifest.json")
  testthat::skip_if_not(
    file.exists(manifest_path),
    "Connect manifest is not included in the built R package"
  )
  manifest <- jsonlite::read_json(manifest_path)

  testthat::expect_identical(manifest$platform, "4.6.0")
  testthat::expect_contains(
    names(manifest$packages),
    c("deputy", "shinyOAuth")
  )
  testthat::expect_contains(names(manifest$files), "app.R")
  testthat::expect_contains(names(manifest$files), "R/identity.R")
  source_files <- setdiff(names(manifest$files), ".Rbuildignore")
  source_paths <- file.path(dirname(manifest_path), source_files)
  source_files <- source_files[file.exists(source_paths)]
  source_paths <- source_paths[file.exists(source_paths)]
  manifest_checksums <- vapply(
    manifest$files[source_files],
    `[[`,
    character(1L),
    "checksum"
  )
  source_checksums <- unname(tools::md5sum(source_paths))
  testthat::expect_identical(source_checksums, unname(manifest_checksums))
  testthat::expect_no_match(
    names(manifest$files),
    "^prototypes/",
    perl = TRUE
  )
})
