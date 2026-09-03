sample_rill_data <- function() {
  feeds <- data.frame(
    feed_id = c("sample-r-project", "sample-posit", "sample-rweekly"),
    feed_url = c(
      "https://blog.r-project.org/feed.xml",
      "https://posit.co/blog/rss.xml",
      "https://rweekly.org/atom.xml"
    ),
    site_url = c(
      "https://www.r-project.org",
      "https://posit.co/blog",
      "https://rweekly.org"
    ),
    title = c("The R Blog", "Posit Blog", "R Weekly"),
    folder = c("R", "R", "Community"),
    source_kind = "subscription",
    etag = NA_character_,
    last_modified = NA_character_,
    poll_status = "sample",
    last_polled_at = utc_now(),
    created_at = utc_now(),
    stringsAsFactors = FALSE
  )

  entries <- data.frame(
    entry_id = paste0("sample-entry-", 1:6),
    feed_id = c(
      "sample-r-project",
      "sample-posit",
      "sample-rweekly",
      "sample-r-project",
      "sample-posit",
      "sample-rweekly"
    ),
    external_id = paste0("sample-", 1:6),
    url = c(
      "https://www.r-project.org/",
      "https://shiny.posit.co/",
      "https://rweekly.org/",
      "https://cran.r-project.org/web/packages/",
      "https://connect.posit.cloud/",
      "https://rweekly.org/about"
    ),
    canonical_url = NA_character_,
    title = c(
      "A calmer way to keep up with R",
      "Shiny as a personal information surface",
      "This week across the R community",
      "Package releases worth a closer look",
      "Deploying a small stateful application",
      "Notes from people building with R"
    ),
    author = c("R Core", "Posit", "R Weekly", "CRAN", "Posit", "R Weekly"),
    summary = c(
      "A sample story showing Rill's reading and interaction model.",
      "The reader itself is a Shiny app; durable state lives outside the process.",
      "A compact digest that is useful to skim and easy to return to.",
      "Package changes can be captured as feed items and revisited later.",
      "Neon provides durable Postgres while Connect Cloud runs the application.",
      "The interaction ledger is designed to support later personal analysis."
    ),
    feed_content = NA_character_,
    published_at = format(
      Sys.time() - c(900, 7200, 18000, 86400, 172800, 259200),
      tz = "UTC",
      usetz = TRUE
    ),
    inserted_at = utc_now(),
    content_hash = paste0("sample-hash-", 1:6),
    stringsAsFactors = FALSE
  )

  documents <- lapply(seq_len(nrow(entries)), function(index) {
    markdown <- paste0(
      "## A readable document\n\n",
      entries$summary[[index]],
      "\n\n",
      "Rill keeps the source feed, a cleaned reading copy, and your interactions as separate records. ",
      "That separation lets the reader improve without turning observability data into a behavioral database.\n\n",
      "> This is bundled demo content. Add a public feed to exercise the full ingestion and Defuddle path.\n\n",
      "### What gets remembered\n\n",
      "- opens and read state\n",
      "- stars and saves\n",
      "- dwell heartbeats and scroll milestones\n",
      "- the surface and list position that led to the article\n"
    )
    new_rill_document(
      entry_id = entries$entry_id[[index]],
      source_url = entries$url[[index]],
      acquisition_method = "sample",
      producer = "rill",
      producer_version = "1",
      title = entries$title[[index]],
      author = entries$author[[index]],
      site = feeds$title[match(entries$feed_id[[index]], feeds$feed_id)],
      published_at = entries$published_at[[index]],
      markdown = markdown,
      provenance = list(kind = "bundled_demo")
    )
  })
  names(documents) <- vapply(documents, `[[`, character(1), "document_id")

  list(feeds = feeds, entries = entries, documents = documents)
}

sample_rill_orientation <- function(store, reader_id) {
  candidates <- orientation_candidates(store, reader_id, limit = 6L)
  boundary <- orientation_boundary(candidates)
  selected <- candidates[c(1L, 5L)]
  cards <- Map(
    function(candidate, role, frame, interpretation, why_now) {
      list(
        role = role,
        frame = frame,
        document_id = candidate$document$document_id,
        entry_id = candidate$entry$entry_id,
        interpretation = interpretation,
        why_now = why_now,
        evidence = paste(
          "Rill keeps the source feed, a cleaned reading copy, and your",
          "interactions as separate records."
        )
      )
    },
    selected,
    c("anchor", "contrast"),
    c("unresolved_question", "counterpoint"),
    c(
      paste(
        "The source boundary is the foundation: captured material remains",
        "distinct from Rill's interpretation of it."
      ),
      paste(
        "Durable application state makes that boundary useful across days,",
        "but persistence should not collapse provenance into behavior."
      )
    ),
    c(
      "It states the product constraint that every agent action must preserve.",
      "It tests that constraint against the practical need for durable state."
    )
  )

  new_rill_orientation(
    reader_id = reader_id,
    boundary = boundary,
    question = "What must stay separate as Rill becomes agent-native?",
    introduction = paste(
      "Start with the boundary Rill protects, then test it against the case",
      "for durable state."
    ),
    cards = cards,
    agent_run_id = rill_id(
      "sample-orientation-run",
      reader_id,
      boundary$hash
    ),
    status = "Two source-grounded selections are ready."
  )
}

sample_rill_orientation_run <- function(orientation) {
  list(
    run_id = orientation$agent_run_id,
    reader_id = orientation$reader_id,
    kind = "orientation",
    request_key = rill_id(
      "sample-orientation-request",
      orientation$reader_id,
      orientation$boundary$hash
    ),
    retry_of_run_id = NULL,
    status = "completed",
    pinned_inputs = list(
      boundary_hash = orientation$boundary$hash,
      candidate_document_ids = orientation$boundary$document_ids,
      data_destination = "Bundled demo",
      model = "none",
      policy_version = orientation$policy_version
    ),
    requested_at = orientation$evaluated_at,
    terminal_at = orientation$evaluated_at,
    terminal_reason = "bundled_demo",
    updated_at = orientation$evaluated_at,
    usage = list()
  )
}
