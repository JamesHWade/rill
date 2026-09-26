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

  site <- "https://jameshwade.github.io/rill/"
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
    url = paste0(
      site,
      c(
        "",
        "#features",
        "articles/agents.html",
        "articles/agents.html#orientation",
        "articles/configuration.html",
        "articles/reading-copies.html"
      )
    ),
    canonical_url = NA_character_,
    title = c(
      "Welcome to Rill",
      "Read faster with the keyboard",
      "Ask Rill about a story",
      "Let Orientation choose what to read",
      "Keep your Library",
      "Save pages from your browser"
    ),
    author = "Rill",
    summary = c(
      "A tour of the reader in six short stories, bundled with the demo.",
      "J and K move between stories, and a few more keys cover the rest.",
      "Ask questions about the story you're reading, answered from its text.",
      "Orientation picks a few unread stories and shows the passage behind each pick.",
      "Point Rill at PostgreSQL to keep your feeds and reading history.",
      "Send pages Rill can't fetch, such as articles behind a login, from your browser."
    ),
    feed_content = NA_character_,
    preview_image_url = NA_character_,
    preview_image_alt = NA_character_,
    published_at = format(
      Sys.time() - c(900, 7200, 18000, 86400, 172800, 259200),
      tz = "UTC",
      usetz = TRUE
    ),
    inserted_at = utc_now(),
    content_hash = paste0("sample-hash-", 1:6),
    stringsAsFactors = FALSE
  )

  bodies <- c(
    paste(
      paste(
        "Rill is a feed reader. The six stories in this list are a short tour",
        "of it, and anything you do in the demo lasts until the app restarts."
      ),
      paste(
        "Rill has three columns. Feeds and views are on the left, the stories",
        "in the current view are in the middle, and the story you're reading",
        "fills the rest. On a phone, each one gets the whole screen."
      ),
      "To read your own feeds:",
      paste(
        "- Choose **Manage feeds** and paste a feed or website address.",
        "- Import an OPML file exported from another reader.",
        paste(
          "- Choose **Refresh feeds** to fetch the latest stories from The R",
          "Blog, the Posit Blog, and R Weekly."
        ),
        sep = "\n"
      ),
      paste(
        "The other stories cover the keyboard, Ask Rill, Orientation, keeping",
        "your Library, and saving pages from your browser."
      ),
      sep = "\n\n"
    ),
    paste(
      paste(
        "Opening a story marks it as read, so you can work through a whole",
        "view without the mouse:"
      ),
      paste(
        "| Key | Action |",
        "|---|---|",
        "| J | Next story |",
        "| K | Previous story |",
        "| O | Open the original page in a new tab |",
        "| S | Save or unsave |",
        "| F | Star or unstar |",
        "| Esc | Close the story |",
        sep = "\n"
      ),
      paste(
        "Shortcuts pause while you type in a field. On a phone, swipe a story",
        "to the left to mark it read, and use **Undo** if you change your",
        "mind."
      ),
      sep = "\n\n"
    ),
    paste(
      paste(
        "Open a story and choose **Ask Rill**. Try asking for a summary, the",
        "main argument, or the evidence behind a claim."
      ),
      paste(
        "Rill sends your question and the story's text to the model provider",
        "you configure. The agent can't browse the web or run code. It quotes",
        "the story for what the text supports and says when it goes further."
      ),
      "Choose the model before starting Rill, for example:",
      "    RILL_AGENT_MODEL=openai\n    OPENAI_API_KEY=your-key",
      "Without a provider key, questions end with an error message.",
      sep = "\n\n"
    ),
    paste(
      paste(
        "When no story is open, Rill can show Orientation: up to three unread",
        "stories worth your time, each with a short note on why and the",
        "passage it is based on. The rest of what it looked at is grouped into",
        "topics."
      ),
      paste(
        "Orientation sends the text of your newest unread stories to your",
        "model provider without you asking, so it starts turned off. Setting",
        "`RILL_ORIENTATION_ENABLED=true` makes it available, and each reader",
        "then turns it on from the sidebar."
      ),
      paste(
        "The Orientation in this demo is a fixed sample. Choose",
        "**Not for me** to remove a pick."
      ),
      sep = "\n\n"
    ),
    paste(
      paste(
        "The demo keeps everything in memory. To keep your feeds, stars,",
        "saves, and reading history, give Rill a PostgreSQL database:"
      ),
      "    DATABASE_URL=postgresql://user:password@host/dbname?sslmode=require",
      paste(
        "Rill creates its tables the first time it starts. A small hosted",
        "database such as Neon works well."
      ),
      paste(
        "To run Rill for other people, the",
        "[hosting guide](https://jameshwade.github.io/rill/articles/hosting.html)",
        "covers sign-in, scheduled feed polling, and approving new readers."
      ),
      sep = "\n\n"
    ),
    paste(
      paste(
        "Rill keeps a clean copy of every story. It starts with the text in",
        "the feed, then fetches the full article in the background with",
        "Defuddle. Some pages can't be fetched, often because they need you",
        "to be signed in."
      ),
      paste(
        "For those, extract the page in your own browser and send it to Rill.",
        "Set `RILL_CAPTURE_TOKEN`, then post the page's Markdown to",
        "`/api/v1/captures`. The capture becomes the story's reading copy, or",
        "a new story under **Local captures**."
      ),
      "Your browser's cookies never reach Rill, only the text you send.",
      sep = "\n\n"
    )
  )

  documents <- lapply(seq_len(nrow(entries)), function(index) {
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
      markdown = paste0(
        bodies[[index]],
        "\n\n> This story comes with Rill's demo."
      ),
      provenance = list(kind = "bundled_demo")
    )
  })
  names(documents) <- vapply(documents, `[[`, character(1), "document_id")

  list(feeds = feeds, entries = entries, documents = documents)
}

sample_rill_orientation <- function(store, reader_id) {
  candidates <- orientation_candidates(store, reader_id, limit = 6L)
  boundary <- orientation_boundary(candidates)
  selected <- candidates[c(1L, 5L, 6L)]
  cards <- Map(
    function(candidate, interpretation, why_now, evidence) {
      list(
        document_id = candidate$document$document_id,
        entry_id = candidate$entry$entry_id,
        interpretation = interpretation,
        why_now = why_now,
        evidence = evidence
      )
    },
    selected,
    c(
      paste(
        "An overview of the layout and of what the demo contains, with the",
        "first steps toward reading your own feeds."
      ),
      paste(
        "Explains why nothing in the demo is kept, and the one setting that",
        "changes that."
      ),
      paste(
        "Covers pages Rill can't fetch itself and how to send them from your",
        "own browser."
      )
    ),
    c(
      "Start here on a first visit",
      "Read before adding your own feeds",
      "Useful once your own feeds are coming in"
    ),
    c(
      paste(
        "The six stories in this list are a short tour of it, and anything",
        "you do in the demo lasts until the app restarts."
      ),
      "The demo keeps everything in memory.",
      paste(
        "Some pages can't be fetched, often because they need you",
        "to be signed in."
      )
    )
  )
  themes <- list(
    list(
      name = "Reading faster",
      note = "Keyboard shortcuts, swipes, and Undo.",
      entry_ids = candidates[[2L]]$entry$entry_id
    ),
    list(
      name = "The agent features",
      note = "Ask Rill and Orientation, and what each sends to a model provider.",
      entry_ids = vapply(
        candidates[c(3L, 4L)],
        \(candidate) candidate$entry$entry_id,
        character(1)
      )
    )
  )

  new_rill_orientation(
    reader_id = reader_id,
    boundary = boundary,
    question = "Where should a new reader start?",
    cards = cards,
    themes = themes,
    agent_run_id = rill_id(
      "sample-orientation-run",
      reader_id,
      boundary$hash
    ),
    status = "Three picks are ready."
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
