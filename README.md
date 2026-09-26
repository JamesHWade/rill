# Rill <img src="man/figures/logo.png" align="right" height="139" alt="Rill hex sticker: an otter reading a book in a stream." />

<!-- badges: start -->
[![R-CMD-check](https://github.com/JamesHWade/rill/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JamesHWade/rill/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

Rill is a personal feed reader, packaged as an R package with a Shiny app. It
follows RSS and Atom feeds, keeps a clean copy of each article, and remembers
what you have read, starred, and saved. Two optional features use a language
model: Ask Rill answers questions about the story you are reading, and
Orientation suggests what to read next. Both work from Rill's saved copy of
each article and say when they go beyond its text.

![The Rill reader: feeds and views on the left, the unread queue in the middle, and an article on the right.](man/figures/README-reader.png)

## Features

- Reads RSS 2.0, RSS 1.0, and Atom feeds. Give it a website address and it
  finds the feed.
- Sorts feeds into Groups. A feed can belong to several Groups, and OPML import
  and export keep them.
- Shows Unread, All, Starred, Saved, Today, This week, and This month views.
  The calendar views follow your browser's time zone.
- Opens a story straight from the feed's text, then fetches the full article
  in the background with [Defuddle](https://github.com/kepano/defuddle). Pages
  that need a login can be captured in your own browser and sent to Rill.
- Ask Rill answers questions about the open story from its saved copy.
- Orientation, when turned on, picks up to three unread stories and shows the
  passage behind each pick.
- Keyboard shortcuts: <kbd>J</kbd> and <kbd>K</kbd> for the next and previous
  story, <kbd>O</kbd> to open the original, <kbd>S</kbd> to save,
  <kbd>F</kbd> to star, and <kbd>Esc</kbd> to close the story.

## Try the demo

Rill needs R 4.3 or later. Install it from GitHub with
[pak](https://pak.r-lib.org), then start the app:

```r
# install.packages("pak")
pak::pak("JamesHWade/rill")

shiny::runApp(rill::rill_app())
```

Without a database, Rill opens six stories that tour the app and keeps
everything in memory, so your changes disappear when R restarts. Orientation
shows a fixed sample, and Ask Rill works once a model provider is set up.

## Keep your reading

Point `DATABASE_URL` at a PostgreSQL database to keep your feeds and reading
state. Rill creates and updates its tables when it starts. A
[Neon](https://neon.tech) connection string works well:

```text
DATABASE_URL=postgresql://user:password@host.neon.tech/neondb?sslmode=require
RILL_ACTOR_ID=reader
```

Add feeds from **Manage feeds** in the sidebar, or import an OPML file from
another reader. From R, `read_opml()` and `write_opml()` do the same
conversion.

To ask questions about stories, set `RILL_AGENT_MODEL` to any model that
[`ellmer::chat()`](https://ellmer.tidyverse.org/reference/chat.html) accepts,
along with that provider's API key:

```text
RILL_AGENT_MODEL=openai
OPENAI_API_KEY=your-key
```

The [configuration article](https://jameshwade.github.io/rill/articles/configuration.html)
lists every setting.

## Learn more

- [Configuration](https://jameshwade.github.io/rill/articles/configuration.html):
  every environment variable, grouped by feature.
- [Ask Rill and Orientation](https://jameshwade.github.io/rill/articles/agents.html):
  what the model features do and what they send to your provider.
- [Reading copies](https://jameshwade.github.io/rill/articles/reading-copies.html):
  how Rill prepares full articles and accepts pages captured in your browser.
- [Running Rill for others](https://jameshwade.github.io/rill/articles/hosting.html):
  sign-in, scheduled polling, admitting readers, and telemetry.

## Development

Rill uses the standard devtools workflow and [Air](https://posit-dev.github.io/air/)
for formatting. From a clone:

```r
source("scripts/bootstrap.R") # install development dependencies
devtools::test()
shiny::runApp() # runs app.R against the working copy
```

Set `RILL_TEST_DATABASE_URL` to a disposable PostgreSQL database to run the
PostgreSQL tests too. Design decisions are recorded in
[`docs/adr/`](https://github.com/JamesHWade/rill/tree/main/docs/adr), and
`CONTEXT.md` defines the terms used in the code.
