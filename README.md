# Rill <img src="man/figures/logo.png" align="right" height="139" alt="Rill hex sticker: an otter reading a book in a stream." />

Rill is a fast, personal Google Reader-style app delivered as an R package and
Shiny application. It keeps the feed reader small while leaving useful seams
for later analysis:

- RSS and Atom ingestion with HTML feed autodiscovery
- OPML import and export with folder preservation
- conditional refreshes using ETag and Last-Modified
- clean reading copies from Defuddle or a local browser capture, cached in Postgres
- Ask Rill, with source-grounded questions handled by a tightly bounded Deputy Agent embedded in the reader
- a maintained Orientation that offers a source-grounded path into the reading queue
- read, star, and save state
- an append-only interaction ledger for impressions, opens, scroll milestones, dwell heartbeats, and outbound clicks
- vendor-neutral OpenTelemetry traces and logs, ready for Logfire
- a bundled in-memory demo when no database is configured

The working title is **Rill**. Rename it freely.

## Architecture

| Concern | Owner | Why |
|---|---|---|
| Feed discovery and parsing | R (`httr2`, `xml2`) | Small enough to keep inside the app initially |
| Clean article document | Defuddle or authenticated browser capture | Keeps exact Markdown and producer provenance behind one document boundary |
| Reader UI | Shiny + `bslib` | Keeps the product fully in R |
| Ask Rill | Deputy + shinychat | Streams governed answers from one pinned immutable Document |
| Durable state | Neon Postgres | Connect Cloud instances should not be treated as durable filesystems |
| Product behavior | `events` table in Neon | Interaction data remains queryable application data |
| Operations and diagnostics | OpenTelemetry → Logfire | Disclosed, opt-out traces remain non-authoritative diagnostic data |

The package exposes five focused entry points: `rill_app()` creates the Shiny
application, `poll_feeds()` runs a due-aware durable scheduled refresh,
`prepare_today()` pre-builds the current day's reading copies, and
`read_opml()` and `write_opml()` provide a small interchange boundary for
Subscription lists. Feed parsing, storage, telemetry, and UI helpers remain
internal. Defuddle remains behind a narrow adapter rather than becoming its
own package.

## Run in demo mode

You need a current R installation. From the project directory:

```r
source("scripts/bootstrap.R")
devtools::test()
shiny::runApp()
```

The bootstrap reads development dependencies from `DESCRIPTION` with pak and
records them in `renv.lock`. When R is not available,
`node --check inst/app/www/app.js && node scripts/static-check.mjs` still catches
JavaScript syntax errors, missing app files, and unbalanced R delimiters. It is
not a substitute for the R tests.

With no `DATABASE_URL`, Rill loads six bundled stories and exercises the UI and interaction model in memory. Restarting the R process resets that demo state.

On desktop, press `J` or `K` to move to the next or previous visible story, `O` to open the selected story's original page in a new tab, `S` to toggle Saved, and `F` to toggle Starred. Shortcuts are disabled while typing in a form field.

## Ask Rill about a story

Select a story and use the **Ask Rill** panel without leaving the reading
surface. Rill creates a session-scoped Deputy Agent with one read-only tool: it
can retrieve the selected immutable Document and its provenance, but it cannot
browse the web, read or write files, run a shell, or execute R code. Answers are
prompted to distinguish source evidence, interpretation, and unsupported gaps.
The panel names the configured model provider that receives the Reader's
question and the provider-safe projection of the selected Document.

Set the model and the matching provider credential before launching Rill. The
default uses ellmer's OpenAI provider:

```text
RILL_AGENT_MODEL=openai
OPENAI_API_KEY=your-key
```

Any model specification supported by `ellmer::chat()` can be supplied through
`RILL_AGENT_MODEL`. Ask Rill messages currently last for the Shiny session;
durable Conversation history is a follow-up. Each question still receives
a bounded Agent Run. In PostgreSQL mode, its pinned Document identity, lifecycle,
usage, terminal reason, and Deputy run ID persist across app restarts. Demo-mode
Agent Runs reset with the R process.

Today, This week, and This month use the browser's local calendar and show their
date range and time zone. Calendar views update as the local day changes; a
story already open for reading stays open. UTC is the labeled fallback if the
browser's time zone is unavailable.

The Today view includes **Prepare**, which queues missing clean reading copies
in the background. Existing captured or extracted documents are preserved, and
failures remain available for a later retry. The synchronous
`rill::prepare_today()` API remains available outside the app and uses the
scheduled process's time zone.

## Maintain Orientation

With no story selected, Orientation presents up to three source-linked choices
that frame why each Document may be worth reading now. It keeps exact Source
Evidence separate from agent-written Interpretation, preserves the producing
Agent Run, and treats **Not for me** as explicit Reader input. Opening a card
does not count as endorsement.

Automatic model use is off by default. To make maintained Orientation available
in the app, configure the model provider and installation gate:

```text
RILL_AGENT_MODEL=openai
RILL_AGENT_BASE_URL=https://api.openai.com/v1
OPENAI_API_KEY=your-key
RILL_AGENT_POLICY_URL=https://provider.example/privacy
RILL_ORIENTATION_ENABLED=true
```

The Reader must then confirm the named Data Destination once from the sidebar.
That confirmation is stored per Reader, remains inspectable, and can be disabled
without deleting the current Orientation. A provider endpoint change fails
closed and requires a new confirmation; a model upgrade at the same endpoint
does not. Remote Ollama endpoints are external destinations; only a loopback
Ollama endpoint is treated as part of the installation. Providers whose
endpoint Rill cannot resolve must set `RILL_AGENT_BASE_URL` explicitly before
automatic Orientation can be enabled.
Rill reuses immutable reading copies, creates missing copies only from
already-ingested feed content, and sends only the bounded candidate copies to
the confirmed provider. That bounded set discloses which candidate Documents
are currently unread, but not the rest of the Library, the Reading History
event log, Reader Memory, or credentials. External Orientation cannot be
enabled until
`RILL_AGENT_POLICY_URL` provides an inspectable HTTP or HTTPS link to the terms
that govern data at that provider. Full scheduled maintenance remains follow-up
work. If a Reader question arrives during maintenance, Rill durably preserves
it, cooperatively stops Orientation, and starts the question exactly once.

## Move subscriptions with OPML

Open **Manage feeds** in the sidebar to import an OPML file from another feed
reader or export the current subscriptions. Rill preserves nested OPML groups
as slash-separated folder paths and restores those paths when exporting. An
import registers subscriptions immediately; use **Refresh feeds** to fetch
their first stories. In demo mode, imported subscriptions last until the R
process restarts.

The same interchange is available from R:

```r
subscriptions <- read_opml("subscriptions.opml")
write_opml(subscriptions, "rill-subscriptions.opml")
```

## Add Neon

Create a Neon project and copy its pooled Postgres connection string. Set it as
`DATABASE_URL`; include `sslmode=require`. Rill applies ordered, checksummed
schema migrations when the app starts.

For local development, put variables in your normal secret-management flow or load an uncommitted `.env`. The repository includes `.env.example` only as a field reference. Do not commit a real Neon password.

Required for persistence:

```text
DATABASE_URL=postgresql://user:password@host.neon.tech/neondb?sslmode=require
RILL_ACTOR_ID=reader
```

Set `RILL_CAPTURE_TOKEN` to enable local browser capture for the Reader named by
`RILL_ACTOR_ID`. Use a long random value and keep it in the same secret-management
flow as `DATABASE_URL`; Rill stores only its hash in PostgreSQL. Removing the
variable disables the capture endpoint even if stored credential hashes remain.

Neon is a good fit for this first version. Alternatives become attractive only for a specific reason: Supabase if bundled auth/storage is important, or a conventional managed Postgres instance if predictable always-on latency matters more than serverless scale-to-zero.

## Deploy on Posit Connect Cloud

Rill supports an in-app Auth0 gate for Posit Connect Cloud, where Free content
remains public at the platform edge. The gate prevents Rill's Reader server,
Library queries, and Agent actions from starting until Auth0 validates an
identity attached to a Reader. A verified identity without a binding receives
an access-request confirmation while Rill records one pending admission. The
application shell and static assets remain public.

See [the Connect Cloud deployment runbook](docs/deployment/connect-cloud.md) for
the manifest, Auth0 callback, Neon, OpenAI, and hourly GitHub Actions polling
configuration.

## Defuddle

Opening an article renders its saved reading copy immediately, without waiting
for Defuddle. On a cache miss, Rill first stores a sanitized **Feed copy** that
preserves paragraphs, images, links, and lists, and may be an excerpt. A separate
process prepares the full article while you keep reading. When it is ready,
choose **Load full article**; background completion never changes the displayed
copy or an Ask Rill answer's evidence. Browser captures and pinned Orientation
copies are preserved. Automatic Orientation retains its bounded text-only inputs.

After Library refresh, Rill automatically prepares missing public articles from
the past seven days. The scheduled poller also prepares up to 100 articles or
ten minutes of work after releasing the Feed polling lock. The configured
extractor receives public subscription article URLs (by default, `defuddle.md`),
not private captures or Reader state. Shared per-article leases prevent duplicate
work across sessions and the poller. Failures back off for 5, 10, 20, 40, then
80 minutes; after five automatic attempts, **Prepare full article** or Today's
**Prepare** can retry once that cooldown has elapsed. A crashed worker's lease
expires after five minutes. Saved copies remain available throughout; manual
preparation details expose sanitized references, not raw service errors.

This improves responsiveness; it cannot guarantee that a publisher permits full
extraction. A blocked article remains readable from its feed content with an
**Original** link. Hosted opening latency still needs measurement after deployment.

Recognized YouTube and Vimeo video references render as privacy-enhanced,
sandboxed embeds. Rill removes arbitrary embedded frames and executable markup
from clean reading copies.

The default hosted API is an external Data Destination. Rill sends it the public
page URL so it can fetch and extract the page. Set `DEFUDDLE_API_KEY` if you
have one:

```text
DEFUDDLE_BACKEND=hosted
DEFUDDLE_API_KEY=your-key
```

To keep extraction within the Rill installation, install the Defuddle CLI with
`npm install -g defuddle`, then configure that backend:

```text
DEFUDDLE_BACKEND=local
DEFUDDLE_COMMAND=defuddle
```

`DEFUDDLE_COMMAND` may also be an explicit executable path. Rill invokes
`defuddle parse <url> --md --frontmatter`, applies the same public-URL safety
check as the hosted adapter, and records `defuddle-local` as the extraction
engine. The executable must be installed in the runtime environment, so the
hosted backend is usually simpler for Connect deployments.

Both backends fetch public pages without an authenticated browser session.
Paywalled or browser-only pages can instead be extracted in the reader's
authenticated browser and sent through the Rill capture endpoint described
below. Rill accepts the supplied document; it does not fetch with or store
browser credentials.

## Browser capture

When `RILL_CAPTURE_TOKEN` is set, the same application serves
`POST /api/v1/captures`. The token resolves to its owning Reader rather than a
process-wide identity. A browser extension or local clipper sends already
extracted Markdown with a stable capture ID and producer identity:

```json
{
  "capture_id": "3f87de90-4302-4f3e-9e84-2f5d4c966abd",
  "source_url": "https://example.com/article",
  "canonical_url": "https://example.com/article",
  "title": "An article worth keeping",
  "author": "Ada Lovelace",
  "site": "Example",
  "published_at": "2026-08-29T12:30:00Z",
  "markdown": "# An article worth keeping\n\nExact captured text.",
  "captured_at": "2026-08-30T22:15:00Z",
  "producer": "my-rill-clipper",
  "producer_version": "1.0.0",
  "metadata": {
    "extraction_mode": "article"
  }
}
```

Send it as authenticated JSON:

```sh
curl --request POST "https://your-rill.example/api/v1/captures" \
  --header "Authorization: Bearer $RILL_CAPTURE_TOKEN" \
  --header "Content-Type: application/json" \
  --data @capture.json
```

`capture_id`, `source_url`, `title`, `markdown`, `captured_at`, and `producer`
are required. Source URLs must be public HTTP or HTTPS URLs, and request bodies
are limited to 5 MiB. An exact retry returns the original entry and document
IDs; reusing a capture ID for different evidence returns `409 Conflict`.

Rill attaches a capture to an existing feed entry when the canonical or source
URL matches. Otherwise it creates an unread entry under **Local captures**.
Captured Documents, standalone capture entries, and selected reading copies are
private to the Reader resolved from the token. A capture by one Reader cannot
replace or reveal another Reader's reading copy. Public extractions remain
shared, and Readers without a private selection follow the latest public copy.
Documents remain immutable: exact content, producer version and metadata,
source URLs, producer record ID, capture time, receipt time, and hashes remain
available while separate public and Reader-owned selectors choose the reading
copy. That Document interface grounds Ask Rill and Orientation.
Demo-mode captures work but disappear when the R process restarts.

## Logfire and telemetry

The current code emits low-cardinality, content-free spans and logs around
database setup, feed HTTP work, and document extraction. It never intentionally
sends article titles, article bodies, or full URLs to the telemetry backend.

The reader now includes Ask Rill, but content-bearing agent-interaction
telemetry remains disabled. The agent-native v1 design permits
opt-out Conversation tracing only after the Reader confirms Logfire as a Data
Destination. The future diagnostic copy may contain visible messages and
sanitized structural spans, but not hidden instructions, credentials, raw
Documents, tool payloads, or unsanitized exceptions. This export path is not yet
implemented.

Set the standard OTLP variables:

```text
OTEL_SERVICE_NAME=rill
OTEL_RESOURCE_ATTRIBUTES=service.namespace=personal-reader,deployment.environment=production
OTEL_TRACES_EXPORTER=http
OTEL_LOGS_EXPORTER=http
OTEL_METRICS_EXPORTER=none
OTEL_R_SUPPRESS_SCOPES=httr2
OTEL_ATTRIBUTE_VALUE_LENGTH_LIMIT=256
OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT=false
OTEL_EXPORTER_OTLP_ENDPOINT=https://logfire-us.pydantic.dev
OTEL_EXPORTER_OTLP_HEADERS=Authorization=your-logfire-write-token
```

`OTEL_R_SUPPRESS_SCOPES=httr2` matters: automatic HTTP semantic spans may contain complete request URLs, which would leak part of the reading history. Rill's own enclosing spans identify only the operation and extractor, not the destination.

Rill refuses to start when `OTEL_INSTRUMENTATION_GENAI_CAPTURE_MESSAGE_CONTENT` is enabled. The standard ellmer instrumentation flag can export prompts, responses, and tool results; Rill keeps it disabled until a separately accepted, sanitized Ask Rill tracing path exists.

Because this is OTLP rather than a Logfire-specific client, Grafana Cloud, Honeycomb, or another collector can replace Logfire without changing the app's instrumentation. The `events` table is deliberately separate: those records are the material for later ranking, daily review, and behavioral analysis.

## Deploy a personal instance to Posit Connect Cloud

1. Run `rsconnect::writeManifest()` and commit the resulting `manifest.json`.
2. Run the tests and launch the app locally.
3. Push the project to the Git repository used by Connect Cloud, or publish it from Positron/RStudio.
4. Add `DATABASE_URL`, `RILL_ACTOR_ID`, `RILL_AGENT_MODEL`, its provider API key, optional `RILL_AGENT_BASE_URL`, the selected `DEFUDDLE_*` settings, optional `RILL_ORIENTATION_ENABLED`, the required `RILL_AGENT_POLICY_URL` when external Orientation is enabled, optional `RILL_CAPTURE_TOKEN`, and any `OTEL_*` values as deployment secrets.
5. Keep the first deployment private. The current app assumes one trusted reader identity and does not contain multi-user authentication.

The refresh button works inside the app. `scripts/poll.R` is the ingestion entry point for a scheduled job. It skips Feeds until `RILL_POLL_INTERVAL_MINUTES` has elapsed, records every per-Feed outcome, and tolerates isolated failures. It exits non-zero when failures reach `RILL_POLL_FAILURE_THRESHOLD` or a systemic error interrupts the run. A scheduler can run `rill::prepare_today()` afterward to pre-build clean reading copies for the current local day.

## Hosted Rill direction

Subscriptions, folders, Entry state, Reading History, captured Documents, and
selected reading copies are Reader-owned. Public source Documents remain
shared, with immutable acquisition provenance. The invited-beta deployment
uses a single Render Docker web process behind an OpenID Connect proxy, Auth0
as the first identity adapter, Neon PostgreSQL, and a separate Render cron
process for feed polling. The production image, local composition, and exact
environment contract are documented in the
[Render deployment guide](docs/deployment/render.md). See also the
[proposed ADR](docs/adr/0001-hosted-rill-runtime-and-identity.md) and
[hosting research](docs/research/hosted-rill-platforms.md). The accepted
[Reader ownership contract](docs/adr/0007-separate-shared-sources-from-reader-libraries.md)
defines the isolation and legacy migration, while the
[Reader data lifecycle policy](docs/adr/0008-govern-reader-data-lifecycle-and-capacity.md)
defines export, deletion, shared-source retention, and capacity signals.
Additional Readers enter through an explicit, audited admission rather than
the bootstrap allowlist.

An operator with the deployment's `DATABASE_URL` can approve a recorded request
without copying its external identity subject:

```r
requests <- rill::list_reader_admissions()
rill::approve_reader_admission(
  requests$request_id[[1]],
  responsible_id = "operator:james"
)
```

Approving a pending request creates a new empty Library; retrying that approval
keeps the existing Reader and Library. Approval never grants access to another
Reader's Subscriptions, reading state, Captures, or selected reading copies.
Unlinked requests expire 30 days after their most recent sign-in.

## Package development

The repository follows the standard devtools, testthat, roxygen2, pkgdown, and
Air workflow:

```r
devtools::document()
devtools::test()
pkgdown::check_pkgdown()
devtools::check()
```

Run `air format .` before the checks. `R CMD check` is also exercised across
R release, devel, and oldrel on Linux, macOS, and Windows with r-lib/actions.

## Event contract

Each event has an id, Reader, session, optional Feed Entry, timestamp, surface, list position, JSON payload, and schema version. Current event types are:

- `feed_filter`, `feed_added`, `feed_renamed`, `feed_moved`, `feed_unsubscribed`, `feeds_refreshed`, `opml_imported`, `opml_exported`
- `entry_opened`, `article_impression`, `open_original`
- `document_captured`
- `star_changed`, `save_changed`, `read_state_changed`, `read_state_bulk_changed`
- `today_prepared`
- `scroll_milestone`, `dwell_heartbeat`, `page_hidden`

This is enough to derive useful daily features without pretending every signal means the same thing. For example: opened but bounced, read deeply, saved but never reopened, repeatedly revisited, or discovered through a particular feed position.

## Immediate next cuts

Collect real reading, capture, Ask Rill, and maintained Orientation feedback.
The next hardening cut is scheduled maintenance and longer-lived Conversation
restoration. Search, embeddings, and summaries should still enter only through
an accepted reading workflow rather than as standalone features.
