# rill 0.0.0.9000

* Manage feeds now opens a searchable dialog with per-feed status, single-feed refresh, retry for failed feeds, existing-folder suggestions, and restoration of unsubscribed feeds; manual refresh records polling outcomes and reports only genuinely new stories.

* `approve_reader_admission()` and `list_reader_admissions()` provide a privacy-safe operator workflow for admitting a verified identity to a new isolated Reader, while both hosted Auth0 adapters now record pending access requests and explain the next step to the invited person (#26, #45).

* Rill can now enforce an in-app Auth0 gate on public Shiny hosts such as Posit Connect Cloud, and the default branch can run due-Feed polling hourly under an explicit kill switch (#45).

* Hosted Rill now binds browser-capture credentials, captured Documents, standalone capture entries, and reading-copy selection to each Reader while retaining shared public Documents and immutable acquisition provenance, and admits additional Readers only after these isolation boundaries are installed (#21).

* Hosted Rill now stores active and inactive Subscriptions, folders, Feed labels, Entry state, and Reading History per Reader while keeping Feed acquisition shared and polling each Feed once while any Subscription is active; add, move, rename, unsubscribe, and OPML workflows all enforce the authenticated Library boundary (#20).

* Hosted Rill now resolves verified external identities through a durable Reader Identity module, records deduplicated pending admissions and mutable profile metadata, denies disabled Readers, and supports explicit, audited operator admission (#19).

* Hosted Rill now admits only configured Auth0 subjects to one private Reader, keeps Shiny behind oauth2-proxy, strips forged identity headers, and provides complete sign-out without retaining or forwarding provider tokens (#29).

* A non-root production image now runs Rill as either a loopback-only Shiny web process behind oauth2-proxy or a scheduled Feed poller, with local Compose and Render deployment guidance (#23).

* Orientation now maintains a source-grounded zero-to-three-Document reading path, preserves exact evidence and producer provenance, records explicit dismissals, durably preserves questions while yielding to User Engagement, and requires persistent per-Reader confirmation of its endpoint-bound Data Destination before automatic model use (#32).

* Ask Rill now embeds shinychat beside the selected story, streams a tightly bounded Deputy Agent over one immutable Document, and records each question in the durable Agent Run lifecycle with cancellation and Retry (#30).

* Rill now has a reading-otter identity across desktop and compact headers, browser and home-screen icons, and quiet reading states, paired river-mist daylight and warm ink-and-reed dark palettes, a system-aware appearance control, and an Atkinson Hyperlegible/Literata type system tuned for efficient navigation and comfortable reading.

* Navigation and the reading queue now use nested, fill-aware `bslib` sidebars that resize on wide screens, reduce to Queue plus reading canvas at medium widths, and become separate Library, Queue, and Reading surfaces on phones without discarding the selected Document (#47, #48).

* Orientation now distinguishes source Documents, agent interpretation, path rationale, and quoted evidence at every step; Reading exposes stored-copy provenance through `bslib`, and Ask Rill has a source-bound context panel with an explicit responsive trigger (#49).

* Rill now presents startup, loading, disconnected, reconnecting, progress, notification, validation, and recovery states as one accessible visual system, with a skip link, live status semantics, reduced-motion and forced-color support, and an inspectable responsive browser contract (#50).

* Feed navigation and the reading queue now remain independently scrollable when their contents exceed the viewport (#28).

* Feeds can now be given a reader-defined name under **Manage feeds**; the Reader's label is stored separately from source metadata and persists when the Feed is refreshed (#28).

* Defuddle extraction can now run through a locally installed CLI by setting `DEFUDDLE_BACKEND=local`; the hosted API remains the default.

* `DATABASE_URL` is now parsed into explicit PostgreSQL connection fields, including Neon SSL parameters, instead of being treated as a literal database name.

* Invalid feed publication dates no longer interrupt story rendering.

* Local browser captures can now be posted to the authenticated `/api/v1/captures` endpoint. Captured and Defuddle-produced content share an immutable, provenance-preserving document boundary for reading and future agent use.

* The reading queue now keeps opened stories in place until the view changes, uses contextual empty states, and has clearer keyboard focus and accessible labels.

* Reading-status controls can mark the current story unread, mark all stories in the current Feed scope as read, or mark stories older than 24 hours as read; bulk changes remain distinct from open history (#28).

* The reading queue can now sort stories by newest, oldest, recently added, Feed name, or story title (#28).

* The reading queue now includes Today, This Week, and This Month views based on local calendar boundaries and excludes future-dated stories (#28).

* The Today view and `prepare_today()` can now pre-build missing clean reading copies without overwriting successful documents; extraction failures and feed fallbacks remain retryable (#28).

* Trusted YouTube and Vimeo embeds now appear in clean reading copies through privacy-enhanced, sandboxed frames; arbitrary embedded frames remain blocked (#28).

* The reader now supports `J`/`K` navigation, `O` to open the original, `S` to save, and `F` to star.

* `poll_feeds()` refreshes only due shared Feeds, prevents overlapping runs, records durable per-Feed outcomes, tolerates isolated failures, and exits non-zero only for systemic errors or the configured failure threshold (#22).

* `read_opml()` and `write_opml()` import and export OPML 2.0 subscription lists, including nested feed folders; the Shiny app registers imports immediately under **Manage feeds** and leaves feed refresh as a separate action.
