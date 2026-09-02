# rill 0.0.0.9000

* Source-grounded Conversation now embeds shinychat beside the selected story, streams a tightly bounded Deputy Agent over one immutable Document, and records each question in the durable Agent Run lifecycle with cancellation and Retry (#30).

* Rill now has a duck-and-ripple identity, paired duck-egg daylight and warm ink-and-reed dark palettes, a system-aware appearance control, and an Atkinson Hyperlegible/Literata type system tuned for efficient navigation and comfortable reading.

* Navigation and the reading queue now use nested, fill-aware `bslib` sidebars that can be resized or collapsed on desktop and stack above content on mobile.

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

* `poll_feeds()` and `rill_app()` provide focused entry points for scheduled refreshes and the Shiny application.

* `read_opml()` and `write_opml()` import and export OPML 2.0 subscription lists, including nested feed folders; the Shiny app registers imports immediately under **Manage feeds** and leaves feed refresh as a separate action.
