# rill 0.0.0.9000

* Rill now has a duck-and-ripple identity, paired duck-egg daylight and warm ink-and-reed dark palettes, a system-aware appearance control, and an Atkinson Hyperlegible/Literata type system tuned for efficient navigation and comfortable reading.

* Navigation and the reading queue now use nested, fill-aware `bslib` sidebars that can be resized or collapsed on desktop and stack above content on mobile.

* Defuddle extraction can now run through a locally installed CLI by setting `DEFUDDLE_BACKEND=local`; the hosted API remains the default.

* `DATABASE_URL` is now parsed into explicit PostgreSQL connection fields, including Neon SSL parameters, instead of being treated as a literal database name.

* Invalid feed publication dates no longer interrupt story rendering.

* Local browser captures can now be posted to the authenticated `/api/v1/captures` endpoint. Captured and Defuddle-produced content share an immutable, provenance-preserving document boundary for reading and future agent use.

* The reading queue now keeps opened stories in place until the view changes, uses contextual empty states, and has clearer keyboard focus and accessible labels.

* The reader now supports `J`/`K` navigation, `O` to open the original, `S` to save, and `F` to star.

* `poll_feeds()` and `rill_app()` provide focused entry points for scheduled refreshes and the Shiny application.

* `read_opml()` and `write_opml()` import and export OPML 2.0 subscription lists, including nested feed folders; the Shiny app registers imports immediately under **Manage feeds** and leaves feed refresh as a separate action.
