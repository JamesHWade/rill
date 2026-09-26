# rill 0.0.0.9000

* The README is shorter and covers installing Rill and trying the demo. Setup details moved to four website articles: Configuration, Ask Rill and Orientation, Reading copies, and Running Rill for others.

* Manage feeds starts with adding a feed, keeps Done and a close button in view while its body scrolls, makes Retry failed feeds a secondary action, gathers the Group tools under one heading, and asks before deleting a Group (#99).

* Ask Rill shows its greeting again after you open a story. Clearing the conversation for a new story used to remove the greeting until the page reloaded.

* The rating buttons in Orientation and Ask Rill are smaller, and Browse unread stories now comes before Rate this Orientation.

* Story cards take a feed's initials from its first two words, skipping "The" ("The R Blog" is RB, not TH), and feed details say when a feed was last checked in words such as "2 hours ago".

* On touch screens, a thin strip of the swipe action shows at the edge of each story, hinting that stories can be swiped (#100).

* The demo opens with six short stories that walk through reading, the keyboard, Ask Rill, Orientation, keeping your Library, and browser captures. They replace placeholder articles attributed to R Core, Posit, and CRAN.

* Interface text is shorter and plainer. Ask Rill, Orientation, and the reading-copy details say what they do and what they send in everyday terms, Orientation settings name the environment variable to set when something is missing, and long Orientation quotes end at a word instead of mid-word.

* Orientation rejects evidence quoted from Rill's own truncation notice while the agent can still correct it, instead of failing the update after the agent finishes.

* A blank `RILL_ACTOR_ID` now uses the default Reader instead of opening a second, empty Library, and a question that can't be resumed after a reload reports its actual cause.

* The reading queue is no longer capped at 150 stories. Long views show a count such as "150+", and **Show more stories** loads the next page.

* Expanded Groups in the Library stay open when counts update, and keyboard focus returns to Star, Save, or Mark unread after you use them.

* Opening, starring, saving, or marking stories now reports storage errors, including a story removed from your Library on another device, instead of closing the session.

* The open article no longer rebuilds when you star or save it or when the Library refreshes in the background, so focus, text selection, and the reading-copy details stay put.

* Manage feeds keeps a half-typed feed name and unsaved Group choices when the Library refreshes.

* Keyboard shortcuts keep working after you choose a view, and no longer act on stories behind an open dialog.

* Ask Rill no longer stops the app for every reader when an answer reaches its time limit or when a question waits for Orientation. Stop now interrupts an answer while it streams, and stopping a question that is waiting for Orientation re-enables the chat.

* A fresh visit no longer reopens a story whose question you cancelled or that failed more than ten minutes ago; running answers and recent failures still reopen so Retry stays in view.

* Reader Memory explains problems you can fix, such as a passage that appears more than once in the story, instead of reporting that the memory changed.

* Reading copies now drop HTML comments, `noscript`, and other elements that browsers parse differently from Rill's sanitizer, so feed content can no longer run scripts in the reader.

* Feed fetching never treats a response body or feed field as a URL or file path, checks each redirect before following it, rejects `*.localhost`, carrier-grade NAT, numeric, and IPv6 literal hosts, and no longer waits on `Retry-After` values over ten seconds.

* Feeds keep their full `content:encoded` or Atom `content` when a shorter description comes first, ignore empty Media RSS `content`, read Atom author names without their URL or email, prefer publication dates over update dates, use permalink GUIDs for items without a link, and skip `atom:link` when finding a site's address.

* RSS publication dates now keep their time zone offsets, such as `-0700`, so stories from feeds outside UTC sort correctly and appear in the right calendar view.

* Feeds that declare their encoding only in the XML prolog, or send Windows-1252 text without a charset, now refresh instead of failing.

* Opening a story whose feed item has no readable text now shows a placeholder copy instead of closing the session.

* Mark older than a day as read now works with PostgreSQL storage; it previously ended the session.

* Reader Memory now consumes Graft's public API4 artifact workflow while preserving exact reviewed decisions, retained evidence, and Reader-scoped consultation (JamesHWade/graft#91).

* Rill now requires R 4.3.0 or later, matching its Graft dependency (#104).

* Opt-in Reader Memory preserves explicitly accepted preferences and source-anchored interpretations privately through Graft, with correction, Archive, restore, and exact decision checks before Ask Rill consultation (#104).

* `evaluate_reader_feedback()` turns retained Reader ratings into offline vitals evaluations and compares independent helpfulness judgments against those ratings; `reader_feedback_samples()` preserves output identity, reasons, and historical provenance for analysis (#93).

* Orientation has a labeled button in the queue and article toolbar. Fresh visits no longer reopen the article from a completed answer; use Reopen last answer in the Library to return to that answer and its reading copy.

* Orientation now shows up to three independent picks in a compact row layout with the question as the headline, the first sentence of each exact Source Evidence passage inline, and the title as the read action; the agent evaluates up to 36 newest unread Documents within its source budget and folds the unpicked ones into up to five themes that open a scoped unread queue, with a computed totals line covering the whole unread queue (#51).

* Article navigation and read actions respond sooner. On phones, queue cards follow your finger as you swipe left to mark read; inside an article, swipe left for the next article or right for the queue. Successful action notices dismiss after eight seconds, with extra time while using Undo (#88).

* Browse a source-first article timeline with source excerpts and optional images, visible Save and Mark read actions, and swipe-to-read with Undo (#88).

* Reading and Ask Rill use scalable text and offer reversible pane focus controls (#82, #83).
* Document tool results show source details before inspectable original JSON (#84).
* Rating review renders retained Markdown, keeps optional reasons collapsed, and presents saved ratings as readable choices (#85, #86).

* Rate Orientation and Ask Rill outputs privately, with optional reasons and comments; review, revise, or withdraw saved ratings and their exact output snapshots (#74).

* Reading copies remain saveable when an extractor supplies a relative or nonstandard publication date, and local extraction diagnostics distinguish source HTTP failures from extractor failures (#78).

* Feed refresh tolerates unescaped `]]>` text in XML sources and rejects HTML directory pages mislabeled as XML (#78).

* Organize feeds into overlapping Groups, manage several feeds at once, and read feeds in any or all selected Groups. OPML preserves memberships and empty Groups when moving between Rill Libraries (#76).

* Folder buttons combine their feeds into one reading queue, with folder-scoped views and bulk read actions; individual feeds can be expanded beneath each folder (#75).

* Startup waits for the Library before announcing readiness and offers reload recovery after a delay; Ask Rill displays running and retry states, and dismissed dialogs restore keyboard focus (#50, #51).

* Scheduled polling uses Rill's bundled extractor and reports how many articles prepared or failed before reporting feed failures (#45).

* Mobile queues reveal Read and Save actions with a swipe or a named actions button, and reading supports previous/next swipes alongside visible buttons; page language, resize controls, and refreshing text have improved accessibility (#50).

* Feed refresh supports RSS 1.0 sources such as Nature and The Oatmeal, preserves saved entries when a feed is empty, and keeps the polling lock alive during slow fetches; scheduled polling prepares available articles before reporting source failures and includes failure counts by condition class in its logs (#45).

* Orientation lets its agent correct rejected source quotations before accepting a submission and correctly handles ellmer's typed card fields (#45, #50).

* Orientation reports failed evaluations with native error messages and expandable cause and backtrace details in the Reader session, offers an explicit retry, and preserves current selections while recovering; production traces distinguish reused attempts, failed requests, and published selections (#45, #50).

* Article preparation can use a bundled, versioned Defuddle CLI with Node.js or Deno, including Connect Cloud's Deno runtime, without installing JavaScript packages during reading (#45).

* Operational telemetry now separates article opening, database work, background queue and worker time, and extraction HTTP status and retries; Logfire traces correlate safe failure references without exporting article content, URLs, or raw exceptions (#45).

* Hosted startup no longer sends unnecessary database cancellation requests that can interrupt initialization; failed startup also closes its database pool (#45, #50).

* Articles open from saved or clearly labeled feed copies without waiting for extraction; full articles prepare in the background and after Feed refresh, with bounded retries and an explicit action to load a newer copy without changing an active reading or Ask Rill session (#45).

* Disabled Readers no longer keep Feeds eligible for scheduled polling; shared Feeds remain eligible while another active Reader follows them, and existing Subscriptions are preserved (#26).

* The hosted Library owner can review pending Access requests and approve an invited Reader into a separate, empty Library from the sidebar; approvals recheck the owner's identity and record who granted access (#26, #45).

* Today, This week, and This month now follow the browser's local calendar, show their date range and time zone, and keep Prepare on the same day; queue controls fit narrow sidebars and OPML import/export buttons share a consistent layout (#28, #47).

* Feed fallback copies preserve paragraphs, images, links, and lists, clearly identify themselves as feed copies, and offer a per-article retry; successful preparation upgrades selected fallbacks without deleting earlier copies or replacing browser captures (#28, #45).

* Today's Prepare action and `prepare_today()` now report safe failure details for each Feed Entry, distinguish extraction from storage failures, and provide references to content-free server logs while leaving missing copies retryable (#28, #45).

* Library refresh now runs in a separate background process with live progress, so you can keep reading, navigate, or close Manage feeds while new stories are fetched; refresh completion preserves your current reading and Subscription choices (#22, #28).

* Manage feeds now opens a searchable dialog with per-feed status, single-feed refresh, retry for failed feeds, existing-folder suggestions, and restoration of unsubscribed feeds; manual refresh records polling outcomes and reports only genuinely new stories (#28).

* `approve_reader_admission()` and `list_reader_admissions()` provide a privacy-safe operator workflow for admitting a verified identity to a new isolated Reader, while both hosted Auth0 adapters now record pending access requests and explain the next step to the invited person (#26, #45).

* Rill can now enforce an in-app Auth0 gate on public Shiny hosts such as Posit Connect Cloud, and the default branch can run due-Feed polling hourly under an explicit kill switch (#45).

* Hosted Rill now polls compact change fingerprints instead of repeatedly transferring Feed Entry and Document content, sharply reducing PostgreSQL network usage (#45).

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

* `poll_feeds()` backs off repeatedly failing Feeds from hourly retries to at most daily, while manual refresh remains immediate and successful checks restore the normal interval (#78).

* `poll_feeds()` refreshes only due shared Feeds, prevents overlapping runs, records durable per-Feed outcomes, tolerates isolated failures, and exits non-zero only for systemic errors or the configured failure threshold (#22).

* `read_opml()` and `write_opml()` import and export OPML 2.0 subscription lists, including nested feed folders; the Shiny app registers imports immediately under **Manage feeds** and leaves feed refresh as a separate action.
