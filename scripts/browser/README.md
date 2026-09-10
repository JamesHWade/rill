# Browser verification

Run from the repository root with R dependencies installed:

```sh
Rscript scripts/browser/app.R
```

In a second terminal:

```sh
npm ci --prefix scripts/browser --ignore-scripts
npm run check --prefix scripts/browser
```

The fixture binds only to loopback, clears database and provider credentials,
and creates separate bundled demo data for each session. `?stress=1` supplies
116 synthetic feeds, 150 entries, long titles, and long reading copies.
Google Chrome must be installed. `RILL_BROWSER_URL` can select another local
fixture port; the check rejects non-loopback hosts.

Results and screenshots go to `artifacts/responsive-audit/`. The suite checks
viewport transitions, real Chromium touch input, gesture exclusions, keyboard
actions, accessible names and contrast, native dialogs and errors, long code
blocks, dark mode, reduced motion, and connection recovery. Its 320 by 225
viewport represents the available CSS space at 400% zoom on a 1280 by 900
window; it does not certify browser zoom behavior or accessibility conformance.

The fixture-only `audit_error` and `audit_disconnect` inputs exercise native
Shiny feedback and actual session loss. They are not registered by the product.
No production accounts, feeds, or model services are used.

Run `node feedback.mjs` from `scripts/browser` to verify Reader feedback at 320,
390, and 1440 CSS pixels. Its `?feedback=fixture` mode seeds finished synthetic
responses and one other Reader's private rating. The checks exercise optional
ratings, revision, earlier responses, downloads, withdrawal, chained-dialog focus
restoration, contrast, and overflow. The exported JSON must exclude the other
Reader's record. Results and screenshots go to `artifacts/ui-followups-feedback/` (or `RILL_BROWSER_OUTPUT`).
This local fixture does not replace hosted identity or screen-reader testing.

To capture the 15 comparison states at phone and desktop widths:

```sh
cd scripts/browser
RILL_CAPTURE_LABEL=after node capture.mjs
```

For a baseline fixture on another local port, set `RILL_CAPTURE_LABEL=before`
and `RILL_BROWSER_URL=http://127.0.0.1:3877`. The validation report records the
baseline revision and fixture adaptation. Each run writes screenshots and axe
results to `artifacts/responsive-comparison/<label>/`. The fixture's delayed
startup, access decisions, and agent output are synthetic; only the separate
hosted observations in the report use a real account and model request.

Run `node pane-focus.mjs` to check reversible pane focus, restoration of native
sidebar widths and open states, Escape, unsent drafts, doubled root text size,
and 320-pixel reflow. Run `node tool-results.mjs` to check the public shinychat
Document display, pinned source identity, and exact JSON clipboard content.
The feedback suite also checks long Markdown, unbroken text, original-answer
copying, and doubled text size. Text enlargement is distinct from browser zoom;
the automated checks do not certify a screen reader or browser zoom behavior.

Run `RILL_LEGACY_MEDIA_QUERIES=true node pane-focus.mjs` to repeat the pane
workflow using Rill's legacy MediaQueryList listeners. Third-party component
media queries retain their normal browser APIs.

Run `RILL_BROWSER_URL=http://127.0.0.1:3876 node timeline.mjs` to verify the
article timeline at 320, 390, 430, 768, and 1440 CSS pixels, source-image display,
keyboard actions, touch reveal/commit, Undo, vertical scrolling, and doubled
text. Its `?timeline=fixture` mode supplies fictional article text and image
URLs. The fixture serves a generated landscape and an unavailable-image response
through the same session proxy used by the Queue; the generated landscape is test material, never a production
article image. Results are written to `artifacts/article-timeline/`.
