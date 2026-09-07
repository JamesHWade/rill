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
