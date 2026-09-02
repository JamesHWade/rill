# Orientation browser check

Use this manual smoke check after changing the Orientation entry surface. It
keeps browser verification reproducible without adding a browser-automation
dependency to the package.

## Demo flow

1. Clear `DATABASE_URL` and run `shiny::runApp(rill::rill_app())`.
2. At 1280 by 900 pixels, confirm that Orientation shows zero to three cards,
   Source Evidence and the evaluated basis start closed, and the ordinary
   unread queue has a visible escape.
3. Inspect Source Evidence and confirm that it shows an immutable Document ID,
   Original Source, acquisition metadata, and limitations separately from the
   agent Interpretation.
4. At 390 by 844 pixels, confirm that the page has no horizontal overflow and
   each Source Evidence summary has a 44-pixel target.
5. Open a card. Confirm that the normal reader opens with
   `data-selection-surface="orientation"`, then return to Orientation and check
   that focus lands on its heading.
6. Dismiss each remaining card. After the final dismissal, confirm that the
   compact Orientation status appears above the usable queue, the offscreen
   reader pane is `inert` and `aria-hidden="true"`, and focus lands on a visible
   queue card.

## Consent flow

1. Use an empty temporary PostgreSQL database and set
   `RILL_ORIENTATION_ENABLED=true`, an external `RILL_AGENT_MODEL`, its explicit
   endpoint in `RILL_AGENT_BASE_URL`, and an inspectable
   `RILL_AGENT_POLICY_URL`. Do not configure provider credentials for this
   smoke check.
2. Open the Data Destination settings and confirm that the provider, exact
   endpoint, policy link, and unread-status disclosure are visible.
3. Choose **Enable automatic Orientation**. Confirm that the page contains
   exactly one element with `role="dialog"`, its `aria-labelledby` target is
   **Enable automatic Orientation?**, and no provider request occurs before
   confirmation.
4. Dismiss once with Escape and once with **Not now**. Both paths must return
   focus to the Data Destination summary.
5. Confirm that the browser console has no errors or accessibility warnings.

Do not choose the confirmation action during this smoke check: confirmation is
the boundary that authorizes sending bounded unread reading copies to the named
Data Destination.
