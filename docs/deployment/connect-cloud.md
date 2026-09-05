# Run a real-use trial on Posit Connect Cloud

This deployment is an invitation-only product trial. It asks whether hosted
Rill is useful for daily reading with real Feeds, Orientation, and Ask Rill. It
does not replace the Render/two-Reader isolation proof in issue #24 or change
the proposed invited-beta architecture in ADR 0001.

Posit Connect Cloud Free content remains public at the platform edge. Rill
therefore enforces Auth0 inside the Shiny application. The public URL, initial
Shiny connection, application shell, and static assets remain reachable. Rill's
Reader server does not start, query the Library, or make Agent calls until the
OIDC token's exact issuer and `sub` resolve to a Reader. A verified identity
without a binding creates one pending access request while remaining outside
every Library.

## Prepare the services

Use the fresh Neon project provisioned for this trial and keep its pooled
connection string with `sslmode=require`. The Connect Cloud deployment and the
Render spike may reuse this database sequentially, but must never be live
against it at the same time. Starting either web process interrupts unfinished
Agent Runs as restart recovery, including runs started by another process.
Suspend one deployment and confirm its sessions and Agent Runs have stopped
before attaching the database to the other. Keep both deployments on a
compatible schema revision, use the same `RILL_ACTOR_ID`, and configure only
one scheduled Feed poller. Do not reuse or migrate the local database in place.

Create a dedicated OpenAI project and API key. Configure a hard monthly spend
limit of USD 100 for that project. Rill also bounds every Ask Rill run to USD 2
and every Orientation run to USD 0.50.

In Auth0, create a Regular Web Application. For this prototype, the Connect
Cloud deployment and the Render spike may use the same Auth0 application.
Disable public sign-up for its database connection and leave automatic account
linking off. Use stable top-level URLs such as:

```text
https://rill.share.connect.posit.cloud/
https://rill.example-render-domain.com/
```

Add both exact URLs to **Allowed Callback URLs** and **Allowed Logout URLs**.
Do not use wildcard or ephemeral preview URLs. Both deployments share
`AUTH0_DOMAIN`, `AUTH0_CLIENT_ID`, and `AUTH0_CLIENT_SECRET`, but each sets
`AUTH0_REDIRECT_URI` to its own exact public URL. Use the same
`RILL_ALLOWED_OIDC_SUBJECTS` value on both deployments for the bootstrap
Reader. Sign in once, then copy that Reader's exact Auth0 `user_id`/OIDC `sub`
from the Auth0 user record. Rill compares it case-sensitively and never uses
email as an identity key. Additional verified identities request separate
Readers through the app. Split the Auth0 clients before expanding beyond this
bounded prototype so one deployment can be rotated or revoked independently.

## Configure Connect Cloud

Connect the Rill GitHub repository, select `app.R`, and claim the top-level app
URL before configuring Auth0. Set all identity variables before adding the
Neon connection string so a persistent Library is never attached to an
ungated application.

Configure these environment variables on the content:

```text
RILL_ENV=production
RILL_ACTOR_ID=james
RILL_IDENTITY_MODE=auth0
RILL_ALLOWED_OIDC_SUBJECTS=<exact Auth0 sub>
AUTH0_DOMAIN=<tenant>.us.auth0.com
AUTH0_CLIENT_ID=<Auth0 client ID>
AUTH0_CLIENT_SECRET=<Auth0 client secret>
AUTH0_REDIRECT_URI=https://rill.share.connect.posit.cloud/
DATABASE_URL=<Neon pooled URL ending in sslmode=require>
RILL_AGENT_MODEL=openai
OPENAI_API_KEY=<dedicated project key>
RILL_AGENT_POLICY_URL=https://platform.openai.com/docs/models/default-usage-policies-by-endpoint
RILL_ORIENTATION_ENABLED=true
DEFUDDLE_BACKEND=hosted
```

Leave `RILL_CAPTURE_TOKEN` unset for this trial. The imported Library contains
no private Captures, and browser capture is outside the trial boundary.

The committed `manifest.json` describes the R runtime and package dependencies.
Regenerate it whenever application files or dependencies change. The helper
also pins the manifest to R 4.6.0, the newest R version currently supported by
Connect Cloud:

```sh
Rscript --vanilla scripts/write-connect-manifest.R
```

## Configure hourly polling

The `Poll feeds` GitHub Actions workflow runs at minute 17 of each hour. Add
these repository settings:

- Secret `RILL_DATABASE_URL`: the same Neon pooled connection string.
- Variable `RILL_ACTOR_ID`: the same opaque Reader identifier used by the app.
- Variable `RILL_POLLING_ENABLED`: `true` to enable the job; any other value is
  the kill switch.
- Optional variables `RILL_POLL_INTERVAL_MINUTES` and
  `RILL_POLL_FAILURE_THRESHOLD`; they default to `60` and `5`.

The workflow needs neither Auth0 nor OpenAI credentials. It acquires Rill's
PostgreSQL polling lock, refreshes only due Feeds with active Subscriptions,
records per-Feed outcomes, and exits nonzero only for a systemic error or the
configured failure threshold.

## Diagnose startup failures

Distinguish dependency installation from the application worker's startup log.
The September 5, 2026 failures in publications 16 and 17 occurred after the
dependencies loaded, with `canceling statement due to user request`, followed
by a checked-out pool object warning. Publication 17 used `d207d5a`; the same
failure in publication 16 preceded that commit.

Rill's migration setup previously used `dbExecute()` for two `SELECT`
statements. With RPostgres 1.4.10, clearing those unread results sends database
cancellation requests. A connection intermediary can delay a cancellation until
the next query has started. Startup now consumes both results with
`dbGetQuery()` and closes the pool or store if later initialization fails.
Schema failures still stop startup.

The regression fixture in `tests/testthat/fixtures/postgres-cancel-proxy.py`
reproduces delayed cancellation against a disposable loopback PostgreSQL
database. Before the fix, the reproduction failed on its second startup with
the same cancellation error and pool warning. After the fix, 100 consecutive
startups succeeded without sending cancellation requests. The package suite
also covers failed pool and application initialization cleanup. These results
establish a reproducible mechanism; they do not prove the hosted cause or a
successful Connect deployment.

After publishing the fix, verify the source commit and a fresh worker startup,
then open the authenticated Library and reload it. Record the publication,
startup log outcome, and Library result in #45 and #50 before treating the
hosted failure as resolved. The manual assistive-technology checks in #50
remain separate from database startup verification.

## Diagnose preparation failures

When Today reports that reading copies could not be prepared, its Preparation
details dialog lists the affected Feed Entries, failure stage, and reference.
The details can be reopened beside Prepare until the next preparation attempt
or the end of the session. Retrying preserves ready copies and attempts missing
copies again.

Search the Connect runtime log for `article.prepare_failed` and the reference.
These records contain only the reference, stage, diagnostic code, known error
type, HTTP status when available, and extraction backend. They are emitted to
stderr even without an OpenTelemetry exporter. Feed Entry titles, URLs, credentials,
source content, and raw exception text are deliberately excluded from logs.
The Reader sees only Feed Entries from their own Library; diagnostic details are
not written into the reading-behavior event payload.

An extraction failure means no usable copy was built; a storage failure means
the copy was built but saving it failed. A Library failure means the batch could
not load its input. Use these distinctions to investigate the failing boundary;
the summary alone does not establish an extraction-service outage.

## Verify the boundary

Before importing the real Library, verify all of the following:

1. An unauthenticated top-level visit redirects to Auth0.
2. The allowlisted identity opens an empty Library.
3. Another valid Auth0 identity receives an access-request dialog, creates one
   pending admission, and cannot invoke any Rill action.
4. As the hosted owner (the Reader bound to `RILL_ACTOR_ID`), open **Access
   requests** in the Library sidebar, select the invited person, and choose
   **Approve access**. Other Readers must not see the control. The owner identity
   is revalidated before listing requests and approvals, and the owner's Reader
   ID is recorded in the audit log. Alternatively, in an R process configured
   with the deployment's `DATABASE_URL`, approve the opaque request ID:

   ```r
   requests <- rill::list_reader_admissions()
   rill::approve_reader_admission(
     requests$request_id[[1]],
     responsible_id = "operator:james"
   )
   ```

5. After reloading, the approved identity opens a new empty Library and cannot
   access the bootstrap Reader's Library.
6. Sign out clears the Auth0 session and requires a fresh login.
7. A process restart reconnects to the same empty Neon Library.
8. A manual `Poll feeds` workflow run succeeds against that database.

Only then import Subscriptions, Feed Entries, current reading state, and public
Documents. Exclude credentials, Captures, Reading History from before the
trial, and historical Agent Runs.

## Record the seven-day result

Use issue #45 as the trial record. Add one short note per day covering whether
Rill was used voluntarily, Feed freshness, useful or failed Orientation and
Ask Rill interactions, and any auth, privacy, or durability failure. Rill's
new interaction events provide the quantitative evidence.

Success requires use on at least five of seven days, consistently fresh Feeds,
useful Orientation and grounded Ask Rill answers, and no authentication bypass,
privacy failure, or durable data loss. If successful, keep the deployment and
database. If rejected, export the issue and relevant Rill events, disable
polling, and remove the trial cloud resources.
