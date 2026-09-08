# Complete the isolated hosted proof

Use this runbook for [issue #24](https://github.com/JamesHWade/rill/issues/24)
before the invited beta in #27. Configuration details remain in
[Render](render.md) and [Connect Cloud](connect-cloud.md). Record the exact
revision, image digest where applicable, deployment ID, migration state, UTC
time, and browser version alongside every result.

## Isolate the preview

Use a fresh preview database with synthetic source material and two invited test
Readers. Do not copy personal captures, credentials, or identity bindings into
it. Keep the active personal trial database separate.

This is a runtime constraint: `rill_app()` calls
`store_interrupt_agent_runs(..., recovery = "process_restart")` at startup.
Starting a second web process against the personal database can interrupt work
owned by the first process. Connect Cloud and Render must not run web processes
against that database simultaneously. A paid instance does not remove this
constraint. Keep one Shiny web instance for the preview.

Use the same immutable image for Render web and poll roles. Use a dedicated
Auth0 application/callback configuration for the preview, with invited signup
only. Store secrets in the host's secret settings and keep them out of screenshots
and acceptance records. Confirm `/ready` and the public proxy-only listener
before beginning Reader checks.

## Execute the acceptance sequence

| Step | Action | Required observation |
| --- | --- | --- |
| Identity | Sign in as A and B in independent browser profiles. Admit B through the operator workflow. | Distinct stable Reader bindings; B starts with an empty Library. Bootstrap allowlisted subjects intentionally share one Reader and do not test this criterion. |
| Isolation | A subscribes, saves a story, and captures a synthetic private document. B follows the same public Feed and attempts A's private identifiers. | Shared acquisition, separate organization/state, and denial of A's capture/selected copy. |
| Gate | Use a signed-out profile and a verified but unadmitted identity. Exercise the public URL with a forged identity header; test that the Shiny listener is unreachable externally. | Auth0 redirect or access-request/denial surface; no Library leak. |
| External poll | Run one due-Feed pass outside Shiny; overlap a second worker while the first holds the polling lock. | One fetch per shared Feed, second worker skips safely, both Readers see shared new entries. |
| Restart | Save synthetic state for both Readers, restart the preview, and reconnect both browsers. | Both identities and Libraries persist; interrupted agent work is reported honestly. |
| Deploy | Deploy a new pinned revision and reconnect both profiles. | Expected WebSocket interruption, recovery to the same Readers, and no lost persisted state. |
| Sign-out/revocation | Sign out; revoke or suspend test Reader B and attempt access again. | Fresh authentication is required after logout; B loses access without affecting A. Record restoration and credential behavior separately. |
| Rollback | Restore the prior known-good image/configuration after confirming migration compatibility. | Readiness and both synthetic Libraries remain correct. Do not reverse migrations or overwrite the database blindly. |

Render does not impose a fixed WebSocket duration, but a deployment or instance
shutdown terminates connections. Its default shutdown grace period is 30 seconds
and can be configured up to 300 seconds. Test the application's actual reconnect
behavior; these platform guarantees do not establish it.
[Render WebSockets](https://render.com/docs/websocket).

Choose one enabled scheduler. Render cron prevents overlap within a job, but a
manual trigger cancels an active run; do not use that action to demonstrate
Rill's concurrent-worker lock. Use two independent preview poll processes for
that test. Bound poll execution to the existing 30-minute GitHub Actions limit
or an equivalent configured deadline. Render's platform maximum alone is too
long for this acceptance target.
[Render cron jobs](https://render.com/docs/cronjobs).

## Decide beta readiness from evidence

Attach pass/fail evidence for each row to #24. Keep deletion, export, backup
restoration, and purge reapplication separate: the current local suite does not
implement or certify all of those lifecycle obligations. Track the remaining
work against [ADR-0008](../adr/0008-govern-reader-data-lifecycle-and-capacity.md) and the
[cross-Reader evidence matrix](../agents/cross-reader-verification.md).

For #27, record the selected runtime's current monthly compute estimate,
database/identity/model limits, provider-spend stop threshold, alert owner,
scheduler kill switch, backup policy, and tested rollback. Confirm the complete
seven-day personal trial before beginning the required two-week invited beta.
Do not treat zero automated accessibility findings, green CI, persisted Reader
bindings, or a healthy readiness endpoint as substitutes for these gates.
