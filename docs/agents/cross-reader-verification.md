# Cross-Reader isolation and recovery verification

This is the automated foundation for [issue #26](https://github.com/JamesHWade/rill/issues/26).
It does not complete the authenticated hosting proof in
[issue #24](https://github.com/JamesHWade/rill/issues/24).

## Run the suite

Point `RILL_TEST_DATABASE_URL` at a disposable PostgreSQL database whose role can
create schemas, then run from the package checkout:

```r
testthat::test_local()
```

The tests create randomly named schemas, apply the real migrations, and remove
their schemas and connections on exit. Use synthetic Readers and source material.
Do not point the suite at a personal Library or a hosted trial database.

The PostgreSQL CI job runs the entire suite with its PostgreSQL 16 service.
Previously, its file-name filter excluded the Reader identity, Library, and
Document contracts. Running all tests also includes database cases in files that
do not have a `postgres` suffix. The job checks that the database URL is configured
before running; connection failures fail the tests. Other platform jobs can still
skip PostgreSQL cases when no database is configured.

## Evidence boundaries

| Contract | Automated evidence | Remaining evidence |
| --- | --- | --- |
| Stable Reader identity | `test-reader-identity-postgres.R` exercises admission, exact identity bindings, revocation, and concurrent identity operations. | Two real invited identities completing Auth0 sign-in on the selected host. |
| Shared acquisition and isolated Libraries | `test-reader-library-postgres.R` exercises shared Feeds, separate folders, titles, state, historical pins, and subscription-revocation races. `test-polling.R` verifies one refresh per shared Feed. | Observe scheduled polling outside the hosted Shiny process with two Readers. |
| Private Documents and credentials | `test-reader-documents-postgres.R` exercises private Capture ownership, selection, guessed identifiers, credential ownership, and legacy migration. | Repeat capture and forbidden-access checks through the authenticated preview. |
| Fresh database connections | `test-reader-recovery-postgres.R` closes the initial pool, opens independent pools, resolves both identities again, and checks persisted organization, read/save state, events, credentials, and public/private selections. | A real process restart, WebSocket reconnect, and intentional deploy disconnect. |
| Suspension | The recovery test uses real identity/store code with two `MockShinySession` objects and verifies that disabling one Reader closes only that session and denies that Reader's capture credential. Polling tests cover both storage backends, preserving Subscriptions while excluding disabled Readers from eligibility. | Hosted session termination, cancellation of active agent work, permanent credential revocation across restoration, and operator restoration. |
| Poller overlap and interrupted work | `test-polling-postgres.R` exercises a separate connection holding the polling lock, overlap rejection, and interrupted-run recovery. | Observe the same behavior across hosted workers and deployment. |
| Deletion and backup recovery | Not established by these tests. ADR-0008 defines the required lifecycle. | Implement and exercise pending deletion, export completion/failure, purge, shared-source preservation, provider outcomes, and reapplication of purges after backup restoration. |

The suspension polling regression must retain a shared Feed while a second active
Reader follows it, exclude Feeds followed only by disabled Readers, and preserve
the Subscription records. It covers both active-Feed enumeration and due-Feed
selection; neither path may rely on Subscription status alone.

## Hosted completion record

Before closing #26, attach evidence against the exact deployed revision using two
synthetic invited Readers and separate browser profiles. Record the runtime,
deployment identifier, migration state, browser versions, test time, and outcome
for each remaining check above. Keep tokens, identity-provider secrets, private
source content, and real Reader identifiers out of the issue record.

Record observed failures and missing lifecycle capabilities as remaining work.
Passing the local database suite must not be reported as hosted isolation,
WebSocket recovery, deletion, or invited-beta readiness.

## Local validation on September 5, 2026

Validated the working changes based on `94ec144` using R 4.6.1 and a disposable
PostgreSQL 16.11 instance on macOS:

- The polling regression failed in both storage backends before the fix and
  passed afterward.
- The full `testthat::test_local()` run passed 2,386 assertions with zero failures,
  errors, warnings, or skipped tests, with PostgreSQL enabled.
- `devtools::check(args = c("--no-manual", "--no-tests"))` reported zero errors,
  warnings, and notes. Tests ran separately in the full PostgreSQL-enabled suite.
- `air format . --check`, `pkgdown::check_pkgdown()`, and `git diff --check` passed.

This record covers local validation. GitHub CI and the hosted completion matrix
have not been exercised for these changes.
