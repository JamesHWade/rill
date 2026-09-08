# Reader acceptance and hosting evidence

This record supports issues #24, #27, #45, and #51. It does not close their
remaining hosted or human acceptance criteria. Observations were collected on
September 7 in America/Detroit, September 8 in UTC.

## Delivered changes awaiting merge

[PR #79](https://github.com/JamesHWade/rill/pull/79), revision `5467e7c`,
normalizes extracted publication dates before storage, classifies extractor
HTTP failures, and backs off repeated scheduled Feed failures. All eight CI
checks passed, and the retry-message review was addressed. Production polling
must be checked after merge; local regressions do not prove deployed recovery.

[PR #80](https://github.com/JamesHWade/rill/pull/80), revision `ec673e8`, adds
Reader feedback for completed Orientation and Ask Rill output. All eight CI
checks passed on its initial head `ba7866f`; the subsequent preview review fix
passed 76 PostgreSQL-enabled feedback assertions. Ratings are not yet deployed and cannot substantiate the trial's
usefulness criteria retroactively.

## Browser refresh

The refreshed audit exposed insufficient light-theme contrast in the Add Groups
and Remove Groups controls: 2.98:1 against a required 4.5:1. Applying the existing
readable modal-button colors throughout the dialog fixes these controls. The
browser suite now also checks dark feed management and a focused Group action.

The expanded 46-state suite passed with no automated WCAG A/AA findings,
horizontal overflow, or browser errors. Its captures are recorded in
[the audit results](../../artifacts/responsive-audit/results.json). The focused
system UI asset suite passed 75 assertions; pkgdown and relative documentation
links passed. These checks do not certify assistive-technology conformance.

## Section acceptance matrix

The existing [browser harness](../../scripts/browser/README.md) uses 116
synthetic Feeds and 150 entries. It covers narrow and wide layouts, keyboard
controls, Chromium touch events, reduced motion, dark mode, and recovery states.
The [previous validation record](2026-09-07-reader-reliability.md) includes a
native Chrome 400% zoom pass and an authenticated hosted reading/Ask Rill path.

| Surface | Evidence available | Remaining acceptance |
| --- | --- | --- |
| App shell | Automated reflow, startup, slow startup, offline, disconnect, reload, and error checks; prior native 400% zoom pass. | Screen-reader announcements and physical Safari/iOS behavior. |
| Library | Large synthetic Library, narrow-layout navigation, feed-management dialogs, and automated accessibility checks. | Fresh complete navigation of the real large Library with assistive technology. |
| Queue | Six widths, long titles, keyboard actions, save focus, and Chromium touch disclosure/exclusions. | Physical touch and screen-reader sequence. |
| Orientation | Synthetic running/completed/interrupted states and feedback checks on PR #80. | Human usefulness rating over the trial and hosted feedback persistence after merge. |
| Reading | Long source text/code, region naming, keyboard navigation, dark mode, and prior hosted prepared-copy observation. | Physical Safari reading gestures and complete spoken traversal. |
| Ask Rill | Prior hosted grounded answer; local running/completed/interrupted states; feedback dialog checks on PR #80. | Fresh hosted failure/retry and feedback persistence after merge. |
| Dialogs | Focus restoration, keyboard dismissal, narrow reflow, light/dark contrast, and feedback keyboard checks. | Screen-reader announcement and focus sequence across every dialog. |
| Auth0 | Automated pending/denied fixtures and existing authenticated hosted session. | Fresh sign-in, sign-out, denial, two separate Reader browser profiles, and hosted forged-header checks. |

VoiceOver speech and traversal could not be reliably observed in the earlier
pass. This remains a human acceptance task. Browser automation also timed out
when attaching to the existing hosted Reader during this refresh; that is a
control failure, not evidence that the application itself failed.

## Trial participation

Read-only aggregate queries against the trial database found two active Readers
and three non-revoked external identity bindings spanning those two Readers.
No private source content, email addresses, or identity subjects were exported.
This establishes persisted admission, not two successful authenticated browser
sessions or isolation through the host.

| Local date | Article impressions | Entry-open events | Open-original events |
| --- | ---: | ---: | ---: |
| September 4 | 5 | 5 | 2 |
| September 5 | 21 | 18 | 2 |
| September 6 | 4 | 3 | 0 |
| September 7, partial day | 12 | 1 | 1 |

Counts include validation activity and are not unique articles or independent
human sessions. Four calendar dates do not establish five qualifying days in a
seven-day trial. No usefulness score, grounded-answer audit rate, or complete
provider-spend total is inferred from these events.

## Current hosting evidence

The Render dashboard showed one web service, `rill-render-spike`, and no Render
cron job in its project. The last successful deployment was September 4,
revision `d61f9b0`, using this immutable image:

```text
ghcr.io/jameshwade/rill@sha256:d5539e210a8457c96b7d716576b798566e006f54cef0d9dc0d95ba1349584ffb
```

The service was on Free compute: 0.1 CPU and 512 MB RAM. Its dashboard warned
about sleeping and delayed startup. No CPU or memory observations were available
in the inspected 48-hour metrics view. The public service was not awakened.
The dashboard offered these monthly web-service compute prices:

| Compute | CPU | RAM | Monthly web compute |
| --- | ---: | ---: | ---: |
| Free | 0.1 | 512 MB | $0 |
| 0.5c-512mb | 0.5 | 512 MB | $7 |
| 1c-2g | 1 | 2 GB | $25 |
| 2c-4g | 2 | 4 GB | $85 |

These exclude database, model, identity, cron, workspace, and transfer charges.
They are dashboard observations, not a sizing benchmark. See Render's
[compute plans](https://render.com/docs/compute-plans) and
[pricing](https://render.com/pricing) for current terms. Render cron jobs are
billed for active compute with a minimum of $1 per job per month; the existing
GitHub Actions scheduler is a separate deployment choice.
[Render cron documentation](https://render.com/docs/cronjobs).

Continue the personal trial on Connect Cloud while preserving Render as the
container preview candidate. The current Render spike is not a controlled
performance comparison and is too old to establish current product readiness.
The actual Connect Cloud account tier, current deployment revision, and billing
were not refreshed in this pass. Its documented tiers differ in compute and
sharing; plan availability is not evidence that this account has those features.
[Connect Cloud plans](https://docs.posit.co/connect-cloud/user/account/plans.html).

## Next completion gates

1. Merge the reviewed reliability and feedback changes, then verify the exact
   deployed revision, scheduled polling/preparation outcomes, and visible Reader
   behavior. Record access denials separately from product failures.
2. Complete the [isolated hosted proof](../deployment/hosted-proof.md) with two
   invited test identities. Existing persisted bindings alone do not satisfy it.
3. Complete the human checks in the section matrix and attach results to #51.
4. Finish the seven-day trial in #45, including usefulness, grounded answers,
   and provider spend. Then run the two-week invitation-only beta required by
   #27 before considering broader signup.

Issue #26 is closed, but its [verification document](../agents/cross-reader-verification.md)
explicitly establishes an automated foundation. Hosted lifecycle, deletion, and
backup-restoration outcomes remain unverified; a closed issue is not substitute
evidence. No merge, deployment, new invitation, or paid resource change was made
by this acceptance refresh.
