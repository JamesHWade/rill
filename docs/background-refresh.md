# Background feed refresh

Library, selected-feed, failed-feed retry, and optional startup refresh run in a
separate supervised R process. Fetching, parsing, and PostgreSQL writes stay off
Shiny's event loop. The process opens its own database pool; it never serializes
a live connection or initializes the app, reruns migrations, or recovers Agent
Runs. The existing transaction-scoped polling lock serializes it with scheduled
polling and other refreshes, and the existing polling tables retain outcomes.

The Shiny session checks a small local progress file every 250 ms while work is
active. All refresh triggers share one in-flight operation per session. Closing
Manage feeds or disconnecting does not cancel acquisition: the completion
callback collects the worker result and cleans up, but never updates a closed
session. The refresh is bounded to one hour. Hosting-process shutdown terminates
its supervised workers; a later polling run recovers any interrupted durable
run under the polling lock. This is not a durable job queue or a replacement for
the scheduled hourly poller.

Demo mode sends only the selected shared Feeds and their Entries to the worker.
On completion it merges source acquisition and poll outcomes, not a Library
snapshot. Subscriptions, reading state, private Documents, Captures, events, and
reading-copy selections never enter that payload or get replaced. A memory-store
lock prevents simultaneous refresh snapshots. PostgreSQL workers select active
Subscriptions directly from the shared database.

Workers load the same source checkout as the parent when running through
`pkgload`, and the same installed package directory otherwise. Logs and progress
live in a private temporary directory removed on completion or failure. Detailed
worker errors are not rendered to Readers; the UI offers a generic retry message.
