# Connect Cloud real-use trial source

This throwaway operator script answers one question from issue #45: can the
hosted product trial start with the real public Library without migrating the
local database in place or copying private and historical records?

The script opens the source PostgreSQL database in a repeatable-read,
read-only transaction. It copies only one Reader's Subscriptions, their shared
Feeds and Feed Entries, current reading state, public Documents, and public
document heads. It never reads events, Agent Runs, Capture credentials, private
Documents, or orientation history. The destination must be a fresh database;
any existing product rows abort the transaction.

Load the five required values through the normal secret-management flow, check
them carefully, and run:

```sh
RILL_SOURCE_DATABASE_URL=<local source URL> \
DATABASE_URL=<fresh Neon URL> \
RILL_SOURCE_READER_ID=<local Reader ID> \
RILL_ACTOR_ID=<trial Reader ID> \
RILL_COPY_CONFIRM=copy-public-library \
Rscript --vanilla prototypes/connect-cloud-real-use/copy-library.R
```

The source and target URLs must differ. Do not put their values in a committed
file or issue comment.

Set `RILL_COPY_DRY_RUN=true` to inspect only the selected row counts. A dry run
does not require `DATABASE_URL` or `RILL_COPY_CONFIRM` and never opens a target
connection.
