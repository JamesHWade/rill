---
status: accepted
---

# Use immutable forward-only PostgreSQL migrations

Rill replaces its cumulative startup schema script and ad hoc R migrations with
ordered, immutable SQL migrations recorded in PostgreSQL. Short, transactional
migrations apply automatically at startup under an advisory lock; long or
destructive changes require an explicit maintenance release with a verified
backup. This keeps private single-process deployment simple without hiding
schema drift behind repeated `IF NOT EXISTS` statements.

## Consequences

- The existing `001_init.sql` becomes the immutable baseline. Rill verifies and
  stamps an existing compatible database before applying later migrations; a
  fresh database applies the complete ordered sequence.
- A migration ledger records each identifier, checksum, and application time.
  Changed checksums, unknown newer migrations, or a failed migration stop
  startup rather than allowing partially compatible operation.
- Ordinary migrations are forward-only, transactional, short, and compatible
  with the preceding release. Risky changes use expand, migrate, and contract
  releases instead of down migrations.
- The ephemeral in-memory store always starts at the current model version. It
  has no migration history and must pass the same store-behavior contract tests
  as PostgreSQL.
- Rill migrations never modify Graft's schema. Graft remains responsible for
  its own storage compatibility and evolution.
