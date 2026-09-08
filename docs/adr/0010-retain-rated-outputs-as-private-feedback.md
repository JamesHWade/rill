---
status: accepted
---

# Retain rated outputs as private Reader Feedback

Reader Feedback records a helpful or not-helpful rating, optional reasons, and
an optional comment against the exact output presented for review. Rill freezes
that output and its available Agent Run, model, and policy provenance when the
Reader starts the feedback interaction. It persists the snapshot only when the
Reader submits a rating. A later regeneration, retry, or change in the current
Orientation cannot reassign that rating.

This is a narrow extension to ADR-0003's current-only Orientation policy. An
explicit rating preserves the rated output for evaluation, without archiving
every generated Orientation or making the snapshot a Reading Artifact. Source
Documents and generated interpretation remain distinct.

Feedback remains private to its Reader. The Reader may revise or withdraw it;
withdrawal removes the feedback and its retained snapshot. There is no implicit
cross-Reader aggregation, external export, automatic learning, or promotion to
Reader Memory. Review uses explicit examples and available version information;
missing feedback is unknown, not approval.

Streamed text for up to 50 attempts may remain in a browser session's server-side
feedback buffer after failure or interruption. This buffer disappears with the
session and is persisted only if the Reader rates that output. It does not change
terminal Agent Run cleanup or create a durable archive of unrated failed output.
