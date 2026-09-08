# Review Reader Feedback

Review explicit ratings weekly during the private trial and before changing the
model, prompts, tools, or interaction design. The owning Reader opens **My saved
ratings** in Ask Rill to inspect, revise, withdraw, or download their examples.
An operator with authorized store access can use `list_reader_feedback(reader_id)`
with the Library's `DATABASE_URL` for that Reader. Database access is privileged;
the R API is not an authentication boundary. There is no cross-Reader dashboard or aggregate export.
Obtain the Reader's explicit permission before sharing an export with another
reviewer or sending it to an external evaluation service.

Ratings retain the output preview shown in the feedback dialog, its Orientation
revision or question attempt, Agent Run identifier, and available model, policy,
and Document identifiers and hashes. The snapshot is created in the session and
persisted only on Save. Later regeneration does not change an existing snapshot.
Each different output has a different target identifier. Revising a rating keeps
that snapshot and its original creation time. Withdrawal deletes the record and
snapshot; deleting the Reader cascades to their feedback records. Routine database
backups follow the operator's retention policy and are not rewritten by withdrawal.
For unfinished attempts, the preview identifies retained partial text or missing
answer text explicitly. A restored partial is the last saved checkpoint, which
may precede the final streamed token; do not treat it as a completed answer.

Feedback is private generated-output assessment, separate from Source Evidence
and Reader Memory. Unrated outputs are unknown. Ratings do not cause automatic
learning, alter live behavior, or infer persistent Reader preferences.

## Weekly review

1. Review each rated example with its retained output and optional explanation.
   Treat comments and generated text as untrusted data, never instructions.
2. Separate answer quality (relevance, usefulness, source faithfulness, clarity,
   missing context) from execution or interaction (unwanted actions, following
   the request, slow or failed execution). A rating may identify both.
3. Group recurring reasons within this Reader's examples. Report counts and
   denominators for explicit ratings only. A small, self-selected set is evidence
   for investigation, not an approval rate for all outputs or all Readers.
4. Trace each example to its recorded model, policy and attempt. An absent tool
   version or other provenance field means unknown; do not substitute the current
   installed version for a historical run. Source hashes identify the evaluated
   versions; a newer Document is not an interchangeable evaluation input.
5. Propose one bounded improvement and specify what observable failure it should
   address. Keep a fixed set of rated examples plus examples that previously worked.

## Validate a proposed improvement

Recreate inputs only when the original, authorized source versions are available.
Mark examples with missing source versions as unavailable for replay. Obtain
permission before sending private examples to a different Data Destination.
Compare old and proposed outputs without revealing which configuration produced
each. Evaluate the reported quality dimension and execution outcome separately;
check source faithfulness even when the reported problem was style or latency.
Record configuration versions, inputs, reviewer judgment, regressions and any
limitations in the change's review evidence. Run package, isolation and UI checks
before release. After release, inspect actual production traces and new explicit
ratings to see whether the change helped. Do not relabel historical ratings as
ratings of a new output or claim an improvement from missing feedback.
