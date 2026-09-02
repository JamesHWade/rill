# Agent-native reading interaction prototype

> Preserved design prototype for issue #10. Its source lives under
> `prototypes/agent-native-reading-interaction/` and is excluded from the
> installed Rill application.

## Question

What interaction model makes agents feel native to Rill's queue and reader on
desktop and mobile without turning Rill into blank chat or obscuring the source?

## Run it

```sh
Rscript prototypes/agent-native-reading-interaction/app.R
```

Open <http://127.0.0.1:7348/?variant=D>. Use the fixed bottom switcher, the left
and right arrow keys, or `?variant=A`, `?variant=B`, `?variant=C`, and
`?variant=D`.

Run the prototype's focused checks with:

```sh
Rscript prototypes/agent-native-reading-interaction/test.R
```

The prototype uses bundled demo data and holds interaction state in the browser.
Agent actions, approvals, and messages are inert. The maintained implementation
of the accepted direction is tracked in
[#32](https://github.com/JamesHWade/rill/issues/32). This preserved version also
incorporates the correctness and accessibility findings from the original
study's review.

## Variants

### D: Recommended hybrid

Orientation occupies the otherwise-empty reader as a maintained editorial path
through the ordinary queue. It names one question, distinguishes agent
interpretation from the underlying source titles, and sequences an anchor
Document with a contrasting Document. When the reader pane is too narrow to
share with the queue, Orientation becomes the entry surface and offers an
explicit escape to the full unread queue. In the reader, a small source-linked
Connection Cue offers bounded starts. Conversation opens only after User
Engagement, as a right rail on desktop or a bottom sheet on mobile. It begins
with the current Document, visible Research Scope and Data Destination, and
specific questions rather than blank chat. Carry-forward is a separate Action
Proposal inside that Conversation.

This is the recommended composition: A supplies the Reading Loop, B supplies a
clear Conversation and approval surface, and C contributes the principle that
the source remains unchanged until help is requested.

### A: Editorial seam

Orientation is editorial matter at the head of the ordinary queue. In the
reader, a compact Connection Cue sits between the article header and Document;
Conversation expands in place only after User Engagement.

This model tests whether an agent can feel intrinsic to reading without gaining
a separate destination.

### B: Companion rail

Orientation and Conversation occupy a dedicated rail beside Rill. The rail
switches from queue-level editorial context to Document-level help. On mobile it
becomes an explicitly opened bottom sheet.

This model tests the clarity of a stable agent home against the cost of reducing
space for the queue and source.

### C: Context palette

The ordinary queue and reader remain visually unchanged. A small context-aware
launcher opens Orientation or Document-scoped actions in a palette. The palette
offers bounded starts rather than a blank chat box.

This model tests whether minimal visual presence preserves source primacy at the
cost of making Orientation and Connection Cues easier to miss.

## Evaluation walkthrough

The study evaluated each variant with the same path:

1. Begin in the queue and find the maintained Orientation.
2. Open the source-boundary story from the agent surface.
3. Inspect Source Evidence and its provenance disclosure.
4. Start a Document-scoped question.
5. Find the Carry-forward affordance and check whether it previews a bounded
   outcome rather than silently changing product state.
6. Repeat at a narrow mobile viewport.

The preserved prototype completes that preview in every variant. The original
study rendered it only in B and D; the missing A and C previews were retained as
review findings and corrected without changing the selected direction.

Judge source primacy, discoverability, continuity from queue to reader, the
difference between Conversation and Carry-forward, and whether the layout still
feels like Rill rather than a generic chat product.
