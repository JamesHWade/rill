# Rill

Rill is a personal, agent-native feed reader that keeps source material primary
while helping its reader decide what deserves attention and engage with it.

## Language

### Reader and reading

**Reader**:
An authenticated person with a private Library and reading history in Rill.
_Avoid_: User, actor, account

**Feed**:
A syndicated source of reading material that exists independently of any
Reader's decision to follow it.
_Avoid_: Subscription

**Feed Entry**:
A source-specific item published through a Feed. The same material published
through two Feeds remains two Feed Entries with distinct provenance.
_Avoid_: Story, Document

**Subscription**:
A durable Reader-owned membership that includes a Feed in their Library,
together with that Reader's organization and preferences. An inactive
Subscription preserves those choices for later restoration.
_Avoid_: Feed

**Library**:
A Reader's private collection of Subscriptions, Captures, reading-copy
selections, and reading state.
_Avoid_: Tenant, workspace, account

**Capture**:
A Reader-owned inclusion of an Original Source in their Library, backed by a
private immutable Document. It may refer to an accessible Feed Entry but never
creates a Subscription implicitly.
_Avoid_: Feed, Subscription, bookmark

**Orientation**:
A maintained editorial selection that identifies unread stories deserving the
Reader's attention and gives an inspectable reason for each. It changes only
when available evidence or reading state materially changes.
_Avoid_: Ranking, recommendation feed, daily summary

**User Engagement**:
An explicit conversation, request, or acceptance from the reader. Opening,
saving, revisiting, or scrolling through a story does not count by itself.
_Avoid_: Passive engagement, behavioral signal

**Conversation**:
A durable, reader-visible history of User Engagement and agent responses. It is
not hidden working context, an execution trace, or accepted knowledge.
_Avoid_: Session Context, trace, Reader Memory

**Connection Cue**:
A brief, source-linked suggestion that the current story relates to earlier
reading. It may invite engagement but does not itself create a durable outcome.
_Avoid_: Conclusion, synthesis

**Carry-forward**:
The transition from reading into a durable conclusion or follow-up after User
Engagement. It never occurs from passive behavior alone.
_Avoid_: Automatic follow-up, inferred conclusion

**Reading Artifact**:
A Reader-accepted Carry-forward outcome, such as a conclusion, briefing, open
question, or follow-up. It remains inspectable but does not become Reader
Memory or automatic Reader Context unless the Reader explicitly promotes it.
_Avoid_: Draft, Conversation, Reader Memory

**Reading Loop**:
Orientation followed by selection and reading, optional User Engagement for
understanding or connection-making, and an explicitly initiated Carry-forward.
_Avoid_: Agent workflow, chat session

### Agency and authority

**Action Proposal**:
A reader-visible preview of a bounded durable change an agent could make. It has
no effect until the Reader directly accepts it.
_Avoid_: Action, recommendation, tool call

**Approval**:
A Reader's direct request or acceptance authorizing one bounded outcome. It does
not authorize unrelated changes or future action outside standing maintenance.
_Avoid_: Blanket permission, tool permission

**Action Receipt**:
An inspectable Reading History record of an authorized action, its scope,
outcome, and reversal status. It is neither Reader Memory nor a Reading Artifact.
_Avoid_: Transcript, tool log, Reading Artifact

**Agent Run**:
One bounded execution attempt to produce a reader-visible agent outcome. It may
outlive a Reader's connection, never waits for Approval, and a retry creates a
new linked Agent Run.
_Avoid_: Conversation, Reading Loop, background task

### Evidence and interpretation

**Original Source**:
Material published outside Rill from which a Document is captured. It may
change independently of Rill.
_Avoid_: Document, Source Evidence

**Document**:
An immutable reading copy captured from an Original Source together with the
provenance and limitations of that capture.
_Avoid_: Original Source, live page

**Reading Copy Selection**:
A Reader's sticky choice of which accessible Document represents a Feed Entry
or Capture for reading. Without an explicit choice, the Reader follows the
current public reading copy.
_Avoid_: Document head, global selection

**Source Evidence**:
A specific passage in a Document used to inspect the support for an agent
statement.
_Avoid_: Citation, Original Source, model knowledge

**Supported Claim**:
An agent statement that faithfully quotes or paraphrases Source Evidence
without material inference. Support does not mean independent truth.
_Avoid_: Fact, verified truth

**Interpretation**:
An explicitly identified agent statement that explains or synthesizes beyond
what Source Evidence directly states.
_Avoid_: Supported Claim, conclusion

**Unsupported Gap**:
An explicit statement that the Source Evidence needed for an answer is missing
or insufficient.
_Avoid_: Guess, model knowledge

**Uncertainty**:
A plain-language qualification explaining limits such as incomplete coverage,
source disagreement, capture limitations, or inferential distance.
_Avoid_: Confidence score

**Reader Context**:
Reading History or Reader Memory used to explain why something was surfaced.
It does not support claims about a source or authorize Carry-forward.
_Avoid_: Source Evidence, passive authorization

### Knowledge and privacy

**Research Scope**:
The boundary of material an agent may consult for one outcome. It is limited to
the Library and its Documents unless User Engagement explicitly opens public
web research.
_Avoid_: Corpus, ambient web access

**Data Destination**:
A named runtime or service that receives task-relevant Rill content. It
distinguishes processing within the Rill installation from delivery to an
external provider.
_Avoid_: Local, backend

### Memory

**Reading History**:
A record of observable reading activity and visible decisions about agent
actions. It may contribute Reader Context but is neither User Engagement nor
Reader Memory.
_Avoid_: Engagement, preference, Reader Memory

**Reader Memory**:
A Reader-confirmed preference, intention, or accepted outcome preserved across
sessions for future use. It is created only by direct request or acceptance.
_Avoid_: Reading History, profile, training data

**Session Context**:
Task-scoped instructions, evidence, and working synthesis available within the
current Reading Loop. A Conversation may persist, but hidden Session Context
does not become Reader Memory without direct request or acceptance.
_Avoid_: Conversation, Reader Memory, Reading History

**Memory Proposal**:
An occasional, non-blocking invitation for the Reader to create or revisit
Reader Memory. It is not memory until accepted.
_Avoid_: Inferred preference, automatic memory

**Archive**:
A Reader's choice to keep Reader Memory or a Reading Artifact readable and
restorable while excluding it from automatic agent use.
_Avoid_: Forget, hide

**Forget**:
A Reader's choice to permanently remove Reader Memory and its derived copies.
_Avoid_: Archive, hide
