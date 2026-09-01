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

**Subscription**:
A Reader's decision to include a Feed in their Library, including their own
organization and preferences for that Feed.
_Avoid_: Feed

**Library**:
A Reader's private collection of Subscriptions and the reading state associated
with them.
_Avoid_: Tenant, workspace, account

**Orientation**:
The opening agent outcome that identifies which unread stories deserve the
reader's attention and explains why before the reader chooses what to open.
_Avoid_: Ranking, recommendation feed

**User Engagement**:
An explicit conversation, request, or acceptance from the reader. Opening,
saving, revisiting, or scrolling through a story does not count by itself.
_Avoid_: Passive engagement, behavioral signal

**Connection Cue**:
A brief, source-linked suggestion that the current story relates to earlier
reading. It may invite engagement but does not itself create a durable outcome.
_Avoid_: Conclusion, synthesis

**Carry-forward**:
The transition from reading into a durable conclusion or follow-up after User
Engagement. It never occurs from passive behavior alone.
_Avoid_: Automatic follow-up, inferred conclusion

**Reading Loop**:
Orientation followed by selection and reading, optional User Engagement for
understanding or connection-making, and an explicitly initiated Carry-forward.
_Avoid_: Agent workflow, chat session

### Evidence and interpretation

**Original Source**:
Material published outside Rill from which a Document is captured. It may
change independently of Rill.
_Avoid_: Document, Source Evidence

**Document**:
An immutable reading copy captured from an Original Source together with the
provenance and limitations of that capture.
_Avoid_: Original Source, live page

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
Reading history or preferences used to explain why something was surfaced. It
does not support claims about a source or authorize Carry-forward.
_Avoid_: Source Evidence, passive authorization
