# Rill

Rill is a personal, agent-native feed reader that keeps source material primary
while helping its reader decide what deserves attention and engage with it.

## Language

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
