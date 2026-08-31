# Issue tracker: GitHub

Issues and specifications for this repository live as GitHub issues. Use the
`gh` CLI and infer the repository from the configured Git remote.

## Conventions

- Create, read, comment on, label, assign, and close issues with `gh issue`.
- Pull requests are not a triage request surface.
- When a skill says "publish to the issue tracker," create a GitHub issue.
- When a skill says "fetch the relevant ticket," read the issue and its
  comments.

## Wayfinding

A Wayfinder map is an issue labelled `wayfinder:map`. Its child issues use one
of `wayfinder:research`, `wayfinder:prototype`, `wayfinder:grilling`, or
`wayfinder:task`.

Use GitHub sub-issues and native issue dependencies where available. Otherwise,
link children from the map using task lists and record blockers in each child
body.

Claim work by assigning the issue to the current developer. Resolve it by
recording the answer or outcome, closing the child, and updating the map's
Decisions-so-far section.
