# Rill UI follow-ups

Implemented locally on `codex/ui-demo-followups-20260908`, based on main at
`f0d66f21a438fc1ef659a562911386f5eb1d3d59`.

## Changes

- **#82 — Scalable text:** rem-based reading and interface typography, a 16-pixel default base, and larger chat/composer text. Long chat regions are labelled and keyboard-focusable.
- **#83 — Pane focus:** Focus Reading, Focus Ask Rill, and Restore layout use the existing sidebars. Focused chat fills the available desktop/tablet width. Escape restores the prior layout, and Reading controls remain available while scrolling. Original sidebar widths, open states, scroll positions, and drafts are retained.
- **#84 — Document results:** the public shinychat display adapter shows returned source details before the original JSON. The exact result and original request remain inspectable; JSON has a copy action. The summary uses the returned Document rather than the current selection.
- **#85 — Retained answers:** safe Markdown rendering, exact original-text inspection and copying, a visible rating above the answer, collapsed optional reasons, and a single primary dialog scroll path.
- **#86 — Saved ratings:** full-width radio choices show the complete question separately from type, rating, and time. Ordering is deterministic, duplicate questions are distinguishable, and the selected record survives review and return.

## Evidence

- [Saved ratings before](../reader-feedback-audit/1440-saved-ratings.png) and [after](../ui-followups-feedback/1440-saved-ratings.png).
- [Phone saved ratings](../ui-followups-feedback/320-saved-ratings.png).
- [Retained answer review](../ui-followups-feedback/1440-response-rating.png) and [enlarged text](../ui-followups-feedback/1440-response-rating-enlarged-text.png).
- [Focused chat](focused-chat.png).
- [Document source details and original JSON](tool-source-details.png). This fixture deliberately returns a different source from the current Reading selection to verify that displayed metadata follows the returned Document.

## Validation

- R CMD check: zero errors, warnings, or notes.
- Focused R tests for feedback, tools, UI, assets, and deployment hashes pass; pkgdown's reference-index check passes.
- The general browser audit recorded 46 states with zero automated accessibility violations and no browser errors: [results](results.json).
- Feedback checks recorded 16 states with zero automated accessibility violations: [results](../ui-followups-feedback/results.json). These cover save, review, preservation of an older selection, download, withdrawal, other-Reader exclusion, original-text copying, long Markdown, unbroken text, and dialog focus restoration.
- Pane checks cover a manually resized queue, Reading scroll restoration, preserved draft text, Escape, full-width focused chat, doubled text size, and narrow reflow.
- Tool checks cover the real shinychat renderer, pinned source identity, exact JSON copying, and transition from focused chat to a compact viewport.
- Air formatting, JavaScript syntax, and whitespace checks pass.

The browser fixtures use synthetic data and no model services. Enlarged-text
checks double the root text size; 320 CSS pixels exercise narrow reflow. These
are not a certification of browser zoom or screen-reader behavior.
PostgreSQL-only tests were skipped because no test database was configured.
The changes are intended for review in a pull request and have not been deployed.

Reproduction commands and fixture details are in
[the browser verification guide](../../scripts/browser/README.md).

## Pre-PR review

### Standards

One security finding was fixed: raw retained HTML could introduce Shiny action-link IDs and classes. A strict prose-tag and attribute allowlist now strips application bindings. The original text remains separately escaped and inspectable. The follow-up review found no remaining actionable issue.

### Spec

Three findings were fixed: focus now survives desktop/tablet breakpoints; Original Source is a safe clickable link from the returned Document; and the original-answer copy preserves leading newlines without HTML formatting whitespace. Browser regressions cover these paths, including a manually resized pane and returned-source identity.

### Development shinychat dependency

Rill pins shinychat 0.4.0.9000 at `dde163ea6b27099304a658f3649dd1fac98208bd` in DESCRIPTION and the Connect manifest. CI and container dependency installation honor DESCRIPTION Remotes. The source summary uses the public `extra$display` interface. Validation uses an isolated installation of this exact revision.
