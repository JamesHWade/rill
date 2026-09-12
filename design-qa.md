# Design QA

## Theme, structure, and responsive baseline

### Reference and build

- Selected light reference: `.scratch/rill-theme/selected-reference.png`
- Selected dark reference: `.scratch/rill-theme/dark-reference.png`
- Final dark desktop implementation: `.scratch/rill-theme/implementation-dark-desktop-final.png`
- Final dark desktop comparison: `.scratch/rill-theme/comparison-dark-desktop-final.png`
- Light regression frame: `.scratch/rill-theme/implementation-light-regression.png`
- Dark mobile reader: `.scratch/rill-theme/implementation-dark-mobile-reader.png`
- Dark mobile queue: `.scratch/rill-theme/implementation-dark-mobile-queue.png`
- Desktop viewport: 1440 x 1024
- Mobile viewport: 390 x 844

### Comparison passes

#### Desktop fidelity

The final dark comparison places the exact 1440 x 1024 reference and browser capture side by side in the same first-story state. The three-column geometry, warm-black reader, near-black queue, moss sidebar, parchment hierarchy, celadon secondary color, ochre selection, icon treatment, article measure, and Literata reading rhythm match the selected direction. The appearance control and demo badge remain as intentional product controls.

The light regression frame preserves the earlier duck-egg, reed, paper, and sand theme without typography or spacing drift. The dark duck is a dedicated transparent raster asset, not a CSS filter or a drawn substitute.

#### Responsive behavior

At 390 x 844, the dark reader is full-width with a working return action, readable line length, practical appearance targets, and no horizontal overflow. Returning to the queue restores the stacked navigation and story list without overlap. The horizontally scrollable feed row preserves its compact hierarchy at the narrow breakpoint.

Navigation and the story queue are native nested `bslib` sidebars. Both expose independent desktop resize handles and collapse controls. At compact desktop widths, navigation starts collapsed to preserve the reader measure; at mobile widths, both sidebars use bslib's `always-above` flow and hide desktop-only resize controls.

#### Accessibility and interaction

Keyboard focus remains visible, the paired brand images are decorative beside the product name, navigation and appearance retain native radio semantics, the reader actions expose pressed state, and reduced motion is respected. System, light, and dark choices resolve correctly and persist across reloads. Star/save toggles, J/K story navigation, feed management, and mobile return were exercised. Browser console inspection produced no warnings or errors.

Measured dark-mode contrast was 11.42:1 for reading copy, 7.59:1 for queue summaries, 10.07:1 for sidebar navigation, 7.44:1 for article metadata, and 6.74:1 for the brand subtitle.

### Resolved findings

- P1 layout: removed inherited page padding that clipped the 100dvh shell and prevented the target's full-bleed composition.
- P2 typography: reduced queue-title weight and scale while increasing article-source prominence and restoring the reference's more generous header rhythm.
- P2 icons: replaced text glyph approximations with Bootstrap Icons for navigation, story status, refresh, and reader actions.
- P2 imagery: replaced the letter monogram with the generated duck-and-ripple asset and verified its transparent treatment at navigation and empty-state sizes.
- P2 color modes: added paired semantic tokens for every navigation, queue, reader, form, keycap, code, quote, link, status, hover, focus, and selected surface instead of relying on a global color inversion.
- P2 mode behavior: added an early system-aware theme resolver to avoid an initial wrong-palette flash, persisted explicit choices, and kept system mode responsive to operating-system changes.
- P2 mobile accessibility: enlarged appearance choices at the narrow breakpoint and verified the queue and reader at 390 x 844 with zero page-level horizontal overflow.
- P2 component architecture: replaced the custom three-column grid with two nested, fill-aware `layout_sidebar()`/`sidebar()` pairs from bslib 0.12.0. Drag resizing, independent collapse/restore, compact-desktop defaults, and mobile stacking were verified in the running app.

No open P0, P1, or P2 findings remained in the baseline pass.

## Queue controls and reading-header density

### Source truth

- Browser comments 1–3 in this task, captured from the Rill preview before the
  change.
- Reproduced baseline at commit `9cc9141`:
  - `/private/tmp/rill-header-density-qa-20260904/design-qa-before-desktop.png`
  - `/private/tmp/rill-header-density-qa-20260904/design-qa-before-mobile.png`

### Implementation captures

- Desktop:
  `/private/tmp/rill-header-density-qa-20260904/design-qa-desktop.png`
- Mobile:
  `/private/tmp/rill-header-density-qa-20260904/design-qa-mobile.png`
- Side-by-side comparisons:
  - `/private/tmp/rill-header-density-qa-20260904/design-qa-comparison-desktop.png`
  - `/private/tmp/rill-header-density-qa-20260904/design-qa-comparison-mobile.png`

### Viewports and state

- Desktop: 1169 × 863 CSS pixels, DPR 1, light appearance, navigation and queue
  open, selected demo story.
- Mobile: 423 × 863 CSS pixels, DPR 1, light appearance, Reading open on the
  same demo story.

### Comparison history

1. Captured the existing implementation at `9cc9141` in both target viewports.
2. Captured the updated implementation in the same viewports and story state.
3. Compared each before/after pair side by side and measured the affected
   elements in the rendered DOM.

### Findings

- P0: none.
- P1: none.
- P2: none.
- The desktop queue controls moved from a 10.8 px center-line spread to 0 px;
  all three controls render at 28 px tall on the same vertical center.
- On mobile, the action-to-source gap fell from 50 px to 22 px, the stored-copy
  cue fell from 48.4 px to 24.3 px tall, and the article header fell from
  437.5 px to 356.7 px tall.
- Both target viewports have no horizontal overflow.
- The full reading-copy boundary remains available in the native bslib
  “About this reading copy” accordion.

## Article title and thematic-break density

### Source truth

- User feedback in this task: article titles felt oversized and thematic breaks
  carried too much surrounding space.
- Reproduced baseline at commit `39e7192`:
  - `/private/tmp/rill-article-type-qa-20260904/before-mobile.png`
  - `/private/tmp/rill-article-type-qa-20260904/before-desktop.png`

### Implementation captures

- Updated mobile reader:
  `/private/tmp/rill-article-type-qa-20260904/after-mobile.png`
- Updated desktop reader:
  `/private/tmp/rill-article-type-qa-20260904/after-desktop.png`
- Same-state title comparisons:
  - `/private/tmp/rill-article-type-qa-20260904/title-comparison-mobile.png`
  - `/private/tmp/rill-article-type-qa-20260904/title-comparison-desktop.png`
- Focused thematic-break comparison:
  `/private/tmp/rill-article-type-qa-20260904/hr-comparison.png`

### Viewports and state

- Desktop: 1169 × 863 CSS pixels, DPR 1, 1169 × 863 output pixels, light
  appearance, navigation and queue open, “Deploying a small stateful
  application” selected.
- Mobile: 423 × 863 CSS pixels, DPR 2, screenshots normalized to 423 × 863
  output pixels, light appearance, Reading open on the same story.
- Focused divider frame: 1280 × 720 output pixels, light appearance, the
  production `.reader-document` stylesheet with paired before/after fixtures.

### Comparison history

1. Captured the title baseline from `39e7192` at both target viewports.
2. Captured the updated reader in the same viewport, appearance, and story
   state, then compared the full views side by side.
3. Rendered the production article stylesheet around a thematic break and
   compared the original 1 rem rhythm against the new compact rhythm.
4. Measured the affected elements in the rendered DOM and checked both target
   viewports for page-level overflow.

### Findings

- P0: none.
- P1: none.
- P2: none.
- Mobile title size fell from 42.3 px to 38.07 px, and the article header
  became 9.55 px shorter.
- Desktop title size fell from 46.76 px to 40.915 px; the same two-line title
  remains readable without crowding the metadata or reading-copy cue.
- The thematic-break gap is now 8.1 px on both sides instead of 16 px. The
  adjacent paragraph/list margins are constrained as well as the `hr` itself,
  so margin collapsing cannot restore the larger gap.
- Both target viewports have no horizontal overflow.

final result: passed

## Reading toolbar

final result: passed

The source is the approved single reading-mode wireframe at `/Users/james/.codex/generated_images/01a0930a-6b9d-7212-95cc-be8ef23c4f55/exec-5a942dd1-881f-430f-9ce3-1cfd692512dd.png` (1587 × 991). The implementation is `artifacts/reading-toolbar/desktop.png` (1232 × 768 CSS pixels, density 1) at http://127.0.0.1:3927. Both were opened in the same comparison input; comparison uses their equivalent viewport aspect ratio, not literal pixel differences.

State: dark theme, Library collapsed, queue visible, article selected, menu closed. The implementation uses bundled demo content, so article wording, headings, queue contents, and title wrapping intentionally differ. The existing resizable queue width is preserved. This is a layout/hierarchy comparison rather than a pixel reproduction of the article text.

### Visual review

- Typography: existing Literata/Atkinson families retained; title capped at 44px, with a readable serif article body and small utility metadata.
- Spacing: a single 49px desktop toolbar replaces the stacked controls. Article content begins at 191px in the verified desktop state. Source, title, and byline remain grouped. Focus mode centers the reading column.
- Colors: existing dark-paper, cream, and river-green theme tokens retained; light theme was also visually checked.
- Assets: Bootstrap icons from bsicons; no replacement brand artwork.
- Copy: source link and reading-copy identity remain explicit. Provenance moves into a byline popover; feed-excerpt warnings and preparation/recovery controls stay visible.
- The full-view comparison makes the toolbar, metadata, and article text readable without a separate detail crop.

### Corrections verified

- Removed the redundant sidebar toggle that obscured the overflow button.
- Corrected dropdown nesting so hidden actions remain bound without occupying the toolbar.
- Disabled animation on article tooltips and copy popovers to avoid Bootstrap callbacks after a reactive header is disposed.
- Removed the mobile sidebar header's unused 56px gap.
- Escape closes the current article disclosure before restoring pane focus; regression cases cover menu, copy popover, modal ownership, focus, and Ask Rill.

### Interaction checks

Browser checks cover the overflow menu, Save/Star hotkeys with the menu closed, article navigation, focus/restore, copy details, Ask Rill open/close, and mobile return to the queue. The 320px layout has no horizontal document overflow; its menu and copy popover remain within the viewport. A 390 × 844 capture is saved at `artifacts/reading-toolbar/mobile.png`. Browser error/warning logs were empty in the final desktop pass.

No remaining P0/P1/P2 findings. Production deployment and authenticated production traces are outside this local implementation pass. The local preview uses in-memory demo data and does not send model requests.
