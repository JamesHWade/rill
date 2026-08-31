# Design QA

## Reference and build

- Selected light reference: `.scratch/rill-theme/selected-reference.png`
- Selected dark reference: `.scratch/rill-theme/dark-reference.png`
- Final dark desktop implementation: `.scratch/rill-theme/implementation-dark-desktop-final.png`
- Final dark desktop comparison: `.scratch/rill-theme/comparison-dark-desktop-final.png`
- Light regression frame: `.scratch/rill-theme/implementation-light-regression.png`
- Dark mobile reader: `.scratch/rill-theme/implementation-dark-mobile-reader.png`
- Dark mobile queue: `.scratch/rill-theme/implementation-dark-mobile-queue.png`
- Desktop viewport: 1440 x 1024
- Mobile viewport: 390 x 844

## Comparison passes

### Desktop fidelity

The final dark comparison places the exact 1440 x 1024 reference and browser capture side by side in the same first-story state. The three-column geometry, warm-black reader, near-black queue, moss sidebar, parchment hierarchy, celadon secondary color, ochre selection, icon treatment, article measure, and Literata reading rhythm match the selected direction. The appearance control and demo badge remain as intentional product controls.

The light regression frame preserves the earlier duck-egg, reed, paper, and sand theme without typography or spacing drift. The dark duck is a dedicated transparent raster asset, not a CSS filter or a drawn substitute.

### Responsive behavior

At 390 x 844, the dark reader is full-width with a working return action, readable line length, practical appearance targets, and no horizontal overflow. Returning to the queue restores the stacked navigation and story list without overlap. The horizontally scrollable feed row preserves its compact hierarchy at the narrow breakpoint.

Navigation and the story queue are native nested `bslib` sidebars. Both expose independent desktop resize handles and collapse controls. At compact desktop widths, navigation starts collapsed to preserve the reader measure; at mobile widths, both sidebars use bslib's `always-above` flow and hide desktop-only resize controls.

### Accessibility and interaction

Keyboard focus remains visible, the paired brand images are decorative beside the product name, navigation and appearance retain native radio semantics, the reader actions expose pressed state, and reduced motion is respected. System, light, and dark choices resolve correctly and persist across reloads. Star/save toggles, J/K story navigation, feed management, and mobile return were exercised. Browser console inspection produced no warnings or errors.

Measured dark-mode contrast was 11.42:1 for reading copy, 7.59:1 for queue summaries, 10.07:1 for sidebar navigation, 7.44:1 for article metadata, and 6.74:1 for the brand subtitle.

## Resolved findings

- P1 layout: removed inherited page padding that clipped the 100dvh shell and prevented the target's full-bleed composition.
- P2 typography: reduced queue-title weight and scale while increasing article-source prominence and restoring the reference's more generous header rhythm.
- P2 icons: replaced text glyph approximations with Bootstrap Icons for navigation, story status, refresh, and reader actions.
- P2 imagery: replaced the letter monogram with the generated duck-and-ripple asset and verified its transparent treatment at navigation and empty-state sizes.
- P2 color modes: added paired semantic tokens for every navigation, queue, reader, form, keycap, code, quote, link, status, hover, focus, and selected surface instead of relying on a global color inversion.
- P2 mode behavior: added an early system-aware theme resolver to avoid an initial wrong-palette flash, persisted explicit choices, and kept system mode responsive to operating-system changes.
- P2 mobile accessibility: enlarged appearance choices at the narrow breakpoint and verified the queue and reader at 390 x 844 with zero page-level horizontal overflow.
- P2 component architecture: replaced the custom three-column grid with two nested, fill-aware `layout_sidebar()`/`sidebar()` pairs from bslib 0.12.0. Drag resizing, independent collapse/restore, compact-desktop defaults, and mobile stacking were verified in the running app.

No open P0, P1, or P2 findings remain.

final result: passed
