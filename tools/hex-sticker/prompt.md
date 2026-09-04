# rill sticker artwork

Generated with the built-in image generation tool. The square illustration is
cropped and typeset separately by render.py using Pillow and Avenir Next Bold.
The logo is a raster PNG, not vector artwork. Outer corners are transparent.

References: https://ellmer.tidyverse.org/logo.png and https://vitals.tidyverse.org/logo.png

## Generation prompt

Create a polished original illustration for an R package sticker, inspired by the friendly illustrative craft of tidyverse package mascots. SQUARE full-bleed illustration, NOT a hexagon, no border, NO TEXT or letters. Flat vector-like shapes, confident dark outlines, charming expressive animal character, tightly controlled vivid palette, subtle hand-drawn character, excellent clarity at 2 inches. No photorealism, no 3D, no gradients, no drop shadows. Central composition suitable for subsequent point-up regular hexagonal cropping: keep important features within the central 65% width and central 60% height; corners contain only background. Leave lower 22% fairly quiet for a package wordmark to be added later. Character is sophisticated and endearing, not generic clip art.

A charming river otter floating comfortably on its back in a gently winding blue-green stream, holding and reading an open warm cream book on its chest. Expressive attentive face, small rounded ears, russet-brown fur with cream muzzle, visible curved tail. A few rhythmic stream lines and two simple riverbank leaves. Pale mint background, teal water shapes, dark deep-teal outlines, warm rust and honey accents. Main otter face and book in upper-middle. A lovely calm reading mood with personality and graphic clarity.

## Render

Run from the package root with Python and Pillow installed:

`python3 tools/hex-sticker/render.py tools/hex-sticker/artwork.png rill man/figures/hex-sticker.png --ink '#124c50'`

## Website icons

Run `python3 tools/hex-sticker/export-favicons.py` to regenerate pkgdown icons from `man/figures/logo.png`. The SVG favicon embeds raster artwork; it is not a vector master.
