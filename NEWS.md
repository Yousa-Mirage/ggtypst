# ggtypst 0.2.0

Many thanks to **Stein Are Sæther** for carefully spotting several plotting
issues and suggesting fixes. Most of the changes below stem from his report.

## Breaking changes

- ggtypst now requires R 4.3 or later, the first version that can draw native
  glyphs.
- In `annotate_typst()`, `annotate_math_typst()`, and `annotate_math_mitex()`,
  all optional arguments after `x` and `y` must now be named explicitly.
- Text and math are now drawn at the requested `size`. They used to be about 9%
  too small, so labels in existing plots get slightly larger.

## New rendering

- Typst output is now drawn directly with grid instead of being imported as
  SVG. On devices that rasterize or embed fonts (ragg, `png()`, `cairo_pdf()`,
  svglite, Quartz), text is drawn as native glyphs like `ggplot2` text: crisp
  on screen, no longer heavier than `element_text()`, and with the real fonts
  embedded by `cairo_pdf()`. On `pdf()` and `postscript()`, text is drawn as
  outlines so it looks the same everywhere.
- New options, described in `?ggtypst-options`: `ggtypst.renderer` chooses
  native glyphs or outlines, `ggtypst.raster_dpi` sets the resolution of
  gradients, tilings, images, and color emoji (drawn as images, 600 dpi by
  default), and `ggtypst.font_cache` sets where the built-in fonts are written
  for native glyphs.
- `rsvg` and `grImport2` are no longer used, so installing ggtypst no longer
  requires the system library `librsvg`.

## Layout fixes

- Typst theme elements now respect `hjust` and `vjust`: titles, subtitles,
  captions, and legend labels are no longer always centered.
- `element_typst()` and the math elements can now be set on the root `text`
  element, as in `theme(text = element_typst())`, to draw all of a plot's text
  with Typst.
- The lines of multi-line labels are aligned by `hjust`, unless the Typst code
  sets its own alignment.
- Labels now line up with `ggplot2` text: `hjust` and `vjust` place the text
  itself rather than Typst's page margin, `lineheight` spaces lines as in
  `ggplot2`, and theme elements reserve the same height as `element_text()`.
- Clipped boxes inside clipped boxes are now clipped by both.

## Performance

- Labels render much faster: a label in a `geom_*()` layer adds under 1 ms to
  a PNG and about 2 ms to a PDF, less than with `ggtext`.
- The installed fonts are loaded in the background when the package loads, so
  the first plot appears sooner.
- PDFs with text drawn as outlines are smaller, and memory use stays limited
  when rendering many different labels.

# ggtypst 0.1.0

This is the first public release of `ggtypst`.

## Features

- Added `annotate_typst()`, `geom_typst()`, and `element_typst()` for
  Typst-based text rendering in `ggplot2` annotations, data layers, and theme
  elements.
- Added native Typst math helpers:
  - `annotate_math_typst()`
  - `geom_math_typst()`
  - `element_math_typst()`
- Added MiTeX-backed LaTeX math helpers:
  - `annotate_math_mitex()`
  - `geom_math_mitex()`
  - `element_math_mitex()`
- Added support for styling arguments including `size`, `face`,
  `colour`/`color`, `angle`, `alpha`, `lineheight`, `family`, and `mathfamily`
  across the public plotting helpers.

## Quality

- Added `testthat` and `vdiffr` coverage for annotation, geom, and theme-element
  rendering paths, along with supporting package components.
