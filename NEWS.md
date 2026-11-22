# ggtypst 0.2.0

- **(BREAKING)** `annotate_typst()`, `annotate_math_typst()`, and `annotate_math_mitex()`
  now require optional arguments after `x` and `y` to be named, so misplaced
  positional inputs and misspelled argument names fail fast.
-  **(BREAKING: Expect plots to change slightly in size)**
  Typst output is now drawn at exactly the size Typst lays it out, so `size`
  matches the requested font size. Previously all rendered text and math were
  about 9% too small, and short labels shrank by up to a further ~5% depending
  on their box size (so a single-line label drew smaller glyphs than a
  multi-line label of the same `size`).
- `element_typst()`, `element_math_typst()`, and `element_math_mitex()` now
  respect `hjust` and `vjust` within their cell, like `element_text()`: titles,
  subtitles, captions, and legend labels are no longer always centered.
- The lines of multi-line labels are now aligned left, center, or right
  according to `hjust` in annotations, geoms, and theme elements, as with
  `ggplot2` text. Alignment set in the Typst code itself still takes precedence.
- Labels are now justified by their content: Typst's page margin no longer
  shifts text away from its anchor, so `hjust = 0` and `hjust = 1` line up
  exactly with `ggplot2` text.
- Vertical metrics now match `ggplot2` text. `vjust = 0` puts the last
  baseline on the anchor and `vjust = 1` the cap height of the first line,
  whatever glyphs the text contains; `lineheight` sets the baseline distance to
  `1.2 * lineheight` font sizes as in `ggplot2` (it used to add that much
  spacing between lines), defaulting to `geom_text()`'s 1.2; and theme
  elements reserve the same height for any text of a given font and size.
- Typst output is now drawn with grid directly instead of being imported as
  SVG. On devices that rasterize or embed fonts (ragg, `png()`, `cairo_pdf()`,
  svglite, ...) text is drawn as native glyphs, exactly like `ggplot2` text, so
  it is no longer heavier than `element_text()` on screen and `cairo_pdf()`
  embeds the real fonts. On `pdf()` and `postscript()`, which only reference
  fonts by name, text is drawn as outlines so it looks the same everywhere.
  See `?ggtypst-options` to choose (native glyphs need R >= 4.3).
- `rsvg` and `grImport2` are no longer required. They are only used, when
  installed, to draw Typst content that ggtypst cannot draw with grid yet
  (gradients, images, and color glyphs drawn as outlines), so most
  installations no longer need the system library `librsvg`.

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
