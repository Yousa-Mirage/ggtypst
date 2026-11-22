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
