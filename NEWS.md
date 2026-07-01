# ggtypst 0.1.1

- `annotate_typst()`, `annotate_math_typst()`, and `annotate_math_mitex()` now
  require optional arguments after `x` and `y` to be named, so misplaced
  positional inputs and misspelled argument names fail fast.
- (DOING) Improved the performance of insert contents.

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
