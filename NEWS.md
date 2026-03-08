# ggtypst 0.0.0.9000

## New features

- Added `geom_typst()` for data-driven Typst text rendering in `ggplot2`.
- Added `annotate_typst()` for one-off Typst annotations.
- Added `element_typst()` for Typst-based theme elements rendering.
- Added MiTeX-backed LaTeX-to-Typst conversion support for annotations, geoms,
  and theme elements.
- Added math-specific annotation helpers:
  - `annotate_math_typst()`
  - `annotate_math_mitex()`
- Added math-specific geoms:
  - `geom_math_typst()`
  - `geom_math_mitex()`
- Added math-specific theme elements:
  - `element_math_typst()`
  - `element_math_mitex()`
- Supported `size`, `face`, `color`, `angle`, `alpha`, `lineheight`, `family`, `mathfamily` 
  arguments for these plot functions.

## Testing and maintenance

- Added broad `testthat` and `vdiffr` coverage for annotate, geom, element
  rendering paths and other components.
