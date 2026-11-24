---
name: ggtypst-skill
description: Explain and use the ggtypst R package, which renders Typst text and math (native Typst math or LaTeX math through MiTeX) in ggplot2 annotations, data labels, and theme elements. Use whenever someone asks about ggtypst installation, annotate_typst()/geom_typst()/element_typst() and their math variants, Typst or LaTeX formulas in ggplot2 plots, rich text labels, fonts, sizes, or justification of ggtypst labels, PDF vs PNG output, rendering errors, performance, or the package's internals, or when writing ggplot2 code that should use Typst for labels.
---

# ggtypst Skill

Use this skill to help users understand and use `ggtypst`, an R package that renders Typst text and math inside
`ggplot2`.

## Orient quickly

State the core idea first:

- `ggtypst` compiles Typst code with the Typst compiler built into the package (a Rust library), so users need no
  local Typst or LaTeX installation
- the laid-out result is drawn directly with grid, like `ggplot2`'s own text: as native glyphs on devices that
  rasterize or embed fonts, and as glyph outlines on devices such as `pdf()` (see "Rendering and devices")
- it supports rich Typst text, native Typst math, and LaTeX math converted to Typst by MiTeX
- labels follow `ggplot2`'s text conventions: `size` is the real font size, and `hjust`, `vjust`, `angle`, and
  `lineheight` work as in `geom_text()` and `element_text()`

Use this mental model when choosing APIs:

- `annotate_*()` for one manually positioned label
- `geom_*()` for one label per row of data
- `element_*()` for titles, subtitles, captions, axis titles and text, legend text, and strips

## Explain installation correctly

Recommend the prebuilt binaries from R-universe first, which need no Rust toolchain:

```r
# Windows and macOS
install.packages("ggtypst", repos = "https://yousa-mirage.r-universe.dev")
```

Ubuntu 24.04 has Linux binaries too; the README shows the repository URL to use. On other platforms, or for the
development version, install from source:

```r
# install.packages("devtools")
devtools::install_github("Yousa-Mirage/ggtypst")
```

Mention these points when relevant:

- installing from source needs Rust: `cargo` and `rustc` (1.89 or later)
- `ggtypst` needs R 4.3 or later, the first version with native glyph drawing (`grid::glyphGrob()`)
- no separate Typst or LaTeX installation is needed
- `library(ggtypst)` also attaches `ggplot2`

## Choose the right helper family

### `annotate_*()`

Use for one-off labels at fixed coordinates. Each call adds one layer.

- `annotate_typst()` for Typst text (markup, math in `$...$`, any Typst code)
- `annotate_math_typst()` for a Typst math formula
- `annotate_math_mitex()` for a LaTeX math formula converted by MiTeX

The code and `x`, `y` come first; every other argument comes after `...` and must be named.

```r
ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  annotate_typst(
    r"(*Fuel economy* #linebreak() $sum_(i=1)^n x_i$)",
    x = 5,
    y = 32,
    hjust = 1,
    size = 13,
    color = "#1D4E89"
  )
```

### `geom_*()`

Use for data-driven labels, one per row. Map the Typst code to the `label` aesthetic, or give a constant
`label = "..."` to draw the same label on every row.

- `geom_typst()` for Typst labels
- `geom_math_typst()` for Typst math labels
- `geom_math_mitex()` for LaTeX math labels converted by MiTeX

```r
labels <- data.frame(
  wt = c(1.84, 2.32, 5.25),
  mpg = c(33.9, 30.4, 15.0),
  label = c("*Toyota Corolla*", "*Fiat 128*", "*Maserati Bora*")
)

ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  geom_typst(
    data = labels,
    aes(wt, mpg, label = label),
    nudge_y = 0.8,
    size = 12
  )
```

The geoms understand the aesthetics `colour`, `size`, `alpha`, `angle`, `hjust`, `vjust`, `lineheight`, `face`,
`family`, `math_family`, and `scale`; each can be mapped or set.

### `element_*()`

Use for Typst-rendered theme text. The plot's labels (from `labs()`, axis breaks, legend keys, facet strips) are
read as Typst code.

- `element_typst()` for Typst text
- `element_math_typst()` for Typst math
- `element_math_mitex()` for LaTeX math converted by MiTeX

```r
ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
  geom_point() +
  labs(
    title = "*Fuel economy* by weight",
    x = r"(sum_(i=1)^n c_i)",
    y = r"(\eta = \frac{mpg}{wt})"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_typst(size = 16),
    axis.title.x = element_math_typst(size = 13),
    axis.title.y = element_math_mitex(size = 13)
  )
```

Set them on specific elements (`plot.title`, `axis.title.x`) or on groups (`axis.title`, `axis.text`,
`legend.text`, `strip.text`), whose children inherit them. Do not set `text = element_typst()`: the root `text`
element currently fails to build. The elements take the usual `element_text()` arguments, including `margin`,
`hjust`, `vjust`, and `angle`.

## Explain Typst strings clearly

Prefer R raw strings when Typst or LaTeX code contains backslashes or quotes:

```r
r"(*bold text*)"
r"($sum_(i=1)^n x_i$)"
r"(\frac{a}{b})"
```

Explain why:

- ordinary R strings need every backslash and quote escaped
- raw strings keep Typst markup and LaTeX math readable, which matters most for MiTeX

## Distinguish text, Typst math, and MiTeX math

- Typst text: `annotate_typst()`, `geom_typst()`, `element_typst()`. The code is Typst markup, so math needs
  `$...$` and functions need `#`, for example `#text(red)[warning]`.
- Typst math: `annotate_math_typst()`, `geom_math_typst()`, `element_math_typst()`. The code is a formula; the
  surrounding `$...$` is optional.
- LaTeX math: `annotate_math_mitex()`, `geom_math_mitex()`, `element_math_mitex()`. MiTeX converts the LaTeX
  formula to Typst math, which is evaluated in a scope that defines the LaTeX commands, so no LaTeX installation
  is needed. Surrounding `$...$` or `$$...$$` is optional.

Math helpers draw display-style math by default; `inline = TRUE` gives inline style (smaller fractions, limits beside
operators).

## Mention important parameter conventions

- `size` is in points by default (`size.unit = "pt"`). Annotations and geoms default to 11 pt; theme elements
  inherit the theme's size. Use `size.unit = "mm"` to give sizes in `geom_text()`'s millimetre units.
- `colour` and `color` are aliases, as are `face` and `fontface`; giving both of a pair is an error.
- text helpers accept `face` `"plain"`, `"bold"`, `"italic"`, or `"bold.italic"` (or 1 to 4); math helpers accept
  only `"plain"` and `"bold"`.
- `hjust` and `vjust` justify the label box as in `ggplot2`: `vjust = 0` puts the last baseline on the anchor,
  `vjust = 1` the cap height of the first line. The lines of a multi-line label are aligned left, centre, or
  right by `hjust`, unless the Typst code sets its own alignment.
- `lineheight` sets the distance between baselines to `1.2 * lineheight` font sizes, as in `ggplot2`; the default
  matches `geom_text()`.
- `scale` multiplies the whole rendered label, `alpha` multiplies the colour's opacity.
- The default fonts are Typst's built-in Libertinus Serif for text and New Computer Modern Math for math, which
  are always available. `family` and `math_family` can name any font installed on the system, which helps to
  match the plot's other text or to show scripts the default font lacks (for example Chinese or Japanese).

## Rendering and devices

Set these with `options()`; see `?ggtypst-options`.

- `ggtypst.renderer`: `"auto"` (default) draws native glyphs on ragg, `png()` and other raster devices,
  `cairo_pdf()`, `cairo_ps()`, svglite, and Quartz, and outlines on `pdf()`, `postscript()`, and unknown devices,
  which only reference fonts by name. `"glyph"` or `"path"` force one way. Outlined text looks the same
  everywhere but is not selectable in a PDF; use `cairo_pdf()` to embed the real fonts.
- Text that native glyphs cannot draw (a stroked outline, a variable font) is always drawn as outlines.
- Gradients, tilings, images, and colour emoji are drawn as images rendered by Typst at
  `ggtypst.raster_dpi` (600 by default). Set it before creating the plot.
- On R 4.3 and 4.4, a label whose glyphs have different angles (one rotated word inside it) is drawn as outlines;
  a label rotated as a whole still uses native glyphs.

## Performance

Answer performance questions with these figures, from the package's benchmark article:

- a label in a `geom_*()` layer adds roughly 0.6 to 0.9 ms to a PNG and about 2 ms to a PDF (more in PDF because
  `pdf()` draws outlines), which is faster than `ggtext::geom_richtext()`
- a few annotations or theme labels add only a few milliseconds to a plot
- `ggtypst` starts scanning the installed fonts in the background when it is loaded, so a plot drawn right after
  `library(ggtypst)` waits briefly for that scan
- `ggplot2` rebuilds every label each time a plot is printed or saved

For many labels, recommend one `geom_*()` layer with a data frame over many `annotate_*()` calls, since every layer
has its own overhead.

## Give package-aware troubleshooting

- **Installation fails**: suggest the R-universe binaries; for source installs, check `rustc --version` and `cargo`.
- **A Typst error**: errors show Typst's message and hints; in a geom they also give the row number and a preview of
  the label. Reproduce the failing label alone with `annotate_typst()` (or the matching math helper) to isolate
  it. Common causes: a math formula given to a text helper without `$...$`, a missing `#` before a function in
  markup, or LaTeX syntax given to a Typst math helper.
- **Text size looks wrong**: check `size.unit`; `geom_text()` sizes are in millimetres.
- **A font is not used**: the family must be installed on the system and spelled as the font calls itself;
  otherwise Typst falls back to its default font and warns.
- **PDF text looks different from PNG, or cannot be selected**: `pdf()` gets outlines; use `cairo_pdf()` or
  set `ggtypst.renderer`.
- **Gradients or emoji look soft**: raise `ggtypst.raster_dpi` before creating the plot.
- **`theme(text = element_typst())` fails**: set the element on groups such as `axis.text`, `axis.title`,
  `legend.text`, and `strip.text` instead.

## Use repo-aware references

Prefer these sources when answering deeper questions:

- `README.Rmd` for positioning, installation, and showcase examples
- `vignettes/articles/get-started.Rmd` for onboarding and usage patterns
- `vignettes/articles/benchmark.Rmd` for performance figures against `ggplot2` and `ggtext`
- `.github/CONTRIBUTING.Rmd` for the architecture and development workflow
- `R/plot-annotate-typst.R`, `R/plot-annotate-math.R`, `R/plot-geom-typst.R`, `R/plot-geom-math.R`,
  `R/plot-element-typst.R`, `R/plot-element-math.R` for the API
- `R/helper.R` for validation and argument aliases; `R/options.R` for the options
- `R/build-source.R` for the Typst code wrapped around each label (size, colour, font, alignment, line spacing)
- `R/mitex.R` for LaTeX-to-Typst conversion
- `R/grob.R` and `R/scene.R` for drawing with grid (renderer choice, glyphs, outlines, clipping)
- `src/rust/src/world.rs`, `src/rust/src/render.rs`, and `src/rust/src/scene.rs` for compiling Typst and
  describing the page for R

## Answer style

When helping a user:

1. Start by choosing the right helper family
2. Give one minimal working example
3. Use raw strings when the example contains Typst or LaTeX markup
4. Call out `size.unit`, `inline`, `face` restrictions, or the output device when they matter
5. Offer the next-most-relevant helper family only after the first example is clear
