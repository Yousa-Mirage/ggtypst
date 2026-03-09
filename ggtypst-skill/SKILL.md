---
name: ggtypst
description: Explain and use the ggtypst R package for Typst-powered text and math in ggplot2. Use when answering questions about ggtypst installation, annotate/geom/element helpers, Typst vs MiTeX math, raw strings in R, package architecture, examples, troubleshooting, or when writing ggtypst-based plotting code.
---

# ggtypst Skill

Use this skill to help users understand and use `ggtypst`, an R package that
renders Typst text and math inside `ggplot2`.

## Orient quickly

State the core idea first:

- `ggtypst` renders Typst source to SVG with a Rust backend
- the rendered SVG is inserted back into `ggplot2` as annotations, geoms, or theme elements
- the package supports both native Typst math and LaTeX-style math converted through MiTeX

Use this mental model when choosing APIs:

- `annotate_*()` for one manually positioned label
- `geom_*()` for one label per row of data
- `element_*()` for titles, subtitles, axis text, strips, and legends

## Explain installation correctly

Recommend:

```r
install.packages("remotes")
remotes::install_github("Yousa-Mirage/ggtypst")
```

Mention these points explicitly:

- `rustc` and `cargo` are required during installation
- a separate local Typst or LaTeX installation is not required
- `ggplot2` is attached when `library(ggtypst)` is called

## Choose the right helper family

Use this decision guide:

### `annotate_*()`

Use for one-off labels at fixed coordinates.

Main functions:

- `annotate_typst()` for general Typst text
- `annotate_math_typst()` for Typst math
- `annotate_math_mitex()` for LaTeX-style math converted by MiTeX

Example:

```r
ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  annotate_typst(
    typst_code = "*Fuel economy* #linebreak() $sum_(i=1)^n x_i$",
    x = 5,
    y = 32,
    hjust = 1,
    size = 13,
    color = "#1D4E89"
  )
```

### `geom_*()`

Use for data-driven labels mapped from rows.

Main functions:

- `geom_typst()` for general Typst labels
- `geom_math_typst()` for Typst math labels
- `geom_math_mitex()` for MiTeX-converted LaTeX math labels

Example:

```r
labels <- data.frame(
  wt = c(1.84, 2.32, 5.25),
  mpg = c(33.9, 30.4, 15.0),
  label = c(
    r"(*Toyota Corolla*)",
    r"(*Fiat 128*)",
    r"(*Maserati Bora*)"
  )
)

ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  geom_typst(
    data = labels,
    aes(wt, mpg, label = label),
    nudge_y = 0.8,
    size = 12,
    show.legend = FALSE
  )
```

### `element_*()`

Use for Typst-rendered theme text.

Main functions:

- `element_typst()` for general Typst text
- `element_math_typst()` for Typst math
- `element_math_mitex()` for MiTeX-converted LaTeX math

Example:

```r
ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
  geom_point() +
  labs(
    title = r"(`ggplot2` + Typst-rendered theme text)",
    x = r"($sum_(i=1)^n c_i$)",
    y = r"(\eta = \frac{mpg}{wt})"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_typst(size = 16, face = "bold"),
    axis.title.x = element_math_typst(size = 13, face = "bold"),
    axis.title.y = element_math_mitex(size = 13, face = "bold")
  )
```

## Explain Typst strings clearly

Prefer R raw strings when Typst or LaTeX contains many backslashes or quotes.

Use this form:

```r
r"(*bold text*)"
r"($sum_(i=1)^n x_i$)"
r"(\frac{a}{b})"
```

Explain why:

- ordinary R strings require escaping
- raw strings are easier for Typst markup and LaTeX-style math
- MiTeX examples are much easier to read with raw strings

## Distinguish text, Typst math, and MiTeX math

Use this mapping:

- plain Typst text: `annotate_typst()`, `geom_typst()`, `element_typst()`
- native Typst math: `annotate_math_typst()`, `geom_math_typst()`, `element_math_typst()`
- LaTeX-style math via MiTeX: `annotate_math_mitex()`, `geom_math_mitex()`, `element_math_mitex()`

Explain MiTeX accurately:

- MiTeX converts LaTeX-style math to Typst math
- `ggtypst` evaluates the converted expression inside an embedded Typst scope
- this is why LaTeX-style math works without a LaTeX installation

## Mention important parameter conventions

Highlight these differences and aliases when relevant:

- `size.unit = "pt"` is the default in `ggtypst`
- use `size.unit = "mm"` to align with `ggplot2::geom_text()` sizing conventions
- `colour` and `color` are aliases
- `face` and `fontface` are aliases
- math helpers only support `face = "plain"` and `face = "bold"`
- `inline = TRUE` switches math helpers to inline math

## Give package-aware troubleshooting

When debugging, check these first:

- installation failures: verify `rustc` and `cargo`
- missing `ggplot()` in docs/examples: attach `ggplot2` explicitly in R Markdown
- MiTeX file lookup failures on Windows: ensure a version containing the virtual path normalization fix is installed
- unexpected text size: check `size.unit`
- LaTeX-style math errors: reduce to a minimal `annotate_math_mitex()` example and inspect the offending label

## Use repo-aware references

Prefer these sources when answering deeper questions:

- `README.Rmd` for high-level package positioning and showcase examples
- `vignettes/get-started.Rmd` for onboarding and usage patterns
- `.github/CONTRIBUTING.Rmd` for architecture and development workflow
- `R/annotate-typst.R`, `R/annotate-math.R`, `R/geom-typst.R`, `R/geom-math.R`, `R/element-typst.R`, `R/element-math.R` for API details
- `R/helper.R` for validation and alias behavior
- `R/mitex.R` for LaTeX-to-Typst conversion behavior
- `src/rust/src/world.rs` and `src/rust/src/render.rs` for rendering internals

## Answer style

When helping a user:

1. Start by choosing the right helper family
2. Give one minimal working example
3. Mention raw strings if the example contains Typst or LaTeX markup
4. Call out `size.unit`, `inline`, or `face` restrictions when they matter
5. Offer the next-most-relevant helper family only after the first example is clear
