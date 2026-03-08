---
title: ggtypst
---

## Typst for ggplot2

`ggtypst` brings Typst-powered text and math rendering to `ggplot2`.

It lets you use the same typesetting engine across one-off annotations,
data-driven text layers, and theme elements, all without requiring a separate
Typst or LaTeX installation in normal package use.

## Why ggtypst?

- `annotate_typst()` adds rich explanatory callouts directly on a plot.
- `geom_typst()` turns Typst labels into a real data layer with mapped style,
  position, and content.
- `element_typst()` upgrades titles, axis labels, strip text, and legend text
  to Typst-rendered theme elements.
- Math can be written either as native Typst math or as LaTeX-style input
  converted through MiTeX.
- Output is rendered through a Rust backend and imported back into grid as
  vector graphics that fit naturally into ggplot2.

## Installation

Install the development version from GitHub:

```r
pak::pak("Yousa-Mirage/ggtypst")
```

System requirements:

- R >= 4.2
- `cargo`
- `rustc >= 1.89.0`
- `xz`

## Three entry points

### 1. Annotate a single plot

Use `annotate_typst()` when you want a handcrafted explanation, callout, or
equation placed at a specific coordinate.

```r
library(ggplot2)
library(ggtypst)

ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  annotate_typst(
    typst_code = "#text(fill: navy)[*Fuel economy*]",
    x = 4.5,
    y = 33,
    size = 14
  )
```

### 2. Render data-driven labels

Use `geom_typst()` or the math geoms when every row needs its own rendered
label.

```r
library(ggplot2)
library(ggtypst)

df <- data.frame(
  x = c(1, 2, 3),
  y = c(1, 4, 9),
  label = c(r"(x)", r"(x^2)", r"(\\frac{x^3}{3})")
)

ggplot(df, aes(x, y, label = label)) +
  geom_point() +
  geom_math_mitex(nudge_y = 0.4, size = 11)
```

### 3. Style the whole theme

Use `element_typst()` and the math theme elements when Typst should drive plot
titles, axis titles, strips, or legends.

```r
library(ggplot2)
library(ggtypst)

ggplot(mtcars, aes(wt, mpg)) +
  geom_point() +
  labs(
    title = "$E = mc^2$",
    x = "$w_t$",
    y = "$mpg$"
  ) +
  theme(
    plot.title = element_math_typst(size = 16),
    axis.title.x = element_math_typst(size = 12),
    axis.title.y = element_math_typst(size = 12)
  )
```

## Learn more

- Start with the Get Started article for installation notes, mental model, and
  first plots.
- Use the Reference section for full function-level documentation.
- Check the changelog for feature progress and API updates.
