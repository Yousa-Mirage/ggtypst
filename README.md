# ggtypst

`ggtypst` brings Typst-powered text and math rendering to `ggplot2`.

It lets you add rich Typst labels to plot annotations, data-driven layers, and
theme text elements without requiring a local Typst or LaTeX installation. The
package renders through a Rust backend and returns SVG-based grid grobs that fit
into normal `ggplot2` workflows.

## Features

- `annotate_typst()` for one-off Typst annotations
- `geom_typst()` for data-driven Typst text layers
- `element_typst()` for Typst-rendered theme text
- `annotate_math_typst()`, `geom_math_typst()`, and `element_math_typst()` for
  Typst math
- `annotate_math_mitex()`, `geom_math_mitex()`, and `element_math_mitex()` for
  LaTeX-style math converted through MiTeX

## Installation

`ggtypst` is not on CRAN yet.

Install it from GitHub with:

```r
pak::pak("Yousa-Mirage/ggtypst")
```

System requirements:

- R >= 4.2
- `cargo`
- `rustc >= 1.89.0`
- `xz`

You do not need a separate Typst or LaTeX installation.

## Examples

### Typst annotations

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

### Data-driven math labels

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

### Typst theme text

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

## License

MIT.
