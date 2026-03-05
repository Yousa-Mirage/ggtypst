test_that("annotate_math_mitex visual stability and delimiter normalization", {
  skip_if_not_installed("vdiffr")

  huge_latex_math_code <- r"(
    \mathcal{F}(s) = \int_{-\infty}^{+\infty} \frac{\sum_{n=1}^{\infty} \frac{(-1)^{n-1} x^{2n}}{(2n)!}}{\sqrt[3]{\lim_{k \to \infty} \prod_{j=1}^{k} \left(1 - \frac{1}{p_j^2}\right) + \frac{\alpha^2}{\beta^3}}} e^{-2 \pi i s x} \,\mathrm{d}x + \begin{pmatrix} \sum_{i=1}^{n} i^3 = \left( \frac{n(n+1)}{2} \right)^2 & \underbrace{a_1 + a_2 + \cdots + a_k}_{\text{Total: } k \text{ terms}} \\ \begin{cases} \frac{\int_{0}^{x} e^{-t^2} \,\mathrm{d}t}{\sqrt{\pi} + \sum_{m=1}^{M} \frac{1}{m}} & \text{if } x > 0 \\ \begin{pmatrix} \alpha \\ \frac{\beta + \gamma}{\delta} \\ \zeta \end{pmatrix} & \text{otherwise} \end{cases} & \overbrace{\prod_{p \text{ prime}} (1 - p^{-s})^{-1}}^{\zeta(s)} \end{pmatrix}
  )"

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point(size = 1.2, alpha = 0.4, colour = "grey40") +
    ggplot2::coord_cartesian(xlim = c(1.6, 5.6), ylim = c(10, 35)) +
    ggplot2::theme_minimal(base_size = 11) +
    annotate_math_mitex(
      latex_math_code = r"(\frac{1}{2} + \sqrt{3})",
      x = 2.0,
      y = 33,
      size = 13
    ) +
    annotate_math_mitex(
      latex_math_code = r"($$ \sum_{i=1}^{n} i = \frac{n(n+1)}{2} $$)",
      x = 4.8,
      y = 32,
      size = 12.5,
      color = "#D20F39"
    ) +
    annotate_math_mitex(
      latex_math_code = r"($ \int_0^1 x^2 \, dx = \frac{1}{3} $)",
      x = 5.3,
      y = 12.4,
      hjust = 1,
      vjust = 0,
      size = 12,
      color = "#1E66F5"
    ) +
    annotate_math_mitex(
      latex_math_code = huge_latex_math_code,
      x = 3.6,
      y = 20,
      size = 15
    )

  vdiffr::expect_doppelganger("annotate-math-mitex-delimiter-normalization", p)
})

test_that("annotate_math_mitex normalization yields equivalent layer parameters", {
  base <- r"(\frac{1}{2} + \sqrt{3})"

  layer_plain <- annotate_math_mitex(
    latex_math_code = base,
    x = 3.0,
    y = 28.0,
    hjust = 1,
    vjust = 0,
    size = 13,
    color = "#1E66F5",
    scale = 1.2,
    dpi = 240
  )

  layer_wrapped <- annotate_math_mitex(
    latex_math_code = r"($$ \frac{1}{2} + \sqrt{3} $$)",
    x = 3.0,
    y = 28.0,
    hjust = 1,
    vjust = 0,
    size = 13,
    color = "#1E66F5",
    scale = 1.2,
    dpi = 240
  )

  layer_typst <- annotate_typst(
    typst_code = convert_latex_to_typst(base),
    x = 3.0,
    y = 28.0,
    hjust = 1,
    vjust = 0,
    size = 13,
    color = "#1E66F5",
    scale = 1.2,
    dpi = 240
  )

  layer_signature <- function(layer) {
    gp <- layer$geom_params

    list(
      x = c(gp$xmin, gp$xmax),
      y = c(gp$ymin, gp$ymax),
      hjust = gp$grob$hjust,
      vjust = gp$grob$vjust,
      width = gp$grob$width,
      height = gp$grob$height,
      raster_dim = dim(gp$grob$raster),
      raster_type = typeof(gp$grob$raster)
    )
  }

  sig_plain <- layer_signature(layer_plain)
  expect_equal(layer_signature(layer_wrapped), sig_plain)
  expect_equal(layer_signature(layer_typst), sig_plain)
})

test_that("annotate_math_mitex reports MiTeX conversion errors", {
  expect_error(
    annotate_math_mitex("\\end{}", x = 2, y = 20),
    "MiTeX conversion failed"
  )
})

test_that("annotate_math_mitex supports inline rendering mode", {
  layer_display <- annotate_math_mitex(
    latex_math_code = r"(\frac{1}{2})",
    x = 3,
    y = 25,
    inline = FALSE,
    size = 12
  )

  layer_inline <- annotate_math_mitex(
    latex_math_code = r"(\frac{1}{2})",
    x = 3,
    y = 25,
    inline = TRUE,
    size = 12
  )

  h_display <- as.numeric(layer_display$geom_params$grob$height)
  h_inline <- as.numeric(layer_inline$geom_params$grob$height)
  expect_gt(h_display, h_inline)
})

test_that("annotate_math_mitex visual display vs inline", {
  skip_if_not_installed("vdiffr")

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point(size = 1.1, alpha = 0.35, colour = "grey45") +
    ggplot2::coord_cartesian(xlim = c(1.6, 5.6), ylim = c(10, 35)) +
    ggplot2::theme_minimal(base_size = 11) +
    annotate_math_mitex(
      latex_math_code = r"(\frac{1}{2} + \sqrt{3})",
      x = 2.2,
      y = 31,
      size = 13,
      color = "#1E66F5"
    ) +
    annotate_math_mitex(
      latex_math_code = r"(\frac{1}{2} + \sqrt{3})",
      x = 4.9,
      y = 31,
      size = 13,
      color = "#D20F39",
      inline = TRUE
    )

  vdiffr::expect_doppelganger("annotate-math-mitex-display-vs-inline", p)
})
