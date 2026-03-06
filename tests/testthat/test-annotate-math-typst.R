test_that("annotate_math_typst visual stability and delimiter normalization", {
  skip_if_not_installed("vdiffr")

  huge_typst_math_code <- r"(
    cal(F)(s) = integral_(-oo)^(+oo) ( sum_(n=1)^oo ((-1)^(n-1) x^(2n)) / ((2n)!) ) / ( root(3, lim_(k->oo) product_(j=1)^k (1 - 1/p_j^2) + (alpha^2)/(beta^3) ) ) e^(-2 pi i s x) dif x + mat(
      sum_(i=1)^n i^3 = ( (n(n+1)) / 2 )^2,
      underbrace(a_1 + a_2 + dots.c + a_k, text("Total: ") k text(" terms"));
      cases(
        (integral_0^x e^(-t^2) dif t) / (sqrt(pi) + sum_(m=1)^M 1/m) & text("if ") x > 0,
        vec(alpha, (beta + gamma)/(delta), zeta) & text("otherwise")
      ),
      overbrace(product_(p " prime") (1 - p^(-s))^(-1), zeta(s))
    )
  )"

  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point(size = 1.2, alpha = 0.4, colour = "grey40") +
    coord_cartesian(xlim = c(1.6, 5.6), ylim = c(10, 35)) +
    theme_minimal(base_size = 11) +
    annotate_math_typst(
      typst_math_code = r"(sum_(i=1)^n i = (n(n+1))/2)",
      x = 2.0,
      y = 33,
      size = 13
    ) +
    annotate_math_typst(
      typst_math_code = r"($$ a^2 + b^2 = c^2 $$)",
      x = 5,
      y = 32,
      size = 13,
      scale = 1.5,
      angle = -30,
      color = "#D20F39"
    ) +
    annotate_math_typst(
      typst_math_code = r"($ e^(i pi) + 1 = 0 $)",
      x = 5.3,
      y = 12.4,
      hjust = 1,
      vjust = 0,
      size = 20,
      color = "#1E66F5",
      alpha = 0.2
    ) +
    annotate_math_typst(
      typst_math_code = huge_typst_math_code,
      x = 3.6,
      y = 20,
      size = 16
    )

  vdiffr::expect_doppelganger("annotate-math-typst-delimiter-normalization", p)
})

test_that("annotate_math_typst normalization yields equivalent layer parameters", {
  base_code <- r"(a^2 + b^2 = c^2)"

  layer_plain <- annotate_math_typst(
    base_code,
    x = 3.0,
    y = 28.0,
    hjust = 1,
    vjust = 0,
    size = 13,
    color = "#1E66F5",
    scale = 1.2
  )

  layer_wrapped <- annotate_math_typst(
    r"($$ a^2 + b^2 = c^2 $$)",
    x = 3.0,
    y = 28.0,
    hjust = 1,
    vjust = 0,
    size = 13,
    color = "#1E66F5",
    scale = 1.2
  )

  layer_typst <- annotate_typst(
    r"($ a^2 + b^2 = c^2 $)",
    x = 3.0,
    y = 28.0,
    hjust = 1,
    vjust = 0,
    size = 13,
    color = "#1E66F5",
    scale = 1.2
  )

  layer_signature <- function(layer) {
    gp <- layer$geom_params
    grob <- gp$grob

    list(
      x = c(gp$xmin, gp$xmax),
      y = c(gp$ymin, gp$ymax),
      grob_class = class(grob),
      width_pt = grid::convertWidth(grid::grobWidth(grob), "pt", valueOnly = TRUE),
      height_pt = grid::convertHeight(grid::grobHeight(grob), "pt", valueOnly = TRUE)
    )
  }

  sig_plain <- layer_signature(layer_plain)
  expect_equal(layer_signature(layer_wrapped), sig_plain)
  expect_equal(layer_signature(layer_typst), sig_plain)
})

test_that("annotate_math_typst supports inline rendering mode", {
  layer_display <- annotate_math_typst(
    typst_math_code = r"(frac(1, 2))",
    x = 3,
    y = 25,
    inline = FALSE,
    size = 12
  )

  layer_inline <- annotate_math_typst(
    typst_math_code = r"(frac(1, 2))",
    x = 3,
    y = 25,
    inline = TRUE,
    size = 12
  )

  h_display <- grid::convertHeight(
    grid::grobHeight(layer_display$geom_params$grob),
    "pt",
    valueOnly = TRUE
  )
  h_inline <- grid::convertHeight(
    grid::grobHeight(layer_inline$geom_params$grob),
    "pt",
    valueOnly = TRUE
  )
  expect_gt(h_display, h_inline)
})

test_that("annotate_math_typst visual display vs inline", {
  skip_if_not_installed("vdiffr")

  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point(size = 1.1, alpha = 0.35, colour = "grey45") +
    coord_cartesian(xlim = c(1.6, 5.6), ylim = c(10, 35)) +
    theme_minimal(base_size = 11) +
    annotate_math_typst(
      typst_math_code = r"(frac(1, 2) + sqrt(3))",
      x = 2.2,
      y = 31,
      size = 13,
      color = "#1E66F5"
    ) +
    annotate_math_typst(
      typst_math_code = r"(frac(1, 2) + sqrt(3))",
      x = 4.9,
      y = 31,
      size = 13,
      color = "#D20F39",
      inline = TRUE
    )

  vdiffr::expect_doppelganger("annotate-math-typst-display-vs-inline", p)
})
