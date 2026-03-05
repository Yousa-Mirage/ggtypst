test_that("annotate_typst visual stability with default Typst fonts", {
  skip_if_not_installed("vdiffr")

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point(size = 1.2, alpha = 0.45, colour = "grey40") +
    ggplot2::coord_cartesian(xlim = c(1.6, 5.6), ylim = c(10, 35)) +
    ggplot2::theme_minimal(base_size = 11) +
    annotate_typst(
      typst_code = "Default",
      x = 1.9,
      y = 33
    ) +
    annotate_typst(
      typst_code = "Color + alpha",
      x = 2.15,
      y = 29.2,
      color = "#D20F39",
      alpha = 0.75
    ) +
    annotate_typst(
      typst_code = "$sum_(i=1)^n i = (n(n+1))/2$",
      x = 3.55,
      y = 32.5,
      size = 13
    ) +
    annotate_typst(
      typst_code = "Angle",
      x = 5.35,
      y = 27.8,
      angle = 20,
      size = 12
    ) +
    annotate_typst(
      typst_code = "Scaled",
      x = 5.3,
      y = 12.4,
      hjust = 1,
      vjust = 0,
      scale = 1.2,
      dpi = 240,
      size = 12
    ) +
    annotate_typst(
      typst_code = "hjust/vjust",
      x = 3.5,
      y = 10.8,
      hjust = 0,
      vjust = 1,
      size = 11,
      color = "#1E66F5"
    )

  vdiffr::expect_doppelganger("annotate-typst-default-font-parameter-coverage", p)
})

test_that("annotate_typst system fonts visual check (local only)", {
  skip_if_not_installed("vdiffr")
  testthat::skip_on_ci()
  testthat::skip_on_cran()

  arial <- suppressWarnings(typst_svg(build_typst_source("probe", family = "Arial")))
  courier <- suppressWarnings(typst_svg(build_typst_source("probe", family = "Courier New")))

  if (length(arial$warnings) > 0 || length(courier$warnings) > 0) {
    skip("Required system fonts are not available: Arial/Courier New")
  }

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point(size = 1.1, alpha = 0.35, colour = "grey45") +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::coord_cartesian(xlim = c(1.5, 5.6), ylim = c(10, 35)) +
    annotate_typst(
      typst_code = "Arial sample",
      x = 1.8,
      y = 33,
      family = "Arial",
      size = 12
    ) +
    annotate_typst(
      typst_code = "Courier New sample",
      x = 5.4,
      y = 11.5,
      hjust = 1,
      vjust = 0,
      family = "Courier New",
      size = 12
    )

  vdiffr::expect_doppelganger("annotate-typst-system-fonts-local", p)
})

test_that("annotate_math_typst visual stability and delimiter normalization", {
  skip_if_not_installed("vdiffr")

  huge_math_code <- r"(
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

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(wt, mpg)) +
    ggplot2::geom_point(size = 1.2, alpha = 0.4, colour = "grey40") +
    ggplot2::coord_cartesian(xlim = c(1.6, 5.6), ylim = c(10, 35)) +
    ggplot2::theme_minimal(base_size = 11) +
    annotate_math_typst(
      typst_math_code = "sum_(i=1)^n i = (n(n+1))/2",
      x = 2.0,
      y = 33,
      size = 13
    ) +
    annotate_math_typst(
      typst_math_code = "$$ a^2 + b^2 = c^2 $$",
      x = 5,
      y = 32,
      size = 13,
      scale = 1.5,
      angle = -30,
      color = "#D20F39"
    ) +
    annotate_math_typst(
      typst_math_code = "$ e^(i pi) + 1 = 0 $",
      x = 5.3,
      y = 12.4,
      hjust = 1,
      vjust = 0,
      size = 20,
      color = "#1E66F5",
      alpha = 0.2
    ) +
    annotate_math_typst(
      typst_math_code = huge_math_code,
      x = 3.5,
      y = 20,
      size = 15
    )

  vdiffr::expect_doppelganger("annotate-math-typst-delimiter-normalization", p)
})

test_that("annotate_math_typst normalization yields equivalent layer parameters", {
  base_code <- "a^2 + b^2 = c^2"

  layer_plain <- annotate_math_typst(
    base_code,
    x = 3.0,
    y = 28.0,
    hjust = 1,
    vjust = 0,
    size = 13,
    color = "#1E66F5",
    scale = 1.2,
    dpi = 240
  )

  layer_wrapped <- annotate_math_typst(
    "$$ a^2 + b^2 = c^2 $$",
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
    "$ a^2 + b^2 = c^2 $",
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
