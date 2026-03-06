test_that("annotate_typst visual stability with default Typst fonts", {
  skip_if_not_installed("vdiffr")

  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point(size = 1.2, alpha = 0.45, colour = "grey40") +
    coord_cartesian(xlim = c(1.6, 5.6), ylim = c(10, 35)) +
    theme_minimal(base_size = 11) +
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

  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point(size = 1.1, alpha = 0.35, colour = "grey45") +
    theme_minimal(base_size = 11) +
    coord_cartesian(xlim = c(1.5, 5.6), ylim = c(10, 35)) +
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

test_that("annotate_typst uses vector backend by default in SVG output", {
  skip_if_not_installed("svglite")

  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point() +
    annotate_typst(
      typst_code = "$sum_(i=1)^n i$",
      x = 3,
      y = 25
    ) +
    theme_void()
  svg_str <- svglite::stringSVG(print(p), width = 5, height = 4)

  expect_false(grepl("<text\\b", svg_str))
  expect_false(grepl("<image\\b", svg_str))
  expect_true(grepl("<path\\b", svg_str))
})

test_that("annotate_typst validates placement arguments before rendering", {
  expect_snapshot(annotate_typst("[", x = 3, y = 25, scale = 0), error = TRUE)
  expect_snapshot(annotate_typst("[", x = 3, y = 25, hjust = Inf), error = TRUE)
  expect_snapshot(annotate_typst("[", x = 3, y = 25, vjust = Inf), error = TRUE)
})

test_that("annotate_typst converts size according to size.unit", {
  layer_pt <- annotate_typst(
    "scale",
    x = 3,
    y = 25,
    size = 11,
    size.unit = "pt"
  )

  layer_mm <- annotate_typst(
    "scale",
    x = 3,
    y = 25,
    size = 11 / ggplot2::.pt,
    size.unit = "mm"
  )

  width_pt <- grid::convertWidth(
    grid::grobWidth(layer_pt$geom_params$grob),
    "pt",
    valueOnly = TRUE
  )
  width_mm <- grid::convertWidth(
    grid::grobWidth(layer_mm$geom_params$grob),
    "pt",
    valueOnly = TRUE
  )

  expect_equal(width_mm, width_pt, tolerance = 1e-6)
})
