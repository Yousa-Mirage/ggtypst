test_that("geom_math_mitex converts LaTeX and renders a grob", {
  p <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) +
    geom_math_mitex(label = r"(\frac{1}{2})", size = 12)

  grob <- layer_grob(p)[[1]]
  expect_s3_class(grob, "gTree")
  expect_length(grob$children, 1)
})

test_that("geom_math_mitex normalizes outer LaTeX delimiters", {
  get_width <- function(label) {
    p <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) +
      geom_math_mitex(label = label, size = 13)
    grid::convertWidth(
      grid::grobWidth(layer_grob(p)[[1]]$children[[1]]),
      "pt",
      valueOnly = TRUE
    )
  }

  w_plain <- get_width(r"(\frac{1}{2} + \sqrt{3})")
  w_single <- get_width(r"($ \frac{1}{2} + \sqrt{3} $)")
  w_double <- get_width(r"($$ \frac{1}{2} + \sqrt{3} $$)")

  expect_equal(w_single, w_plain, tolerance = 1e-6)
  expect_equal(w_double, w_plain, tolerance = 1e-6)
})

test_that("geom_math_mitex supports inline rendering mode", {
  p_display <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) +
    geom_math_mitex(label = r"(\frac{1}{2})", inline = FALSE, size = 12)

  p_inline <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) +
    geom_math_mitex(label = r"(\frac{1}{2})", inline = TRUE, size = 12)

  h_display <- grid::convertHeight(
    grid::grobHeight(layer_grob(p_display)[[1]]$children[[1]]),
    "pt",
    valueOnly = TRUE
  )
  h_inline <- grid::convertHeight(
    grid::grobHeight(layer_grob(p_inline)[[1]]$children[[1]]),
    "pt",
    valueOnly = TRUE
  )

  expect_gt(h_display, h_inline)
})

test_that("geom_math_mitex supports fontface alias", {
  layer <- geom_math_mitex(label = r"(\frac{1}{2})", fontface = "bold")

  expect_equal(layer$aes_params$face, "bold")
})

test_that("geom_math_mitex supports static label parameters", {
  p <- ggplot(data.frame(x = c(1, 2), y = c(1, 2)), aes(x, y)) +
    geom_math_mitex(label = r"(\frac{1}{2})", size = 12)

  expect_length(layer_grob(p)[[1]]$children, 2)
})

test_that("geom_math_mitex validates static math face", {
  expect_snapshot(
    geom_math_mitex(label = r"(\frac{1}{2})", face = "italic"),
    error = TRUE
  )
})

test_that("geom_math_mitex validates mapped math face before rendering", {
  df <- data.frame(
    x = c(1, 2),
    y = c(1, 2),
    label = c(r"(\alpha)", r"(\beta)"),
    face = c("plain", "italic")
  )

  p <- ggplot(df, aes(x, y, label = label, face = face)) +
    geom_math_mitex()

  expect_snapshot(ggplotGrob(p), error = TRUE)
})

test_that("geom_math_mitex reports row and label context for conversion errors", {
  df <- data.frame(
    x = c(1, 2),
    y = c(1, 2),
    label = c(r"(\alpha)", r"(\end{})")
  )

  p <- ggplot(df, aes(x, y, label = label)) +
    geom_math_mitex()

  expect_snapshot(ggplotGrob(p), error = TRUE)
})

test_that("geom_math_mitex reports static label conversion errors", {
  expect_snapshot(
    geom_math_mitex(label = r"(\end{})"),
    error = TRUE
  )
})

test_that("geom_math_mitex visual delimiter normalization", {
  skip_if_not_installed("vdiffr")

  df <- data.frame(
    x = c(1.9, 3.4, 5.0),
    y = c(31, 31, 31),
    label = c(
      r"(\frac{1}{2} + \sqrt{3})",
      r"($ \frac{1}{2} + \sqrt{3} $)",
      r"($$ \frac{1}{2} + \sqrt{3} $$)"
    ),
    colour = c("#1E66F5", "#D20F39", "#40A02B"),
    face = c("plain", "bold", "plain")
  )

  p <- ggplot(df, aes(x, y, label = label, colour = colour, face = face)) +
    geom_point(aes(x, y), size = 1.1, alpha = 0.35, colour = "grey45", inherit.aes = FALSE) +
    geom_math_mitex(size = 13) +
    scale_colour_identity() +
    coord_cartesian(xlim = c(1.4, 5.5), ylim = c(28, 34)) +
    theme_minimal(base_size = 11)

  vdiffr::expect_doppelganger("geom-math-mitex-delimiter-normalization", p)
})

test_that("geom_math_mitex visual display vs inline with facets", {
  skip_if_not_installed("vdiffr")

  df <- data.frame(
    x = c(2.1, 3.0, 3.9, 2.1, 3.0, 3.9),
    y = c(30.2, 31.0, 31.8, 30.2, 31.0, 31.8),
    label = c(
      r"(\frac{1}{2} + \sqrt{3})",
      r"(\sum_{i=1}^{n} i)",
      r"(\int_0^1 x^2 \, dx)",
      r"(\frac{1}{2} + \sqrt{3})",
      r"(\sum_{i=1}^{n} i)",
      r"(\int_0^1 x^2 \, dx)"
    ),
    colour = c("#1E66F5", "#D20F39", "#40A02B", "#1E66F5", "#D20F39", "#40A02B"),
    mode = factor(rep(c("Display", "Inline"), each = 3), levels = c("Display", "Inline"))
  )

  p <- ggplot(df, aes(x, y)) +
    geom_point(size = 1.1, alpha = 0.35, colour = "grey45") +
    geom_math_mitex(
      data = df[df$mode == "Display", , drop = FALSE],
      aes(label = label, colour = colour),
      size = 13
    ) +
    geom_math_mitex(
      data = df[df$mode == "Inline", , drop = FALSE],
      aes(label = label, colour = colour),
      size = 13,
      inline = TRUE
    ) +
    scale_colour_identity() +
    coord_cartesian(xlim = c(1.6, 4.4), ylim = c(28.8, 32.2)) +
    facet_wrap(~mode) +
    theme_minimal(base_size = 11)

  vdiffr::expect_doppelganger("geom-math-mitex-display-vs-inline-facets", p)
})
