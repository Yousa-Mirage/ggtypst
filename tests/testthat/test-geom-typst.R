test_that("geom_typst batches labels into a single panel grob", {
  df <- data.frame(
    x = c(1, 2, 3),
    y = c(1, 2, 3),
    label = c("A", "B", "C")
  )

  p <- ggplot(df, aes(x, y, label = label)) +
    geom_typst()
  grob <- layer_grob(p)[[1]]

  expect_s3_class(grob, "gTree")
  expect_length(grob$children, 3)
})

test_that("geom_typst maps size aesthetics to rendered grob dimensions", {
  df <- data.frame(
    x = c(1, 2),
    y = c(1, 2),
    size = c(3, 9)
  )

  p <- ggplot(df, aes(x, y, size = size)) +
    geom_typst(label = "scale") +
    scale_size_identity() +
    coord_cartesian(xlim = c(0, 3), ylim = c(0, 3))

  grobs <- layer_grob(p)[[1]]

  widths <- vapply(
    grobs$children,
    function(child) {
      grid::convertWidth(grid::grobWidth(child), "pt", valueOnly = TRUE)
    },
    numeric(1)
  )

  expect_gt(widths[[2]], widths[[1]])
  expect_equal(widths[[2]] / widths[[1]], 3, tolerance = 1e-6)
})

test_that("geom_typst converts mapped size according to size.unit", {
  df_pt <- data.frame(x = 1, y = 1, size = 11)
  df_mm <- data.frame(x = 1, y = 1, size = 11 / ggplot2::.pt)

  p_pt <- ggplot(df_pt, aes(x, y, size = size)) +
    geom_typst(label = "scale", size.unit = "pt") +
    scale_size_identity()

  p_mm <- ggplot(df_mm, aes(x, y, size = size)) +
    geom_typst(label = "scale", size.unit = "mm") +
    scale_size_identity()

  width_pt <- grid::convertWidth(
    grid::grobWidth(layer_grob(p_pt)[[1]]$children[[1]]),
    "pt",
    valueOnly = TRUE
  )
  width_mm <- grid::convertWidth(
    grid::grobWidth(layer_grob(p_mm)[[1]]$children[[1]]),
    "pt",
    valueOnly = TRUE
  )

  expect_equal(width_mm, width_pt, tolerance = 1e-6)

  expect_snapshot(geom_typst(label = "scale", size.unit = "px"), error = TRUE)
})

test_that("geom_typst validates face", {
  expect_snapshot(geom_typst(label = "scale", face = "oblique"), error = TRUE)
})

test_that("geom_typst validates mapped face vectors before row rendering", {
  df <- data.frame(
    x = c(1, 2),
    y = c(1, 2),
    label = c("ok", "still ok"),
    face = c("plain", "oblique")
  )

  p <- ggplot(df, aes(x, y, label = label, face = face)) +
    geom_typst()

  expect_snapshot(ggplotGrob(p), error = TRUE)
})

test_that("geom_typst reports row and label context for render failures", {
  df <- data.frame(
    x = c(1, 2),
    y = c(1, 2),
    label = c("ok", "BROKEN_LABEL_123 [")
  )

  p <- ggplot(df, aes(x, y, label = label)) +
    geom_typst()

  expect_snapshot(ggplotGrob(p), error = TRUE)
})

test_that("geom_typst visual stability with mapped style coverage", {
  skip_if_not_installed("vdiffr")

  df <- data.frame(
    x = c(1.9, 2.3, 3.4, 4.8, 5.2),
    y = c(33, 29.1, 21.8, 28.4, 12.6),
    label = c(
      "Default",
      "Color + alpha",
      "$sum_(i=1)^n i = (n(n+1))/2$",
      "Angle",
      "Anchor"
    ),
    colour = c("black", "#D20F39", "#1E66F5", "#40A02B", "#DF8E1D"),
    alpha = c(NA, 0.7, 1, 1, 1),
    size = c(11, 11, 13, 12, 12),
    angle = c(0, 0, 0, 20, 0),
    face = c("plain", "bold", "plain", "bold.italic", "italic"),
    hjust = c(0.5, 0.5, 0.5, 0.5, 0),
    vjust = c(0.5, 0.5, 0.5, 0.5, 1)
  )

  p <- ggplot(df, aes(x, y, label = label)) +
    geom_point(size = 1.2, alpha = 0.5, colour = "grey40") +
    geom_typst(
      aes(
        colour = colour,
        alpha = alpha,
        size = size,
        angle = angle,
        face = face,
        hjust = hjust,
        vjust = vjust
      ),
      size.unit = "pt"
    ) +
    scale_colour_identity() +
    scale_alpha_identity() +
    scale_size_identity() +
    coord_cartesian(xlim = c(1.6, 5.6), ylim = c(10, 35)) +
    theme_minimal(base_size = 11)

  vdiffr::expect_doppelganger("geom-typst-parameter-coverage", p)
})

test_that("geom_typst system fonts visual check (local only)", {
  skip_if_not_installed("vdiffr")
  testthat::skip_on_ci()
  testthat::skip_on_cran()

  arial <- suppressWarnings(typst_svg(build_typst_source("probe", family = "Arial")))
  courier <- suppressWarnings(typst_svg(build_typst_source("probe", family = "Courier New")))

  if (length(arial$warnings) > 0 || length(courier$warnings) > 0) {
    skip("Required system fonts are not available: Arial/Courier New")
  }

  df <- data.frame(
    x = c(1.8, 5.4),
    y = c(33, 11.5),
    label = c("Arial sample", "Courier New sample"),
    family = c("Arial", "Courier New"),
    hjust = c(0.5, 1),
    vjust = c(0.5, 0)
  )

  p <- ggplot(df, aes(x, y, label = label, family = family, hjust = hjust, vjust = vjust)) +
    geom_point(size = 1.1, alpha = 0.5, colour = "grey45") +
    geom_typst(size = 12) +
    coord_cartesian(xlim = c(1.5, 5.6), ylim = c(10, 35)) +
    theme_minimal(base_size = 11)

  vdiffr::expect_doppelganger("geom-typst-system-fonts-local", p)
})

test_that("geom_typst visual size.unit pt and mm agree", {
  skip_if_not_installed("vdiffr")

  df <- data.frame(
    x = c(2.1, 4.7),
    y = c(26, 26),
    size = c(16, 16 / ggplot2::.pt),
    unit = c("pt", "mm"),
    colour = c("#1E66F5", "#D20F39")
  )

  p <- ggplot(df, aes(x, y)) +
    geom_point(size = 1.1, alpha = 0.5, colour = "grey45") +
    geom_typst(
      data = df[df$unit == "pt", , drop = FALSE],
      aes(size = size, colour = colour),
      label = "16 pt",
      size.unit = "pt"
    ) +
    geom_typst(
      data = df[df$unit == "mm", , drop = FALSE],
      aes(size = size, colour = colour),
      label = "16 pt via mm",
      size.unit = "mm"
    ) +
    scale_colour_identity() +
    scale_size_identity() +
    coord_cartesian(xlim = c(1.5, 5.3), ylim = c(22, 30)) +
    theme_minimal(base_size = 11)

  vdiffr::expect_doppelganger("geom-typst-size-unit-pt-vs-mm", p)
})

test_that("geom_typst visual inward outward just with facets and legend", {
  skip_if_not_installed("vdiffr")

  make_anchor_panel <- function(labels, just, colour, group, panel) {
    data.frame(
      x = c(0.08, 0.92, 0.08, 0.92),
      y = c(0.88, 0.88, 0.12, 0.12),
      label = labels,
      hjust = rep(just, 4),
      vjust = rep(just, 4),
      colour = rep(colour, 4),
      group = rep(group, 4),
      panel = rep(panel, 4)
    )
  }

  df <- rbind(
    make_anchor_panel(
      labels = c("NW inward", "NE inward", "SW inward", "SE inward"),
      just = "inward",
      colour = "#1E66F5",
      group = "Inward",
      panel = "Inward anchors"
    ),
    make_anchor_panel(
      labels = c("$alpha$ outward", "$beta$ outward", "$gamma$ outward", "$delta$ outward"),
      just = "outward",
      colour = "#D20F39",
      group = "Outward",
      panel = "Outward anchors"
    )
  )

  df$group <- factor(df$group, levels = c("Inward", "Outward"))
  df$panel <- factor(df$panel, levels = c("Inward anchors", "Outward anchors"))

  p <- ggplot(df, aes(x, y, label = label, colour = group, hjust = hjust, vjust = vjust)) +
    geom_point(size = 1.2, alpha = 0.5, show.legend = FALSE) +
    geom_typst(show.legend = TRUE, size = 14) +
    scale_colour_manual(values = c(Inward = "#1E66F5", Outward = "#D20F39"), name = "Anchor mode") +
    coord_cartesian(xlim = c(0, 1), ylim = c(0, 1), clip = "off") +
    facet_wrap(~panel) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = "bottom",
      plot.margin = margin(12, 18, 12, 18),
      panel.grid.minor = element_blank()
    )

  vdiffr::expect_doppelganger("geom-typst-facets-legend-inward-outward", p)
})
