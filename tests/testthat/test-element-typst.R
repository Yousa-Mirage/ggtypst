# element_typst constructor

test_that("element_typst() returns correct class", {
  el <- element_typst()
  expect_s3_class(el, c("element_typst", "element_text", "element"))
})

test_that("element_typst() stores parameters", {
  el <- element_typst(
    family = "Arial",
    face = "bold",
    size = 14,
    colour = "red",
    hjust = 0,
    vjust = 1,
    angle = 45,
    lineheight = 1.2,
    margin = margin(5, 5, 5, 5),
    math_family = "STIX Two Math",
    size.unit = "mm",
    debug = TRUE,
    inherit.blank = TRUE
  )

  expect_equal(el$family, "Arial")
  expect_equal(el$face, "bold")
  expect_equal(el$size, 14)
  expect_equal(el$colour, "red")
  expect_equal(el$hjust, 0)
  expect_equal(el$vjust, 1)
  expect_equal(el$angle, 45)
  expect_equal(el$lineheight, 1.2)
  expect_equal(el$math_family, "STIX Two Math")
  expect_equal(el$size.unit, "mm")
  expect_true(el$debug)
  expect_true(el$inherit.blank)
})

test_that("element_typst() color alias works", {
  el <- element_typst(color = "blue")
  expect_equal(el$colour, "blue")

  expect_snapshot(
    element_typst(colour = "red", color = "blue"),
    error = TRUE
  )
})

test_that("element_typst() fontface alias works", {
  el <- element_typst(fontface = "bold")
  expect_equal(el$face, "bold")

  expect_snapshot(
    element_typst(face = "plain", fontface = "bold"),
    error = TRUE
  )
})

test_that("element_typst() size.unit validates input", {
  expect_snapshot(element_typst(size.unit = "inches"), error = TRUE)
})

test_that("element_typst() defaults are NULL / FALSE", {
  el <- element_typst()
  expect_null(el$family)
  expect_null(el$face)
  expect_null(el$size)
  expect_null(el$colour)
  expect_null(el$hjust)
  expect_null(el$vjust)
  expect_null(el$angle)
  expect_null(el$lineheight)
  expect_null(el$margin)
  expect_null(el$math_family)
  expect_equal(el$size.unit, "pt")
  expect_null(el$debug)
  expect_false(el$inherit.blank)
})


# element_grob rendering

test_that("element_grob.element_typst produces titleGrob", {
  grob <- element_typst(size = 11) |>
    element_grob(label = "Hello Typst")

  expect_s3_class(grob, "titleGrob")
  expect_true(!is.null(grob$widths))
  expect_true(!is.null(grob$heights))
})

test_that("element_grob.element_typst returns zeroGrob for NULL label", {
  grob <- element_typst(size = 11) |>
    element_grob(el, label = NULL)

  expect_s3_class(grob, "zeroGrob")
})

test_that("element_grob.element_typst returns zeroGrob for empty string", {
  el <- element_typst(size = 11)
  expect_s3_class(element_grob(el, label = ""), "zeroGrob")
  expect_s3_class(element_grob(el, label = "  "), "zeroGrob")
  expect_s3_class(element_grob(el, label = c("", " ")), "zeroGrob")
})

test_that("element_grob.element_typst drops blank entries and syncs x/y", {
  # Provide explicit x positions so we can verify synchronisation
  grob <- element_typst(size = 11) |>
    element_grob(
      label = c("a", "", "b"),
      x = grid::unit(c(0.1, 0.5, 0.9), "npc"),
      y = grid::unit(c(0.2, 0.5, 0.8), "npc")
    )
  expect_s3_class(grob, "titleGrob")

  # The inner typst_grob should have exactly 2 children (blank dropped)
  expect_equal(length(grob$children[[1]]$children), 2)
})

test_that("element_grob.element_typst handles different labels", {
  grob_vector <- element_typst(size = 11) |>
    element_grob(label = c("a", "b", "c"))

  expect_s3_class(grob_vector, "titleGrob")

  grob_math <- element_typst(size = 12, colour = "navy") |>
    element_grob(label = "$ x^2 + y^2 = r^2 $")

  expect_s3_class(grob_math, "titleGrob")
})

test_that("element_grob.element_typst lineheight changes rendered height", {
  label <- "first line #linebreak() second line"

  grob_tight <- element_typst(size = 12, lineheight = 0.8) |>
    element_grob(label = label)
  grob_loose <- element_typst(size = 12, lineheight = 1.8) |>
    element_grob(label = label)

  height_tight <- grid::convertHeight(grid::grobHeight(grob_tight), "pt", valueOnly = TRUE)
  height_loose <- grid::convertHeight(grid::grobHeight(grob_loose), "pt", valueOnly = TRUE)

  expect_gt(height_loose, height_tight)
})

test_that("element_grob.element_typst anchors rotated grobs by rotated justification", {
  grob <- element_typst(size = 12, angle = 90, vjust = 1) |>
    element_grob(label = "Test")

  just <- grob$children[[1]]$children[[1]]$vp[[1]]$justification
  expect_equal(unname(just), c(0, 0.5))
})

# Visual regression tests

test_that("element_typst visual: plot title", {
  skip_if_not_installed("vdiffr")

  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point() +
    ggtitle("$ E = m c^2 $") +
    theme(plot.title = element_typst(size = 20))

  vdiffr::expect_doppelganger("element-typst-plot-title", p)
})

test_that("element_typst visual: bold axis label", {
  skip_if_not_installed("vdiffr")

  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point() +
    labs(x = "*Weight* (1000 lbs)") +
    theme(axis.title.x = element_typst(size = 12, face = "bold"))

  vdiffr::expect_doppelganger("element-typst-bold-axis-label", p)
})

test_that("element_typst visual: rotated axis text", {
  skip_if_not_installed("vdiffr")

  p <- ggplot(mtcars, aes(factor(cyl), mpg)) +
    geom_boxplot() +
    theme(axis.text.x = element_typst(size = 10, angle = 45))

  vdiffr::expect_doppelganger("element-typst-rotated-axis-text", p)
})

test_that("element_typst visual: margin handling", {
  skip_if_not_installed("vdiffr")

  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point() +
    ggtitle("Title with Margins") +
    theme(
      plot.title = element_typst(
        size = 14,
        margin = margin(30, 0, 30, 0)
      )
    )

  vdiffr::expect_doppelganger("element-typst-margin-title", p)
})

test_that("element_typst visual: facet strip", {
  skip_if_not_installed("vdiffr")

  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point() +
    facet_wrap(~cyl) +
    theme(strip.text = element_typst(size = 12, face = "bold.italic"))

  vdiffr::expect_doppelganger("element-typst-facet-strip", p)
})

test_that("element_typst visual: subtitle caption debug size-unit", {
  skip_if_not_installed("vdiffr")

  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point(colour = "grey35", alpha = 0.8, size = 1.6) +
    labs(
      title = r"( $"Amazing Matrix Title:" mat(delim: #none, 1, 2; alpha, beta) times vec(Delta, 3) = vec(2 + Delta, alpha + 3) $)",
      subtitle = "Subtitle in 8 mm",
      caption = "Caption in Typst"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_typst(size = 14),
      plot.subtitle = element_typst(size = 8, size.unit = "mm", debug = TRUE),
      plot.caption = element_typst(size = 10, face = "italic", colour = "#1E66F5")
    )

  vdiffr::expect_doppelganger("element-typst-subtitle-caption-debug-size-unit", p)
})

test_that("element_typst visual: axis title y and legend text", {
  skip_if_not_installed("vdiffr")

  p <- ggplot(mtcars, aes(wt, mpg, colour = factor(cyl))) +
    geom_point(size = 2) +
    labs(
      x = "$ text(\"Weight\") + omega dot 10^3 $",
      y = "$alpha$ + $beta$ + $Gamma$ + 中文 + § + ± + Ω + ℃",
      colour = "Legend *Title*"
    ) +
    scale_colour_brewer(
      palette = "Dark2",
      labels = c("4 cylinders", "$6$ cylinders", "8 cylinders")
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.title.x = element_typst(size = 14, colour = "#7287FD"),
      axis.title.y = element_typst(size = 14, face = "bold"),
      legend.title = element_typst(size = 11, colour = "#D20F39"),
      legend.text = element_typst(size = 10)
    )

  vdiffr::expect_doppelganger("element-typst-axis-title-y-legend", p)
})

test_that("element_typst visual: inherit blank on x title", {
  skip_if_not_installed("vdiffr")

  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point(colour = "grey35", alpha = 0.8, size = 1.6) +
    labs(x = "Hidden X title", y = "Visible Y title") +
    theme_minimal(base_size = 11) +
    theme(
      axis.title = element_blank(),
      axis.title.x = element_typst(size = 12, debug = TRUE, inherit.blank = TRUE),
      axis.title.y = element_typst(size = 12, colour = "#40A02B")
    )

  vdiffr::expect_doppelganger("element-typst-inherit-blank-axis-title-x", p)
})

test_that("element_typst visual: facet strip x and y", {
  skip_if_not_installed("vdiffr")

  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point(colour = "grey35", alpha = 0.75, size = 1.4) +
    facet_grid(vs ~ gear, labeller = label_both) +
    theme_minimal(base_size = 14) +
    theme(
      strip.text = element_blank(),
      strip.text.x = element_typst(size = 14, face = "bold", colour = "#1E66F5"),
      strip.text.y = element_typst(size = 14, face = "italic", colour = "#D20F39")
    )

  vdiffr::expect_doppelganger("element-typst-facet-strip-x-y", p)
})

test_that("element_typst visual: multiline lineheight", {
  skip_if_not_installed("vdiffr")

  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point(colour = "grey35", alpha = 0.8, size = 1.6) +
    labs(
      title = "tight #linebreak() spacing",
      subtitle = "loose #linebreak() spacing"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title = element_typst(size = 14, lineheight = 0.8, colour = "#1E66F5"),
      plot.subtitle = element_typst(size = 14, lineheight = 1.8, colour = "#D20F39")
    )

  vdiffr::expect_doppelganger("element-typst-lineheight", p)
})
