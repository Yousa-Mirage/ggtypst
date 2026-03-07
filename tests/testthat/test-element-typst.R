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

# rotate_just

test_that("rotate_just remaps justifications across quadrants", {
  expect_equal(
    rotate_just(angle = 0, hjust = 0.2, vjust = 0.8),
    list(hjust = 0.2, vjust = 0.8)
  )
  expect_equal(
    rotate_just(angle = 90, hjust = 0.2, vjust = 0.8),
    list(hjust = 0.2, vjust = 0.2)
  )
  expect_equal(
    rotate_just(angle = 180, hjust = 0.2, vjust = 0.8),
    list(hjust = 0.8, vjust = 0.2)
  )
  expect_equal(
    rotate_just(angle = 270, hjust = 0.2, vjust = 0.8),
    list(hjust = 0.8, vjust = 0.8)
  )
})

test_that("rotate_just normalizes negative and large angles", {
  expect_equal(
    rotate_just(angle = -90, hjust = 0.25, vjust = 0.75),
    rotate_just(angle = 270, hjust = 0.25, vjust = 0.75)
  )
  expect_equal(
    rotate_just(angle = 450, hjust = 0.25, vjust = 0.75),
    rotate_just(angle = 90, hjust = 0.25, vjust = 0.75)
  )
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
    theme(axis.text.x = element_typst(size = 10, angle = 45, hjust = 1))

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
