# element_math_typst constructor

test_that("element_math_typst() returns correct class", {
  el <- element_math_typst()

  expect_true(inherits(el, "ggtypst::element_math_typst"))
  expect_true(inherits(el, "ggtypst::element_typst"))
  expect_true(inherits(el, "ggplot2::element_text"))
})

test_that("element_math_typst() stores parameters", {
  el <- element_math_typst(
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
    inherit.blank = TRUE,
    inline = TRUE
  )

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
  expect_true(el$inline)
})

test_that("element_math_typst() aliases and validation work", {
  el <- element_math_typst(color = "blue", fontface = "bold")
  expect_equal(el$colour, "blue")
  expect_equal(el$face, "bold")

  expect_snapshot(element_math_typst(size.unit = "inches"), error = TRUE)
  expect_snapshot(element_math_typst(face = "italic"), error = TRUE)
  expect_snapshot(element_math_typst(face = "plain", fontface = "bold"), error = TRUE)
  expect_snapshot(element_math_typst(colour = "red", color = "blue"), error = TRUE)
})

test_that("element_math_typst() defaults are NULL / FALSE", {
  el <- element_math_typst()

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
  expect_false(el$inline)
})

test_that("element_math_typst inherit.blank is resolved by ggplot2 theme merging", {
  calc_element <- utils::getFromNamespace("calc_element", "ggplot2")

  th_blank <- theme(
    axis.title = element_blank(),
    axis.title.x = element_math_typst(inherit.blank = TRUE)
  )
  th_keep <- theme(
    axis.title = element_blank(),
    axis.title.x = element_math_typst(inherit.blank = FALSE)
  )

  el_blank <- calc_element("axis.title.x", th_blank)
  el_keep <- calc_element("axis.title.x", th_keep)

  expect_true(inherits(el_blank, "ggplot2::element_blank"))
  expect_true(inherits(el_keep, "ggtypst::element_math_typst"))
})


# element_grob rendering

test_that("element_grob.element_math_typst produces titleGrob", {
  grob <- element_math_typst(size = 11) |>
    element_grob(label = "x^2 + y^2 = z^2")

  expect_s3_class(grob, "titleGrob")
  expect_true(!is.null(grob$widths))
  expect_true(!is.null(grob$heights))
})

test_that("element_grob.element_math_typst handles wrapped and unwrapped labels", {
  grob <- element_math_typst(size = 11) |>
    element_grob(label = c("x^2", "$ y^2 $", "$$ z^2 $$"))

  expect_s3_class(grob, "titleGrob")
  expect_equal(length(grob$children[[1]]$children), 3)
})

test_that("element_grob.element_math_typst returns zeroGrob for blank labels", {
  el <- element_math_typst(size = 11)

  expect_s3_class(element_grob(el, label = ""), "zeroGrob")
  expect_s3_class(element_grob(el, label = "  "), "zeroGrob")
})

test_that("element_grob.element_math_typst validates math-only face overrides", {
  expect_snapshot(
    element_grob(element_math_typst(), label = "x^2", face = "italic"),
    error = TRUE
  )
})


# Visual regression tests

test_that("element_math_typst visual: plot title", {
  skip_if_not_installed("vdiffr")

  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point() +
    ggtitle("x^2 + y^2 = z^2") +
    theme(plot.title = element_math_typst(size = 18))

  vdiffr::expect_doppelganger("element-math-typst-plot-title", p)
})

test_that("element_math_typst visual: bold axis title", {
  skip_if_not_installed("vdiffr")

  p <- ggplot(mtcars, aes(wt, mpg)) +
    geom_point() +
    labs(x = "sum_(i=1)^n x_i") +
    theme(axis.title.x = element_math_typst(size = 12, face = "bold"))

  vdiffr::expect_doppelganger("element-math-typst-bold-axis-title", p)
})
