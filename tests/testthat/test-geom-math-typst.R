test_that("geom_math_typst normalizes outer math delimiters", {
  get_grob <- function(label) {
    p <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) +
      geom_math_typst(label = label, size = 13)
    grob <- layer_grob(p)[[1]]$children[[1]]
  }

  grob_plain <- get_grob("a^2 + b^2 = c^2")
  grob_single <- get_grob("$ a^2 + b^2 = c^2 $")
  grob_double <- get_grob("$$ a^2 + b^2 = c^2 $$")

  expect_equal(
    grid::convertWidth(grid::grobWidth(grob_single), "pt", valueOnly = TRUE),
    grid::convertWidth(grid::grobWidth(grob_plain), "pt", valueOnly = TRUE),
    tolerance = 1e-6
  )
  expect_equal(
    grid::convertWidth(grid::grobWidth(grob_double), "pt", valueOnly = TRUE),
    grid::convertWidth(grid::grobWidth(grob_plain), "pt", valueOnly = TRUE),
    tolerance = 1e-6
  )
})

test_that("geom_math_typst supports inline rendering mode", {
  p_display <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) +
    geom_math_typst(label = "frac(1, 2)", inline = FALSE, size = 12)

  p_inline <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) +
    geom_math_typst(label = "frac(1, 2)", inline = TRUE, size = 12)

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

test_that("geom_math_typst supports fontface alias", {
  layer <- geom_math_typst(label = "x^2", fontface = "bold")

  expect_equal(layer$aes_params$face, "bold")
})

test_that("geom_math_typst supports static label parameters", {
  p <- ggplot(data.frame(x = 1, y = 1), aes(x, y)) +
    geom_math_typst(label = "x^2", size = 12)

  expect_length(layer_grob(p)[[1]]$children, 1)
})

test_that("geom_math_typst validates static math face", {
  expect_snapshot(geom_math_typst(label = "x^2", face = "italic"), error = TRUE)
})

test_that("geom_math_typst validates mapped math face before rendering", {
  df <- data.frame(
    x = c(1, 2),
    y = c(1, 2),
    label = c("x^2", "y^2"),
    face = c("plain", "italic")
  )

  p <- ggplot(df, aes(x, y, label = label, face = face)) +
    geom_math_typst()

  expect_snapshot(ggplotGrob(p), error = TRUE)
})

test_that("geom_math_typst reports row and label context for invalid math", {
  df <- data.frame(
    x = c(1, 2),
    y = c(1, 2),
    label = c("x^2", "$x$ + $y$")
  )

  p <- ggplot(df, aes(x, y, label = label)) +
    geom_math_typst()

  expect_snapshot(ggplotGrob(p), error = TRUE)
})
