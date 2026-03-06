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
    geom_typst(label = "scale", show.legend = FALSE) +
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
    geom_typst(label = "scale", show.legend = FALSE, size.unit = "pt") +
    scale_size_identity()

  p_mm <- ggplot(df_mm, aes(x, y, size = size)) +
    geom_typst(label = "scale", show.legend = FALSE, size.unit = "mm") +
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
})
