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
