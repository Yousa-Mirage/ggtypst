typst_test_grob <- function(code, ...) {
  rendered <- typst_svg(build_typst_source(code))
  positioned_typst_grob(rendered, ...)
}

test_that("Typst output is drawn at its exact size", {
  skip_if_not_installed("svglite")

  box <- "#box(width: 20pt, height: 10pt, fill: black)"

  ink <- ink_extent(typst_test_grob(box))
  expect_pt_equal(ink$right - ink$left, 20)
  expect_pt_equal(ink$bottom - ink$top, 10)

  ink <- ink_extent(typst_test_grob(box, scale = 2))
  expect_pt_equal(ink$right - ink$left, 40)
  expect_pt_equal(ink$bottom - ink$top, 20)
})

test_that("curved outlines are flattened finely enough to look smooth", {
  skip_if_not_installed("svglite")

  grob <- typst_test_grob("#circle(radius: 10pt, fill: black)")

  s <- svglite::svgstring(width = 4, height = 3, standalone = FALSE)
  grid::grid.newpage()
  grid::grid.draw(grob)
  grDevices::dev.off()

  svg <- as.character(s())
  outline <- regmatches(svg, regexpr("<polygon points='[^']+'", svg))
  nums <- as.numeric(unlist(regmatches(outline, gregexpr("-?[0-9]+(\\.[0-9]+)?", outline))))
  xs <- nums[c(TRUE, FALSE)]
  ys <- nums[c(FALSE, TRUE)]

  # Largest angle between consecutive outline vertices, seen from the centre.
  # Without oversampling grImport2 leaves ~10 degree facets on this circle.
  angles <- sort(atan2(ys - mean(range(ys)), xs - mean(range(xs))))
  max_step <- max(diff(c(angles, angles[1] + 2 * pi))) * 180 / pi

  expect_lt(max_step, 6)
})

test_that("single-line and multi-line labels render glyphs at the same size", {
  skip_if_not_installed("svglite")

  single <- ink_extent(typst_test_grob("Hello"))
  double <- ink_extent(typst_test_grob("Hello #linebreak() Hello"))

  expect_pt_equal(double$right - double$left, single$right - single$left)
})

test_that("justification is relative to the Typst layout box, not the padded canvas", {
  skip_if_not_installed("svglite")

  box <- "#box(width: 20pt, height: 10pt, fill: black)"
  rendered <- typst_svg(build_typst_source(box))

  bottom_left <- ink_extent(positioned_typst_grob(rendered, hjust = 0, vjust = 0))
  top_right <- ink_extent(positioned_typst_grob(rendered, hjust = 1, vjust = 1))

  # Moving the anchor from one corner of the layout box to the other shifts the
  # drawing by exactly the layout box size.
  expect_pt_equal(bottom_left$left - top_right$left, rendered$width_pt)
  expect_pt_equal(top_right$top - bottom_left$top, rendered$height_pt)
})
