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

test_that("lines of multi-line labels are aligned by hjust in every API", {
  skip_if_not_installed("svglite")

  # Two lines of different width; each box is drawn as its own polygon.
  label <- paste(
    "#box(width: 10pt, height: 5pt, fill: black)",
    "#linebreak()",
    "#box(width: 30pt, height: 5pt, fill: black)"
  )

  line_edges <- function(grob) {
    s <- svglite::svgstring(width = 4, height = 3, standalone = FALSE)
    grid::grid.newpage()
    grid::grid.draw(grob)
    grDevices::dev.off()
    svg <- as.character(s())
    polys <- regmatches(svg, gregexpr("<polygon points='[^']+'", svg))[[1]]
    t(vapply(
      polys,
      function(p) {
        v <- as.numeric(regmatches(p, gregexpr("-?[0-9]+(\\.[0-9]+)?", p))[[1]])
        range(v[c(TRUE, FALSE)])
      },
      numeric(2),
      USE.NAMES = FALSE
    ))
  }

  apis <- list(
    annotate = function(h) annotate_typst(label, x = 0, y = 0, hjust = h)$geom_params$grob,
    geom = function(h) {
      p <- ggplot(data.frame(x = 0, y = 0), aes(x, y)) + geom_typst(label = label, hjust = h)
      layer_grob(p)[[1]]
    },
    element = function(h) element_grob(element_typst(hjust = h), label = label)
  )

  for (api in names(apis)) {
    left <- line_edges(apis[[api]](0))
    expect_pt_equal(left[1, 1], left[2, 1])

    centre <- line_edges(apis[[api]](0.5))
    expect_pt_equal(mean(centre[1, ]), mean(centre[2, ]))

    right <- line_edges(apis[[api]](1))
    expect_pt_equal(right[1, 2], right[2, 2])
  }
})

test_that("ink overhanging the layout box is drawn, not clipped", {
  skip_if_not_installed("svglite")

  rendered <- typst_svg(build_typst_source('#text(style: "italic")[f]', size = 40))
  ink <- ink_extent(positioned_typst_grob(rendered, hjust = 0, vjust = 0.5))

  # With hjust = 0 the layout box starts at the device centre (144pt); the
  # italic f reaches past it on the left and beyond its advance on the right.
  expect_lt(ink$left, 144 - 1)
  expect_gt(ink$right, 144 + rendered$width_pt + 1)
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
