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

  outline <- drawn_subpaths(typst_test_grob("#circle(radius: 10pt, fill: black)"))[[1]]
  expect_gt(nrow(outline), 16)

  # Largest angle between consecutive outline vertices, seen from the centre.
  centre <- colMeans(apply(outline, 2, range))
  angles <- sort(atan2(outline[, 2] - centre[2], outline[, 1] - centre[1]))
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
    t(vapply(drawn_subpaths(grob), function(m) range(m[, 1]), numeric(2)))
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

test_that("the layout box spans cap height to baseline", {
  heights <- vapply(
    c("HHH", "ace", "Ågy", "$x$", "$integral_0^1 x^2$", "a $x^2$"),
    function(code) typst_svg(build_typst_source(code, size = 10))$height_pt,
    numeric(1)
  )
  expect_equal(unname(heights), rep(heights[[1]], 6))
})

test_that("vjust = 0 puts the last baseline on the anchor", {
  skip_if_not_installed("svglite")

  # Boxes sit on the baseline; both are lower than the cap height.
  for (code in c(
    "#box(width: 10pt, height: 4pt, fill: black)",
    "A #linebreak() #box(width: 10pt, height: 4pt, fill: black)"
  )) {
    ink <- ink_extent(typst_test_grob(code, vjust = 0))
    expect_pt_equal(ink$bottom, 108)
  }
})

test_that("baselines are 1.2 * lineheight font sizes apart", {
  skip_if_not_installed("svglite")

  # Line edges apply to text, so each line holds an "H", whose flat bottom
  # sits on the baseline.
  line_bottoms <- function(lineheight, prefix = "") {
    code <- paste0(prefix, "H #linebreak() H")
    rendered <- typst_svg(build_typst_source(code, size = 10, lineheight = lineheight))
    vapply(drawn_subpaths(positioned_typst_grob(rendered)), function(m) max(m[, 2]), numeric(1))
  }

  for (lineheight in c(1.2, 0.9, 0.5)) {
    bottoms <- line_bottoms(lineheight)
    expect_pt_equal(diff(sort(bottoms)), 1.2 * lineheight * 10)
  }

  # A font size set in the Typst code itself is followed too.
  bottoms <- line_bottoms(1.2, prefix = "#set text(size: 20pt)\n")
  expect_pt_equal(diff(sort(bottoms)), 1.2 * 1.2 * 20)
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

test_that("text and shapes are drawn in Typst's order", {
  scene <- typst_test_grob("#box(fill: yellow)[Hi] H")$children[[1]]

  for (mode in c("path", "glyph")) {
    children <- with_renderer(mode, grid::makeContent(scene))$children

    # The box background first, then the text inside and after it in one run.
    expect_length(children, 2)
    expect_s3_class(children[[1]], "pathgrob")
    expect_equal(children[[1]]$gp$fill, "#FFDC00FF")
    expect_s3_class(children[[2]], if (mode == "glyph") "glyphgrob" else "pathgrob")
  }
})

test_that("dashed strokes are drawn as separate dashes", {
  skip_if_not_installed("svglite")

  grob <- typst_test_grob("#line(length: 30pt, stroke: (paint: black, dash: (4pt, 2pt)))")
  dashes <- drawn_subpaths(grob)

  expect_length(dashes, 5)

  for (dash in dashes) {
    expect_pt_equal(diff(range(dash[, 1])), 4)
  }
})

test_that("clipped groups are clipped", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("png")

  # A 5pt wide clipped box holding much wider text.
  grob <- typst_test_grob("#box(clip: true, width: 5pt, height: 8pt)[HHHHHH]", hjust = 0, vjust = 0)
  ink <- raster_ink(grob)

  expect_lt(ink[["right"]] - ink[["left"]], 5.2)
  expect_gt(ink[["right"]] - ink[["left"]], 4)
})

test_that("content paths cannot draw falls back to the SVG", {
  scene <- typst_test_grob(r"(#text(fill: gradient.linear(red, blue))[Hello])")$children[[1]]
  expect_false(scene$scene$path_ok)

  skip_if_not_installed("rsvg")
  skip_if_not_installed("grImport2")

  children <- grid::makeContent(scene)$children
  expect_s3_class(children[[1]], "typst_svg_fallback")
})

test_that("without rsvg, the fallback warns once and draws what it can", {
  local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")
  scene <- typst_test_grob(r"(#text(fill: gradient.linear(red, blue))[Hello] plain)")$children[[1]]

  rlang::reset_message_verbosity("ggtypst_svg_fallback")
  expect_warning(children <- grid::makeContent(scene)$children, "rsvg")
  expect_no_warning(grid::makeContent(scene))

  # The plain text is still drawn as outlines.
  expect_s3_class(children[[1]], "pathgrob")
})
