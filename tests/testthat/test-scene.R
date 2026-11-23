# Drawing a scene with grid (R/scene.R): its plan, outlines, shapes, and clip
# regions. Text is drawn as outlines here (see helper.R).

test_that("curved outlines are flattened finely enough to look smooth", {
  skip_if_not_installed("svglite")

  outline <- drawn_subpaths(typst_test_grob(
    "#circle(radius: 10pt, fill: black)"
  ))[[1]]
  expect_gt(nrow(outline), 16)

  # Largest angle between consecutive outline vertices, seen from the centre.
  centre <- colMeans(apply(outline, 2, range))
  angles <- sort(atan2(outline[, 2] - centre[2], outline[, 1] - centre[1]))
  max_step <- max(diff(c(angles, angles[1] + 2 * pi))) * 180 / pi

  expect_lt(max_step, 6)
})

test_that("the drawing plan follows Typst's order and clip regions", {
  plan <- typst_test_grob("#box(fill: yellow)[Hi] H")$children[[1]]$plan
  expect_equal(
    vapply(plan$runs, function(r) r$kind, character(1)),
    c("shape", "glyph")
  )
  expect_equal(plan$runs[[2]]$rows, 1:3)
  expect_length(plan$segments, 1)

  # The clipped glyphs, then the glyph after the box, in separate segments.
  clipped <- typst_test_grob("#box(clip: true, width: 5pt)[HHH] H")
  plan <- clipped$children[[1]]$plan
  expect_equal(vapply(plan$runs, function(r) r$clip, integer(1)), c(1L, 0L))
  expect_equal(plan$segments, list(1L, 2L))
  expect_length(plan$clips, 1)
})

test_that("text and shapes are drawn in Typst's order", {
  scene <- typst_test_grob("#box(fill: yellow)[Hi] H")$children[[1]]

  for (mode in c("path", "glyph")) {
    children <- with_renderer(mode, grid::makeContent(scene))$children

    # The box background first, then the text inside and after it in one run.
    expect_length(children, 2)
    expect_s3_class(children[[1]], "pathgrob")
    expect_equal(children[[1]]$gp$fill, "#FFDC00FF")
    expect_s3_class(
      children[[2]],
      if (mode == "glyph") "glyphgrob" else "pathgrob"
    )
  }
})

test_that("dashed strokes are drawn as separate dashes", {
  skip_if_not_installed("svglite")

  grob <- typst_test_grob(
    "#line(length: 30pt, stroke: (paint: black, dash: (4pt, 2pt)))"
  )
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
  grob <- typst_test_grob(
    "#box(clip: true, width: 5pt, height: 8pt)[HHHHHH]",
    hjust = 0,
    vjust = 0
  )
  ink <- raster_ink(grob)

  expect_lt(ink[["right"]] - ink[["left"]], 5.2)
  expect_gt(ink[["right"]] - ink[["left"]], 4)
})
