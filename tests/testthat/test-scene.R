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

test_that("runs follow Typst's order and clip regions", {
  runs <- typst_test_grob("#box(fill: yellow)[Hi] H")$children[[1]]$scene$runs
  expect_equal(runs$kind, c("path", "glyph"))
  expect_equal(runs$n_rows, c(1L, 3L))
  expect_equal(runs$segment, c(1L, 1L))

  # The clipped glyphs, then the glyph after the box, in separate segments.
  clipped <- typst_test_grob("#box(clip: true, width: 5pt)[HHH] H")
  runs <- clipped$children[[1]]$scene$runs
  expect_equal(runs$clip, c(1L, 0L))
  expect_equal(runs$segment, c(1L, 2L))
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

test_that("glyph outlines are computed only when text is drawn as paths", {
  scene <- typst_test_grob("Hello $x^2$")$children[[1]]
  expect_null(scene$scene$outlines)

  count <- new.env()
  count$calls <- 0
  compute <- rs_glyph_outlines
  local_mocked_bindings(
    rs_glyph_outlines = function(...) {
      count$calls <- count$calls + 1
      compute(...)
    }
  )

  with_renderer("glyph", grid::makeContent(scene))
  expect_equal(count$calls, 0)
  with_renderer("path", grid::makeContent(scene))
  with_renderer("path", grid::makeContent(scene))
  expect_equal(count$calls, 1)
})

test_that("outlines are found again for a scene from another session", {
  scene <- typst_render(build_typst_source("Hxg"))$scene
  expected <- glyph_outlines(scene)

  # Font store indices from another session no longer point at the font.
  scene$fonts$store_index <- .Machine$integer.max
  expect_equal(glyph_outlines(scene), expected)

  scene$fonts$cache_key <- "Missing-0-0000000000000000"
  expect_error(
    glyph_outlines(scene),
    "Cannot draw glyph outlines",
    class = "ggtypst_typst_error"
  )
})
