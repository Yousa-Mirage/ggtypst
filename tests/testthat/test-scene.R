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

test_that("gradients are drawn as images in place of the text", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("png")

  code <- r"(#text(fill: FILL)[Hxample])"
  label <- function(fill) {
    rendered <- typst_render(build_typst_source(
      sub("FILL", fill, code),
      size = 30
    ))
    positioned_typst_grob(rendered)
  }
  gradient <- label("gradient.linear(black, luma(20%))")
  scene <- gradient$children[[1]]
  expect_equal(scene$scene$runs$kind, "raster")

  for (mode in c("glyph", "path")) {
    children <- with_renderer(mode, grid::makeContent(scene))$children
    expect_s3_class(children[[1]], "rastergrob")
  }
  # The image covers the text's ink, within a pixel at 300 dpi.
  expect_lt(max(abs(raster_ink(gradient) - raster_ink(label("black")))), 0.3)
})

test_that("the resolution of images follows ggtypst.raster_dpi", {
  code <- build_typst_source(
    "#rect(width: 72pt, height: 36pt, fill: gradient.linear(red, blue))"
  )
  pixels <- function(dpi) {
    old <- options(ggtypst.raster_dpi = dpi)
    on.exit(options(old))
    rasters <- typst_render(code)$scene$rasters
    c(rasters$width_px, rasters$height_px)
  }
  # One inch wide, plus a pixel of margin on each side.
  expect_equal(pixels(300)[1], 302)
  expect_equal(pixels(150)[1], 152)

  old <- options(ggtypst.raster_dpi = -1)
  on.exit(options(old))
  expect_error(typst_render(code), "ggtypst.raster_dpi")
})

test_that("colour glyphs are images only when text is drawn as paths", {
  scene <- typst_test_grob("H \U0001F600")$children[[1]]
  skip_if_not(
    any(scene$scene$glyphs$colour),
    "needs a colour emoji font"
  )

  kinds <- function(mode) {
    children <- with_renderer(mode, grid::makeContent(scene))$children
    vapply(children, function(child) class(child)[1], character(1))
  }
  expect_false("rastergrob" %in% kinds("glyph"))
  expect_true("rastergrob" %in% kinds("path"))
  # As paths, the emoji is not also drawn by its outline.
  outlines <- glyph_outlines(scene$scene)
  expect_equal(outlines$n_points[scene$scene$glyphs$colour], 0L)
})

test_that("nested clip regions are intersected", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("png")

  # A tall clip region around a wide one: only their 30pt square shows.
  grob <- typst_test_grob(
    r"(#box(clip: true, width: 30pt, height: 50pt)[#box(clip: true, width: 50pt, height: 30pt)[#rect(width: 50pt, height: 50pt, fill: black)]])",
    hjust = 0,
    vjust = 0
  )
  for (mode in c("glyph", "path")) {
    ink <- with_renderer(mode, raster_ink(grob))
    expect_equal(unname(ink[["right"]] - ink[["left"]]), 30, tolerance = 0.01)
    expect_equal(unname(ink[["bottom"]] - ink[["top"]]), 30, tolerance = 0.01)
  }
})

test_that("devices without groups clip nested regions by the innermost one", {
  scene <- typst_test_grob(
    r"(#box(clip: true, width: 30pt)[#box(clip: true, width: 20pt)[HHHH]])"
  )$children[[1]]
  has_group <- function(grob) {
    inherits(grob, "GridGroup") ||
      any(vapply(grob$children %||% list(), has_group, logical(1)))
  }

  local_mocked_bindings(device_draws_groups = function() FALSE)
  expect_no_warning(children <- grid::makeContent(scene)$children)
  expect_false(has_group(children[[1]]))

  # A device that can draw groups gets them, not the cached children.
  local_mocked_bindings(device_draws_groups = function() TRUE)
  expect_true(has_group(grid::makeContent(scene)$children[[1]]))
})

test_that("visual: content drawn as images and nested clip regions", {
  skip_if_no_vdiffr()
  skip_if_not_installed("png")
  # A lower resolution keeps the snapshot small.
  old <- options(ggtypst.raster_dpi = 150)
  on.exit(options(old), add = TRUE)

  expect_svglite_snapshot("image-content-paths", image_content_plot(), "path")
  expect_svglite_snapshot("image-content-glyphs", image_content_plot())
})

test_that("visual: colour emoji as native glyphs and as images", {
  skip_if_no_vdiffr()
  # Emoji come from the system's colour font, which differs between systems.
  fonts <- typst_render(build_typst_source("\U0001F600"))$scene$fonts
  skip_if_not(
    identical(fonts$family, "Apple Color Emoji"),
    "needs Apple Color Emoji"
  )
  old <- options(ggtypst.raster_dpi = 150)
  on.exit(options(old), add = TRUE)

  p <- ggplot() +
    annotate_typst(
      "Emoji \U0001F600 \U0001F680 in text",
      x = 0,
      y = 0,
      size = 16
    ) +
    coord_cartesian(xlim = c(-1, 1), ylim = c(-1, 1)) +
    theme_void()
  expect_svglite_snapshot("emoji-paths", p, "path", width = 4, height = 1.5)
  expect_svglite_snapshot("emoji-glyphs", p, width = 4, height = 1.5)
})
