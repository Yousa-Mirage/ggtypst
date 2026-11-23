# Native glyph rendering, requested with options(ggtypst.renderer = "glyph").
# The rest of the suite draws text as outlines (see helper.R).

scene_grob <- function(code, ...) {
  positioned_typst_grob(typst_render(build_typst_source(code, ...)))$children[[
    1
  ]]
}

test_that("the renderer option chooses glyphs or outlines", {
  scene <- scene_grob("Hello $x^2/2$")
  glyph <- with_renderer("glyph", grid::makeContent(scene))$children
  path <- with_renderer("path", grid::makeContent(scene))$children

  expect_s3_class(glyph[[1]], "glyphgrob")
  expect_s3_class(path[[1]], "pathgrob")

  # Shapes (here the fraction bar) are drawn the same way in both modes.
  expect_length(glyph, length(path))
})

test_that("text glyphs cannot express falls back to outlines", {
  scene <- scene_grob("#text(stroke: 0.3pt + red)[Hello]")
  expect_false(scene$scene$glyph_ok)

  children <- with_renderer("glyph", grid::makeContent(scene))$children
  expect_s3_class(children[[1]], "pathgrob")
})

test_that("glyphs are drawn where the outlines are", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("png")

  for (code in c(
    "Hxample long text",
    "$integral_0^1 x^2 dif x$",
    "#rotate(30deg)[Hxg]"
  )) {
    grob <- positioned_typst_grob(
      typst_render(
        build_typst_source(code, size = 14)
      ),
      hjust = 0,
      vjust = 0
    )

    path <- with_renderer("path", raster_ink(grob))
    glyph <- with_renderer("glyph", raster_ink(grob))

    # Within a pixel: outlines are not hinted, glyphs may be.
    expect_lt(max(abs(glyph - path)), 0.15)
  }
})

test_that("glyphs rasterize like grid text, unlike outlines", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("png")
  skip_if_not_installed("systemfonts")

  label <- "Hxample long text"
  rendered <- typst_render(build_typst_source(label, size = 11))

  # Make the font Typst used (embedded in ggtypst) available to grid text too.
  family <- "ggtypst test serif"
  systemfonts::register_font(
    family,
    plain = typst_font_files(rendered$scene$fonts)[1]
  )
  on.exit(systemfonts::clear_registry(), add = TRUE)

  ink <- function(grob) {
    file <- tempfile(fileext = ".png")
    ragg::agg_png(file, 2.2, 0.4, units = "in", res = 96)
    grid::grid.draw(grob)
    grDevices::dev.off()
    sum(1 - png::readPNG(file)[,, 1])
  }
  text <- ink(grid::textGrob(
    label,
    gp = grid::gpar(fontfamily = family, fontsize = 11)
  ))
  glyph <- with_renderer("glyph", ink(positioned_typst_grob(rendered)))
  path <- with_renderer("path", ink(positioned_typst_grob(rendered)))

  expect_equal(glyph / text, 1, tolerance = 0.01)
  # Outlines are not hinted, so at screen resolution they look heavier.
  expect_gt(path / text, 1.05)
})

test_that("embedded fonts are written once to the font cache", {
  cache <- tempfile("font-cache")
  old <- options(ggtypst.font_cache = cache)
  on.exit(options(old), add = TRUE)

  fonts <- typst_render(build_typst_source("Hello"))$scene$fonts
  expect_equal(fonts$path, "")

  file <- typst_font_files(fonts)
  expect_equal(dirname(file), cache)
  # An OpenType font with CFF outlines (Libertinus) starts with "OTTO".
  expect_identical(readBin(file, "raw", 4), charToRaw("OTTO"))
  expect_length(list.files(cache, all.files = TRUE, no.. = TRUE), 1)

  written <- file.mtime(file)
  expect_equal(typst_font_files(fonts), file)
  expect_equal(file.mtime(file), written)
})

test_that("embedded fonts are written from a stale store index", {
  fonts <- typst_render(build_typst_source("Hello"))$scene$fonts
  write <- function(fonts) {
    old <- options(ggtypst.font_cache = tempfile("font-cache"))
    on.exit(options(old))
    file <- typst_font_files(fonts)
    readBin(file, "raw", file.size(file))
  }
  expected <- write(fonts)

  # As in a scene saved in another session: the cache key finds the font.
  fonts$store_index <- .Machine$integer.max
  expect_identical(write(fonts), expected)

  fonts$cache_key <- "Missing-0-0000000000000000"
  expect_error(
    write(fonts),
    "Cannot write a font file",
    class = "ggtypst_typst_error"
  )
})

# Count how often scene grobs are built, while still building them.
count_builds <- function(env = parent.frame()) {
  count <- new.env()
  count$builds <- 0
  build <- scene_grobs
  local_mocked_bindings(
    scene_grobs = function(...) {
      count$builds <- count$builds + 1
      build(...)
    },
    .env = env
  )
  function() count$builds
}

test_that("drawn children are reused for each renderer", {
  scene <- scene_grob("Hello $x^2/2$")
  builds <- count_builds()

  path <- with_renderer("path", grid::makeContent(scene))$children
  expect_identical(
    with_renderer("path", grid::makeContent(scene))$children,
    path
  )
  expect_equal(builds(), 1)

  glyph <- with_renderer("glyph", grid::makeContent(scene))$children
  expect_s3_class(glyph[[1]], "glyphgrob")
  with_renderer("glyph", grid::makeContent(scene))
  with_renderer("path", grid::makeContent(scene))
  expect_equal(builds(), 2)
})

test_that("cached glyphs are rebuilt when their font files are gone", {
  old <- options(ggtypst.font_cache = tempfile("font-cache"))
  on.exit(options(old), add = TRUE)
  scene <- scene_grob("Hello")
  builds <- count_builds()

  with_renderer("glyph", grid::makeContent(scene))
  expect_equal(builds(), 1)

  # Another font cache, as in a new R session reading the grob back.
  options(ggtypst.font_cache = tempfile("font-cache"))
  with_renderer("glyph", grid::makeContent(scene))
  expect_equal(builds(), 2)

  # A deleted font file is written again.
  file <- typst_font_files(scene$scene$fonts)
  unlink(file)
  with_renderer("glyph", grid::makeContent(scene))
  expect_equal(builds(), 3)
  expect_true(file.exists(file))
})

test_that("labels with the same fonts share one font list", {
  # Start empty, and leave no mocked font lists behind for later tests.
  clear <- function() rm(list = ls(font_lists), envir = font_lists)
  clear()
  on.exit(clear(), add = TRUE)
  count <- new.env()
  count$made <- 0
  local_mocked_bindings(
    glyphFontList = function(...) {
      count$made <- count$made + 1
      list(...)
    },
    .package = "grDevices"
  )

  hello <- typst_render(build_typst_source("Hello"))$scene$fonts
  world <- typst_render(build_typst_source("World"))$scene$fonts
  expect_identical(glyph_font_list(hello), glyph_font_list(world))
  expect_equal(count$made, 1)

  glyph_font_list(typst_render(build_typst_source("*Bold*"))$scene$fonts)
  expect_equal(count$made, 2)
})

test_that("an invalid renderer option is reported", {
  scene <- scene_grob("Hello")
  expect_error(
    with_renderer("bitmap", grid::makeContent(scene)),
    "ggtypst.renderer"
  )
})

# How a scene would be drawn on a freshly opened device.
mode_on <- function(scene, open_device) {
  open_device()
  on.exit(grDevices::dev.off())
  typst_render_mode(scene)
}

test_that("auto draws glyphs where devices rasterize or embed fonts", {
  scene <- scene_grob("Hello")$scene
  with_renderer("auto", {
    expect_equal(mode_on(scene, function() grDevices::pdf(NULL)), "path")
    expect_equal(
      mode_on(scene, function() grDevices::postscript(tempfile())),
      "path"
    )
    if (requireNamespace("ragg", quietly = TRUE)) {
      expect_equal(
        mode_on(scene, function() ragg::agg_png(tempfile())),
        "glyph"
      )
    }
    if (requireNamespace("svglite", quietly = TRUE)) {
      expect_equal(
        mode_on(scene, function() svglite::svglite(tempfile())),
        "glyph"
      )
    }
    if (isTRUE(capabilities("cairo"))) {
      expect_equal(
        mode_on(scene, function() grDevices::cairo_pdf(tempfile())),
        "glyph"
      )
      expect_equal(
        mode_on(scene, function() grDevices::png(tempfile(), type = "cairo")),
        "glyph"
      )
    }
  })

  # Auto is the default.
  with_renderer(
    NULL,
    expect_equal(mode_on(scene, function() grDevices::pdf(NULL)), "path")
  )
})

test_that("auto draws outlines when glyphs cannot express the text", {
  skip_if_not_installed("ragg")

  scene <- scene_grob("#text(stroke: 0.3pt + red)[Hello]")$scene
  with_renderer(
    "auto",
    expect_equal(mode_on(scene, function() ragg::agg_png(tempfile())), "path")
  )
})

test_that("ggsave() output gets outlines in pdf() and embedded fonts in cairo_pdf()", {
  skip_if_not(isTRUE(capabilities("cairo")), "needs cairo")

  p <- ggplot() + annotate_typst("Hello $x^2$", x = 0, y = 0) + theme_void()

  # Search the bytes: a PDF is not text in any locale.
  has_font <- function(file) {
    length(grepRaw(
      "/FontFile",
      readBin(file, "raw", file.size(file)),
      fixed = TRUE
    )) >
      0
  }
  with_renderer("auto", {
    pdf_file <- tempfile(fileext = ".pdf")
    ggsave(pdf_file, p, width = 2, height = 1, device = grDevices::pdf)
    expect_false(has_font(pdf_file))

    cairo_file <- tempfile(fileext = ".pdf")
    ggsave(cairo_file, p, width = 2, height = 1, device = grDevices::cairo_pdf)
    expect_true(has_font(cairo_file))
  })
})

# Draw a rendered label with or without `glyphInfo(rot = )`.
rotation_ink <- function(rendered, rot) {
  local_mocked_bindings(glyph_rot_supported = function() rot)
  with_renderer("glyph", raster_ink(positioned_typst_grob(rendered)))
}

test_that("without per-glyph rotation, a rotated label is drawn in a turned viewport", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("png")

  for (angle in c(90, 30)) {
    rendered <- typst_render(
      build_typst_source("Hxample", size = 20, angle = angle)
    )
    expect_lt(
      max(abs(rotation_ink(rendered, FALSE) - rotation_ink(rendered, TRUE))),
      0.15
    )
  }

  local_mocked_bindings(glyph_rot_supported = function() FALSE)
  scene <- scene_grob("Hxample", angle = 90)
  child <- with_renderer("glyph", grid::makeContent(scene))$children[[1]]
  expect_equal(child$vp$angle, 90)
  expect_s3_class(child$children[[1]], "glyphgrob")
})

test_that("without per-glyph rotation, unrotated text needs no viewport", {
  local_mocked_bindings(glyph_rot_supported = function() FALSE)
  scene <- scene_grob("Hello")
  child <- with_renderer("glyph", grid::makeContent(scene))$children[[1]]
  expect_s3_class(child, "glyphgrob")
})

test_that("without per-glyph rotation, labels mixing angles are drawn as outlines", {
  scene <- scene_grob("H #box(rotate(30deg)[H]) H")$scene
  expect_equal(with_renderer("glyph", typst_render_mode(scene)), "glyph")

  local_mocked_bindings(glyph_rot_supported = function() FALSE)
  expect_equal(with_renderer("glyph", typst_render_mode(scene)), "path")
  # A label rotated as a whole keeps native glyphs.
  rotated <- scene_grob("Hxample", angle = 90)$scene
  expect_equal(with_renderer("glyph", typst_render_mode(rotated)), "glyph")
})
