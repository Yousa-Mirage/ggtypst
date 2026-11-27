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

  expect_equal(
    normalizePath(dirname(file), winslash = "/"),
    normalizePath(cache, winslash = "/")
  )

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

# Count how often scene grobs are built, while still building them. Starts
# from an empty cache of drawn children, which other tests share.
count_builds <- function(env = parent.frame()) {
  rm(
    list = ls(scene_children_cache, all.names = TRUE),
    envir = scene_children_cache
  )
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
    if (if_cairo_works()) {
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
  skip_if_not(if_cairo_works(), "needs working Cairo devices")

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

# Ink of a rendered label drawn as native glyphs without `glyphInfo(rot = )`,
# as on R before 4.5, and as outlines, which do not depend on R's version.
turned_ink <- function(rendered) {
  local_mocked_bindings(glyph_rot_supported = function() FALSE)
  with_renderer("glyph", raster_ink(positioned_typst_grob(rendered)))
}

outline_ink <- function(rendered) {
  with_renderer("path", raster_ink(positioned_typst_grob(rendered)))
}

# How a scene is drawn on a device that draws glyphs, with or without
# `glyphInfo(rot = )`.
mode_with_rot <- function(scene, rot) {
  local_mocked_bindings(glyph_rot_supported = function() rot)
  with_renderer("glyph", typst_render_mode(scene))
}

test_that("without per-glyph rotation, a rotated label is drawn in a turned viewport", {
  skip_if_not_installed("ragg")
  skip_if_not_installed("png")

  for (angle in c(90, 30)) {
    rendered <- typst_render(
      build_typst_source("Hxample", size = 20, angle = angle)
    )
    expect_lt(
      max(abs(turned_ink(rendered) - outline_ink(rendered))),
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
  expect_equal(mode_with_rot(scene, TRUE), "glyph")
  expect_equal(mode_with_rot(scene, FALSE), "path")

  # A label rotated as a whole keeps native glyphs.
  rotated <- scene_grob("Hxample", angle = 90)$scene
  expect_equal(mode_with_rot(rotated, FALSE), "glyph")
})

test_that("glyph info built directly is what glyphInfo() returns", {
  skip_if_not(glyph_rot_supported())
  expect_true(glyph_info_direct_ok())

  scene <- typst_render(
    build_typst_source(
      "*Bold* #text(red.transparentize(50%))[_red_]",
      angle = 30
    )
  )$scene
  glyphs <- scene$glyphs
  rows <- seq_along(glyphs$id)
  args <- list(
    id = glyphs$id[rows],
    x = glyphs$x[rows] * 2,
    y = glyphs$y[rows] * 2,
    font = glyphs$font[rows],
    size = glyphs$size[rows] * 2,
    col = glyphs$fill[rows],
    rot = glyphs$rot[rows],
    font_list = glyph_font_list(scene$fonts),
    width = 40,
    height = 12
  )
  expect_identical(
    do.call(direct_glyph_info, args),
    do.call(grdevices_glyph_info, args)
  )
})

test_that("glyphInfo() is only called when glyph info cannot be built directly", {
  skip_if_not(glyph_rot_supported())
  local_mocked_bindings(
    grdevices_glyph_info = function(...) stop("glyphInfo() was called")
  )
  scene <- scene_grob("Hello")
  expect_no_error(scene_children(scene, "glyph"))

  local_mocked_bindings(glyph_info_direct_ok = function() FALSE)
  expect_error(scene_children(scene, "glyph"), "glyphInfo\\(\\) was called")
})

test_that("visual: annotations, geoms, and theme elements as native glyphs", {
  labels <- data.frame(
    wt = c(2, 3.2, 4.2, 5),
    mpg = c(30, 26, 22, 14),
    vs = c(0, 0, 1, 1),
    label = c(
      "Plain",
      "*bold* and _italic_",
      "$frac(a, b) + sqrt(x)$",
      "#box(fill: yellow, inset: 2pt)[in a box]"
    ),
    angle = c(0, 30, 0, -15)
  )
  p <- ggplot(mtcars, aes(wt, mpg, colour = factor(gear))) +
    geom_point(size = 1) +
    geom_typst(
      aes(label = label, angle = angle),
      data = labels,
      colour = "black",
      size = 10
    ) +
    annotate_math_typst(
      "integral_0^1 f(x) dif x",
      x = 4.6,
      y = 32,
      size = 12
    ) +
    annotate_typst(
      "first line\\ second, longer line",
      x = 1.6,
      y = 12,
      hjust = 0,
      size = 9
    ) +
    annotate_typst(
      "#text(stroke: 0.4pt + red)[stroked: drawn as paths]",
      x = 4,
      y = 12,
      size = 9
    ) +
    facet_wrap(~vs, labeller = label_both) +
    labs(
      title = "*Native glyphs* in a ggplot",
      subtitle = "Every Typst label here, except the stroked one",
      x = "Weight",
      y = "frac(\"miles\", \"gallon\")",
      colour = "*Gears*"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      plot.title = element_typst(size = 14),
      plot.subtitle = element_typst(colour = "grey30"),
      axis.title.x = element_typst(),
      axis.title.y = element_math_typst(size = 12),
      strip.text = element_typst(face = "bold"),
      legend.title = element_typst()
    )

  expect_svglite_snapshot("glyph-mode-plot", p)
})
