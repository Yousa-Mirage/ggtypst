library(ggplot2)
options(device = function(...) grDevices::pdf(file = NULL, ...))

# Draw text as outlines everywhere, so visual snapshots do not depend on the
# fonts and glyph rendering of the machine running them. Glyph rendering has
# its own tests (test-glyph.R).
options(ggtypst.renderer = "path")

# Evaluate `code` with text drawn by `renderer` ("auto", "glyph", or "path").
with_renderer <- function(renderer, code) {
  old <- options(ggtypst.renderer = renderer)
  on.exit(options(old))
  force(code)
}

skip_if_no_vdiffr <- function() {
  testthat::skip_if_not_installed("vdiffr")

  run_vdiffr <- Sys.getenv(
    "VDIFFR_RUN_TESTS",
    unset = ifelse(nzchar(Sys.getenv("CI")), "false", "true")
  ) |>
    tolower()

  if (!run_vdiffr %in% c("true", "1", "yes")) {
    testthat::skip("Skipping visual regression tests.")
  }
}

# Draw a grob on a 4 x 3 in (288 x 216 pt) svglite device and return the
# extent of everything visibly drawn, in device points (svg y axis points down).
#
# Each shape's bounds are intersected with the clip rectangle of the group it
# is drawn in: Cairo may paint a fill that covers a whole picture as an
# oversized <rect> that only its clip path trims to size.
ink_extent <- function(grob) {
  s <- svglite::svgstring(width = 4, height = 3, standalone = FALSE)
  grid::grid.newpage()
  grid::grid.draw(grob)
  grDevices::dev.off()
  svg <- as.character(s())

  num <- function(x) {
    as.numeric(unlist(regmatches(x, gregexpr("-?[0-9]+(\\.[0-9]+)?", x))))
  }
  attr_num <- function(el, name) {
    num(regmatches(el, regexpr(sprintf("\\s%s='[^']+'", name), el)))
  }

  clip_ids <- regmatches(
    svg,
    gregexpr("(?<=<clipPath id=')[^']+", svg, perl = TRUE)
  )[[1]]
  clips <- lapply(clip_ids, function(id) {
    el <- regmatches(
      svg,
      regexpr(sprintf("<clipPath id='%s'>\\s*<rect[^>]+>", id), svg)
    )
    x <- attr_num(el, "x")
    y <- attr_num(el, "y")
    c(x, x + attr_num(el, "width"), y, y + attr_num(el, "height"))
  })
  names(clips) <- clip_ids

  groups <- regmatches(
    svg,
    gregexpr("<g clip-path='url\\(#[^)]+\\)'>.*?</g>", svg)
  )[[1]]
  boxes <- lapply(groups, function(g) {
    clip <- clips[[regmatches(g, regexpr("(?<=url\\(#)[^)]+", g, perl = TRUE))]]
    # Leading whitespace keeps `d=` from matching inside `id=`.
    shapes <- regmatches(g, gregexpr("\\s(points|d)='[^']+'", g))[[1]]
    rects <- regmatches(g, gregexpr("<rect[^>]+>", g))[[1]]
    shape_boxes <- lapply(shapes, function(a) {
      v <- num(a)
      c(range(v[c(TRUE, FALSE)]), range(v[c(FALSE, TRUE)]))
    })
    rect_boxes <- lapply(rects, function(r) {
      x <- attr_num(r, "x")
      y <- attr_num(r, "y")
      c(x, x + attr_num(r, "width"), y, y + attr_num(r, "height"))
    })
    lapply(c(shape_boxes, rect_boxes), function(b) {
      c(
        max(b[1], clip[1]),
        min(b[2], clip[2]),
        max(b[3], clip[3]),
        min(b[4], clip[4])
      )
    })
  })
  boxes <- do.call(rbind, unlist(boxes, recursive = FALSE))

  list(
    left = min(boxes[, 1]),
    right = max(boxes[, 2]),
    top = min(boxes[, 3]),
    bottom = max(boxes[, 4])
  )
}

# svglite writes coordinates with two decimals.
expect_pt_equal <- function(object, expected, tolerance = 0.02) {
  expect_lt(abs(object - expected), tolerance)
}

# Draw a grob on an svglite device and return the points of every drawn
# subpath (each polygon, polyline, or `M` segment of a path) as a two-column
# matrix in device points (svg y axis points down).
drawn_subpaths <- function(grob) {
  s <- svglite::svgstring(width = 4, height = 3, standalone = FALSE)
  grid::grid.newpage()
  grid::grid.draw(grob)
  grDevices::dev.off()
  svg <- as.character(s())

  to_matrix <- function(txt) {
    v <- as.numeric(regmatches(txt, gregexpr("-?[0-9]+(\\.[0-9]+)?", txt))[[1]])
    matrix(v, ncol = 2, byrow = TRUE)
  }
  points <- regmatches(
    svg,
    gregexpr("<(polygon|polyline) points='[^']+'", svg)
  )[[1]]
  paths <- regmatches(svg, gregexpr("<path d='[^']+'", svg))[[1]]
  subpaths <- unlist(lapply(paths, function(p) {
    strsplit(sub("^<path d='", "", p), "(?=M)", perl = TRUE)[[1]]
  }))
  subpaths <- subpaths[grepl("[0-9]", subpaths)]
  lapply(c(points, subpaths), to_matrix)
}

# Bounding box of the ink a grob draws at 600 dpi, in points (y down).
raster_ink <- function(grob) {
  file <- tempfile(fileext = ".png")
  ragg::agg_png(file, 4, 3, units = "in", res = 600)
  grid::grid.draw(grob)
  grDevices::dev.off()
  ink <- (1 - png::readPNG(file)[,, 1]) > 0.3
  rows <- which(rowSums(ink) > 0)
  cols <- which(colSums(ink) > 0)
  c(left = min(cols), right = max(cols), top = min(rows), bottom = max(rows)) *
    72 /
    600
}

# A positioned grob drawing Typst `code` with the default text settings.
typst_test_grob <- function(code, ...) {
  rendered <- typst_render(build_typst_source(code))
  positioned_typst_grob(rendered, ...)
}
