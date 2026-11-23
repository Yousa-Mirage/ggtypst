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

# Snapshot `plot` drawn on svglite with text drawn by `renderer`. vdiffr's own
# SVG device cannot draw native glyphs, clipping paths, or groups; svglite can,
# and writes each native glyph as a path from its font. Like vdiffr
# snapshots, these only run locally.
expect_svglite_snapshot <- function(
  name,
  plot,
  renderer = "glyph",
  width = 7,
  height = 5
) {
  skip_if_no_vdiffr()
  testthat::skip_if_not_installed("svglite")
  path <- tempfile(fileext = ".svg")
  svglite::svglite(path, width = width, height = height)
  tryCatch(
    with_renderer(renderer, print(plot)),
    finally = grDevices::dev.off()
  )
  testthat::expect_snapshot_file(
    path,
    paste0(name, ".svg"),
    compare = testthat::compare_file_text
  )
}

# Typst code for a small raster image: four coloured pixels in a 2 x 2 grid,
# as PNG bytes.
typst_png_bytes <- function() {
  pixels <- array(
    c(0.15, 0.62, 0.91, 0.16, 0.47, 0.44, 0.14, 0.80, 0.58, 0.69, 0.32, 0.20),
    dim = c(2, 2, 3)
  )
  paste0(
    "bytes((",
    paste(as.integer(png::writePNG(pixels)), collapse = ", "),
    "))"
  )
}

# One plot with every kind of content drawn as an image (gradients, tilings,
# images) and nested clip regions, each above a plain caption.
image_content_plot <- function() {
  svg <- paste0(
    "<svg xmlns='http://www.w3.org/2000/svg' width='12' height='8'>",
    "<rect width='6' height='8' fill='#2a9d8f'/>",
    "<circle cx='9' cy='4' r='3' fill='#e76f51'/></svg>"
  )
  cells <- c(
    "Linear gradient on text and math" = r"(#text(fill: gradient.linear(rgb("#1f77b4"), rgb("#d62728")))[*Gradient* $sum_(i=1)^n x_i^2$])",
    "Radial gradient with a focus" = r"(#rect(width: 60pt, height: 24pt, radius: 4pt, fill: gradient.radial(white, rgb("#1f77b4"), focal-center: (30%, 30%), focal-radius: 5%)))",
    "Conic gradient" = r"(#circle(radius: 14pt, fill: gradient.conic(..color.map.viridis)))",
    "Gradient stroke" = r"(#line(length: 60pt, stroke: (paint: gradient.linear(red, blue), thickness: 4pt, cap: "round")))",
    "Tiling" = r"(#rect(width: 60pt, height: 24pt, stroke: 0.5pt, fill: tiling(size: (6pt, 6pt))[#place(dx: 1pt, dy: 1pt, circle(radius: 1.5pt, fill: rgb("#264653")))]))",
    "SVG, smooth and pixelated images" = paste0(
      sprintf(r"(#box(image(bytes("%s"), format: "svg", width: 30pt)))", svg),
      " #h(6pt) ",
      sprintf(
        r"(#box(image(%s, format: "png", width: 20pt)))",
        typst_png_bytes()
      ),
      " #h(6pt) ",
      sprintf(
        r"(#box(image(%s, format: "png", width: 20pt, scaling: "pixelated")))",
        typst_png_bytes()
      )
    ),
    "Nested clip regions" = r"(#box(clip: true, width: 40pt, height: 24pt, radius: 12pt)[#box(clip: true, width: 60pt, height: 16pt)[#rect(width: 60pt, height: 30pt, fill: gradient.linear(..color.map.rainbow))]])",
    "Rotated gradient text" = r"(#rotate(-20deg)[#text(fill: gradient.linear(teal, purple))[*Rotated*]])"
  )
  cells_plot(cells)
}

# A plot of Typst `cells` in two columns, each above its name as a caption.
cells_plot <- function(cells) {
  x <- rep(c(1, 2), length.out = length(cells))
  y <- rev(rep(seq_len(length(cells) / 2), each = 2))
  p <- ggplot() +
    coord_cartesian(
      xlim = c(0.4, 2.6),
      ylim = c(0.4, length(cells) / 2 + 0.6)
    ) +
    theme_void()
  for (i in seq_along(cells)) {
    p <- p +
      annotate_typst(cells[[i]], x = x[i], y = y[i] + 0.12, size = 12) +
      annotate_typst(names(cells)[i], x = x[i], y = y[i] - 0.3, size = 8)
  }
  p
}

# One plot with solid fills and strokes, including page fills, drawn as paths
# (no images), on a grey background so that white fills show.
solid_content_plot <- function() {
  cells <- c(
    "Page fill" = r"(#set page(fill: rgb("#FAD7A0"))
Page fill)",
    "Page fill auto (white)" = r"(#set page(fill: auto)
Auto)",
    "Filled box and circle" = r"(#box(width: 30pt, height: 16pt, fill: rgb("#2a9d8f")) #h(6pt) #box(circle(radius: 8pt, fill: rgb("#e76f51"))))",
    "Solid and dashed borders" = r"(#box(width: 30pt, height: 16pt, stroke: 1.5pt + rgb("#264653")) #h(6pt) #box(width: 30pt, height: 16pt, stroke: (paint: rgb("#e63946"), thickness: 1.5pt, dash: "dashed")))",
    "Semi-transparent fills" = r"(#box(width: 24pt, height: 16pt, fill: rgb("#1E66F580")) #h(-10pt) #box(width: 24pt, height: 16pt, fill: rgb("#D20F3980")))",
    "Rounded box with fill and border" = r"(#box(fill: rgb("#FFF3B0"), stroke: 0.8pt + rgb("#E09F3E"), inset: 4pt, radius: 4pt)[Note $x^2$])"
  )
  cells_plot(cells) +
    theme(plot.background = element_rect(fill = "grey85", colour = NA))
}
