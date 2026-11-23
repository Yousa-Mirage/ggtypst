# Drawing a Typst scene (see src/rust/src/scene.rs) with grid: the grobs of
# its runs (native glyphs, glyph outlines, shapes, clip regions), and the font
# files native glyphs need. `scene_typst_grob()` and
# `makeContent.typst_scene_grob()` (grob.R) decide when and how it is drawn.
#
# The scene arrives ready to draw: its runs are in Typst's drawing order, each
# with the range of rows and points it draws; coordinates are points from the
# layout box's bottom-left corner; missing colours are `NA`; widths are `lwd`.
# Only `scale` is applied here. Glyph outlines are not part of the scene: they
# are computed by Rust only when text is drawn as paths (`glyph_outlines()`).

#' Grid grobs drawing a scene, in Typst's drawing order
#'
#' Each run becomes one grob: native glyphs or their outlines, depending on
#' `mode`, or a shape. Consecutive runs in a clip region are drawn in a
#' viewport clipped to it.
#'
#' @param scene The `scene` of a rendered Typst result.
#' @param mode `"glyph"` or `"path"`, see `typst_render_mode()`.
#' @param font_list The scene's fonts as a `glyphFontList`, for `"glyph"`.
#' @param outlines The scene's glyph outlines, for `"path"`, see
#'   `glyph_outlines()`.
#' @param groups Whether the device can draw grid groups (see
#'   `clipped_grob()`).
#' @param scale Scaling factor applied to the rendered output.
#' @param width,height Size of the layout box in points.
#' @return A `gList`.
#' @noRd
scene_grobs <- function(
  scene,
  mode,
  font_list = NULL,
  outlines = NULL,
  groups = TRUE,
  scale = 1,
  width,
  height
) {
  runs <- scene$runs
  grobs <- lapply(seq_along(runs$kind), function(i) {
    rows <- seq(runs$row[i], length.out = runs$n_rows[i])
    points <- seq(runs$point[i], length.out = runs$n_points[i])
    if (runs$only[i] == "path" && mode != "path") {
      # The image of a colour glyph, which native glyphs draw themselves.
      NULL
    } else if (runs$kind[i] == "raster") {
      raster_grob(scene$rasters, rows, scale)
    } else if (runs$kind[i] != "glyph") {
      shape_grob(scene, rows, points, runs$kind[i], scale)
    } else if (mode == "glyph") {
      glyph_run_grob(scene$glyphs, rows, font_list, scale, width, height)
    } else {
      # A run's glyphs have consecutive outlines.
      first <- outlines$point[rows[1]]
      points <- seq(first, length.out = sum(outlines$n_points[rows]))
      outline_run_grob(outlines, scene$glyphs, points, scale)
    }
  })

  segments <- split(seq_along(runs$kind), runs$segment)
  segments <- lapply(unname(segments), function(j) {
    children <- Filter(Negate(is.null), grobs[j])
    id <- runs$clip[j[1]]
    if (id == 0L || length(children) == 0L) {
      return(children)
    }
    list(clipped_grob(scene, id, do.call(grid::gList, children), scale, groups))
  })
  do.call(grid::gList, unlist(segments, recursive = FALSE))
}

# Image `i`: content grid cannot draw as glyphs or paths (gradients,
# tilings, images, colour glyphs drawn as paths), drawn by Typst in Rust.
raster_grob <- function(rasters, i, scale) {
  image <- structure(
    rasters$pixels[[i]],
    dim = c(rasters$height_px[i], rasters$width_px[i]),
    class = "nativeRaster",
    channels = 4L
  )
  grid::rasterGrob(
    image,
    x = rasters$x[i] * scale,
    y = rasters$y[i] * scale,
    width = rasters$width[i] * scale,
    height = rasters$height[i] * scale,
    just = c(0, 0),
    default.units = "bigpts",
    interpolate = rasters$interpolate[i]
  )
}

# `children` clipped by clip region `id` and every region around it.
#
# Clipping viewports inside each other does not intersect their clip regions:
# grid uses the innermost one. Drawing the clipped content as a group and
# clipping the group to the outer region does intersect them. On devices that
# cannot draw groups, only the innermost region clips.
clipped_grob <- function(scene, id, children, scale, groups = TRUE) {
  grob <- grid::gTree(children = children, vp = clip_viewport(scene, id, scale))
  outer <- if (groups) scene$clips$parent[id] else 0L
  while (outer != 0L) {
    grob <- grid::gTree(
      children = grid::gList(grid::groupGrob(grob)),
      vp = clip_viewport(scene, outer, scale)
    )
    outer <- scene$clips$parent[outer]
  }
  grob
}

# A viewport over the layout box, clipped to clip region `id`.
clip_viewport <- function(scene, id, scale) {
  clips <- scene$clips
  points <- seq(clips$point[id], length.out = clips$n_points[id])
  grid::viewport(
    clip = grid::pathGrob(
      x = scene$clip_points$x[points] * scale,
      y = scene$clip_points$y[points] * scale,
      id = scene$clip_points$sub[points],
      default.units = "bigpts"
    )
  )
}

# Native glyphs `rows`, positioned in the layout box.
#
# `glyphInfo()` can only rotate glyphs (its `rot` argument) in R 4.5 and later.
# On older R, if all glyphs of the run have the same angle (for example in a
# rotated axis title), we draw them without rotation inside a viewport that
# is rotated by that angle. Text with glyphs at different angles is drawn as
# outlines instead (see `typst_render_mode()`).
glyph_run_grob <- function(glyphs, rows, font_list, scale, width, height) {
  x <- glyphs$x[rows] * scale
  y <- glyphs$y[rows] * scale
  rot <- glyphs$rot[rows]
  turn <- if (glyph_rot_supported()) 0 else rot[1]
  if (turn != 0) {
    # Rotate the positions back by the angle, around the layout box's
    # bottom-left corner. The rotated viewport below then puts the glyphs
    # back in place.
    angle <- -turn * pi / 180
    turned_x <- x * cos(angle) - y * sin(angle)
    y <- x * sin(angle) + y * cos(angle)
    x <- turned_x
  }

  args <- list(
    id = glyphs$id[rows],
    x = x,
    y = y,
    font = glyphs$font[rows],
    size = glyphs$size[rows] * scale,
    fontList = font_list,
    width = width * scale,
    height = height * scale,
    hAnchor = grDevices::glyphAnchor(0, "left"),
    vAnchor = grDevices::glyphAnchor(0, "bottom"),
    col = glyphs$fill[rows]
  )

  if (glyph_rot_supported()) {
    args$rot <- rot
  }

  grob <- grid::glyphGrob(
    do.call(grDevices::glyphInfo, args),
    x = 0,
    y = 0,
    hjust = 0,
    vjust = 0,
    default.units = "npc"
  )

  if (turn == 0) {
    return(grob)
  }

  grid::gTree(
    children = grid::gList(grob),
    vp = grid::viewport(
      x = 0,
      y = 0,
      width = grid::unit(width * scale, "bigpts"),
      height = grid::unit(height * scale, "bigpts"),
      just = c(0, 0),
      angle = turn
    )
  )
}

# Whether `glyphInfo()` has the `rot` argument (R 4.5 and later).
glyph_rot_supported <- function() {
  "rot" %in% names(formals(grDevices::glyphInfo))
}

# Whether all glyphs in each glyph run have the same angle. If so, native
# glyphs can draw them on R older than 4.5 (see `glyph_run_grob()`).
glyph_runs_share_angle <- function(scene) {
  runs <- scene$runs
  for (i in which(runs$kind == "glyph")) {
    rot <- scene$glyphs$rot[seq(runs$row[i], length.out = runs$n_rows[i])]
    if (any(abs(rot - rot[1]) > 1e-9)) {
      return(FALSE)
    }
  }
  TRUE
}

# Font lists already built in this session, by their fonts, so that labels
# using the same fonts share one.
font_lists <- new.env(parent = emptyenv())

glyph_font_list <- function(fonts, files = typst_font_files(fonts)) {
  key <- paste(
    files,
    fonts$collection_index,
    fonts$family,
    fonts$weight,
    fonts$style,
    sep = "\t",
    collapse = "\n"
  )
  font_list <- font_lists[[key]]
  if (is.null(font_list)) {
    font_list <- do.call(
      grDevices::glyphFontList,
      lapply(seq_along(files), function(i) {
        grDevices::glyphFont(
          files[i],
          fonts$collection_index[i],
          fonts$family[i],
          fonts$weight[i],
          fonts$style[i]
        )
      })
    )
    assign(key, font_list, envir = font_lists)
  }
  font_list
}

#' Outlines of a scene's glyphs
#'
#' Computed by Rust from the scene's glyph table and fonts, for drawing text as
#' paths. Fonts are found again by file or, for fonts built into ggtypst, by
#' their cache key, so this also works for a scene saved in another session.
#'
#' @param scene The `scene` of a rendered Typst result.
#' @return A list with the points `x`, `y`, `sub` (contour), and `owner` (glyph
#'   row), and each glyph's first `point` and `n_points`.
#' @noRd
glyph_outlines <- function(scene) {
  result <- rs_glyph_outlines(scene$fonts, scene$glyphs)
  if (inherits(result, "typst_error")) {
    abort_typst_errors(result)
  }
  result
}

# Outline points `points` of some glyphs as one path grob: one path per glyph
# (`pathId`), made of its contours (`id`).
outline_run_grob <- function(outlines, glyphs, points, scale) {
  if (length(points) == 0L) {
    return(NULL)
  }
  owners <- unique(outlines$owner[points])
  grid::pathGrob(
    x = outlines$x[points] * scale,
    y = outlines$y[points] * scale,
    id = outlines$sub[points],
    pathId = outlines$owner[points],
    rule = "winding",
    default.units = "bigpts",
    gp = grid::gpar(
      fill = glyphs$fill[owners],
      col = glyphs$stroke[owners],
      lwd = glyphs$lwd[owners] * scale
    )
  )
}

# Shape `row`, made of shape points `points`, as a `"path"` or `"polyline"`.
shape_grob <- function(scene, row, points, kind, scale) {
  shapes <- scene$shapes
  x <- scene$shape_points$x[points] * scale
  y <- scene$shape_points$y[points] * scale
  id <- scene$shape_points$sub[points]
  gp <- grid::gpar(
    fill = shapes$fill[row],
    col = shapes$stroke[row],
    lwd = shapes$lwd[row] * scale,
    lineend = shapes$lineend[row],
    linejoin = shapes$linejoin[row]
  )
  if (kind == "path") {
    grid::pathGrob(
      x,
      y,
      id = id,
      rule = shapes$rule[row],
      default.units = "bigpts",
      gp = gp
    )
  } else {
    grid::polylineGrob(x, y, id = id, default.units = "bigpts", gp = gp)
  }
}

#' Files of the fonts a scene's glyphs use
#'
#' System fonts are used where they are. Fonts embedded in ggtypst (Typst's
#' default fonts) are written once per session to
#' `getOption("ggtypst.font_cache")`, a directory under [tempdir()] by
#' default, so graphics devices can load them.
#'
#' @param fonts The `fonts` table of a scene.
#' @return A character vector of font file paths.
#' @noRd
typst_font_files <- function(fonts) {
  files <- fonts$path
  embedded <- which(!nzchar(files))
  if (length(embedded) == 0L) {
    return(files)
  }

  dir <- font_cache_dir()
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  for (i in embedded) {
    file <- file.path(dir, paste0(fonts$cache_key[i], ".otf"))
    if (!file.exists(file)) {
      written <- rs_typst_write_font(
        fonts$store_index[i],
        fonts$collection_index[i],
        fonts$cache_key[i],
        file
      )
      if (inherits(written, "typst_error")) {
        abort_typst_errors(written)
      }
    }
    files[i] <- file
  }
  files
}

font_cache_dir <- function() {
  getOption("ggtypst.font_cache", file.path(tempdir(), "ggtypst-fonts"))
}
