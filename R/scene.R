# Drawing a Typst scene (see src/rust/src/scene.rs) with grid: its drawing
# plan, the grobs of its runs (native glyphs, glyph outlines, shapes, clip
# regions), and the font files native glyphs need. `scene_typst_grob()` and
# `makeContent.typst_scene_grob()` (grob.R) decide when and how it is drawn.

#' Drawing plan of a scene, the same however its text is drawn
#'
#' Glyphs and shapes are put in Typst's drawing order and cut into runs, each
#' drawn by one grob: every shape on its own, consecutive glyphs in the same
#' clip region together. Consecutive runs in the same clip region form a
#' segment, drawn in a viewport clipped to it. The point tables are indexed by
#' owner, so a run takes its points without scanning them.
#'
#' @param scene The `scene` of a rendered Typst result.
#' @return A list with `runs` (each a list of `kind`, `rows`, and `clip`),
#'   `segments` (the runs of each segment), and `outlines`, `shape_points`, and
#'   `clips` (the point indices of each owner).
#' @noRd
scene_plan <- function(scene) {
  glyphs <- scene$glyphs
  shapes <- scene$shapes
  kind <- rep(c("glyph", "shape"), c(length(glyphs$z), length(shapes$z)))
  row <- c(seq_along(glyphs$z), seq_along(shapes$z))
  order <- order(c(glyphs$z, shapes$z))
  kind <- kind[order]
  row <- row[order]
  clip <- c(glyphs$clip, shapes$clip)[order]

  runs <- list()
  segments <- list()
  if (length(kind) > 0L) {
    # A new run starts at every shape, at every glyph that follows a shape,
    # and wherever the clip region changes.
    changed <- c(
      TRUE,
      kind[-1] != kind[-length(kind)] | clip[-1] != clip[-length(clip)]
    )
    run <- cumsum(kind == "shape" | changed)
    runs <- lapply(unname(split(seq_along(kind), run)), function(i) {
      list(kind = kind[i[1]], rows = row[i], clip = clip[i[1]])
    })
    run_clip <- vapply(runs, function(r) r$clip, integer(1))
    segment <- cumsum(c(TRUE, run_clip[-1] != run_clip[-length(run_clip)]))
    segments <- unname(split(seq_along(runs), segment))
  }

  list(
    runs = runs,
    segments = segments,
    outlines = index_by_owner(scene$outlines$owner, length(glyphs$z)),
    shape_points = index_by_owner(scene$shape_points$owner, length(shapes$z)),
    clips = index_by_owner(scene$clips$owner, max(c(0L, scene$clips$owner)))
  )
}

# Indices of the points of each owner 1, ..., n.
index_by_owner <- function(owner, n) {
  unname(split(seq_along(owner), factor(owner, levels = seq_len(n))))
}

#' Grid grobs drawing a scene, in Typst's drawing order
#'
#' Each run of the plan becomes one grob: native glyphs or their outlines,
#' depending on `mode`, or a shape.
#'
#' @param scene The `scene` of a rendered Typst result.
#' @param plan Its drawing plan, see `scene_plan()`.
#' @param mode `"glyph"` or `"path"`, see `typst_render_mode()`.
#' @param font_list The scene's fonts as a `glyphFontList`, for `"glyph"`.
#' @param scale Scaling factor applied to the rendered output.
#' @param width,height Size of the layout box in points.
#' @return A `gList`.
#' @noRd
scene_grobs <- function(
  scene,
  plan,
  mode,
  font_list = NULL,
  scale = 1,
  width,
  height
) {
  grobs <- lapply(plan$runs, function(run) {
    if (run$kind == "shape") {
      shape_grob(scene, run$rows, plan$shape_points[[run$rows]], scale, height)
    } else if (mode == "glyph") {
      glyph_run_grob(scene$glyphs, run$rows, font_list, scale, width, height)
    } else {
      points <- unlist(plan$outlines[run$rows], use.names = FALSE)
      outline_run_grob(scene, points, scale, height)
    }
  })

  # Wrap the runs of each segment in a clip region in a viewport clipped to it.
  segments <- lapply(plan$segments, function(j) {
    children <- Filter(Negate(is.null), grobs[j])
    id <- plan$runs[[j[1]]]$clip
    if (id == 0L || length(children) == 0L) {
      return(children)
    }
    list(grid::gTree(
      children = do.call(grid::gList, children),
      vp = clip_viewport(scene$clips, plan$clips[[id]], scale, height)
    ))
  })
  do.call(grid::gList, unlist(segments, recursive = FALSE))
}

# A viewport over the layout box, clipped to the clip region made of `points`.
clip_viewport <- function(clips, points, scale, height) {
  grid::viewport(
    clip = grid::pathGrob(
      x = scene_x(clips$x[points], scale),
      y = scene_y(clips$y[points], scale, height),
      id = clips$sub[points],
      default.units = "bigpts"
    )
  )
}

# Scene coordinates (points, y down from the layout box's top-left corner) as
# distances in points from the box's bottom-left corner.
scene_x <- function(x, scale) {
  x * scale
}

scene_y <- function(y, scale, height) {
  (height - y) * scale
}

# Native glyphs for some glyph rows. Positions are in points from the layout
# box's bottom-left corner.
glyph_run_grob <- function(glyphs, rows, font_list, scale, width, height) {
  info <- grDevices::glyphInfo(
    id = glyphs$id[rows],
    x = scene_x(glyphs$x[rows], scale),
    y = scene_y(glyphs$y[rows], scale, height),
    font = glyphs$font[rows],
    size = glyphs$size[rows] * scale,
    fontList = font_list,
    width = width * scale,
    height = height * scale,
    hAnchor = grDevices::glyphAnchor(0, "left"),
    vAnchor = grDevices::glyphAnchor(0, "bottom"),
    col = glyphs$fill[rows],
    rot = glyphs$rot[rows]
  )
  grid::glyphGrob(
    info,
    x = 0,
    y = 0,
    hjust = 0,
    vjust = 0,
    default.units = "npc"
  )
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

# Outline points `keep` of some glyphs as one path grob: one path per glyph
# (`pathId`), made of its contours (`id`).
outline_run_grob <- function(scene, keep, scale = 1, height) {
  outlines <- scene$outlines
  if (length(keep) == 0L) {
    return(NULL)
  }
  glyphs <- scene$glyphs
  owners <- unique(outlines$owner[keep])
  grid::pathGrob(
    x = scene_x(outlines$x[keep], scale),
    y = scene_y(outlines$y[keep], scale, height),
    id = outlines$sub[keep],
    pathId = outlines$owner[keep],
    rule = "winding",
    default.units = "bigpts",
    gp = grid::gpar(
      fill = glyphs$fill[owners],
      col = na_if_empty(glyphs$stroke[owners]),
      lwd = pt_to_lwd(glyphs$stroke_width[owners] * scale)
    )
  )
}

# Shape `i`, made of shape points `keep`: a path if it is closed or filled,
# otherwise a polyline.
shape_grob <- function(scene, i, keep, scale = 1, height) {
  shapes <- scene$shapes
  points <- scene$shape_points
  if (length(keep) == 0L) {
    return(NULL)
  }
  gp <- grid::gpar(
    fill = na_if_empty(shapes$fill[i]),
    col = na_if_empty(shapes$stroke[i]),
    lwd = pt_to_lwd(shapes$stroke_width[i] * scale),
    lineend = shapes$lineend[i],
    linejoin = shapes$linejoin[i]
  )
  if (shapes$closed[i] == 1L || nzchar(shapes$fill[i])) {
    grid::pathGrob(
      x = scene_x(points$x[keep], scale),
      y = scene_y(points$y[keep], scale, height),
      id = points$sub[keep],
      rule = shapes$rule[i],
      default.units = "bigpts",
      gp = gp
    )
  } else {
    grid::polylineGrob(
      x = scene_x(points$x[keep], scale),
      y = scene_y(points$y[keep], scale, height),
      id = points$sub[keep],
      default.units = "bigpts",
      gp = gp
    )
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
      # Write to a temporary name first so a concurrent reader never sees a
      # partial file.
      partial <- tempfile(tmpdir = dir)
      writeBin(rs_typst_font_data(fonts$store_index[i]), partial)
      file.rename(partial, file)
    }
    files[i] <- file
  }
  files
}

font_cache_dir <- function() {
  getOption("ggtypst.font_cache", file.path(tempdir(), "ggtypst-fonts"))
}

# Line width in grid's units (1/96 in) from Typst points (1/72 in).
pt_to_lwd <- function(pt) {
  pt * 96 / 72
}

na_if_empty <- function(x) {
  x[!nzchar(x)] <- NA_character_
  x
}
