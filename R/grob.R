#' Build a positioned Typst grob from rendered output
#'
#' Wraps `scene_typst_grob()` in a grob that reports the layout size through
#' `widthDetails()` and `heightDetails()`, so ggplot2 reserves the right space.
#'
#' @inheritParams scene_typst_grob
#' @param class Optional extra class name prepended before `"typst_grob"`.
#' @return A positioned measured Typst grob.
#' @noRd
positioned_typst_grob <- function(
  rendered,
  x = 0.5,
  y = 0.5,
  default.units = "npc",
  scale = 1,
  hjust = 0.5,
  vjust = 0.5,
  angle = NULL,
  class = NULL
) {
  width_pt <- rendered$width_pt * scale
  height_pt <- rendered$height_pt * scale

  grob <- scene_typst_grob(
    rendered = rendered,
    x = x,
    y = y,
    default.units = default.units,
    scale = scale,
    hjust = hjust,
    vjust = vjust,
    angle = angle
  )

  out <- grid::gTree(
    children = grid::gList(grob),
    typst_width = grid::unit(width_pt, "bigpts"),
    typst_height = grid::unit(height_pt, "bigpts")
  )

  class(out) <- unique(c(class, "typst_grob", class(out)))
  out
}

#' Draw a Typst scene with grid
#'
#' The Typst page has no margin, so it is exactly the content's layout box
#' (`width_pt` x `height_pt`), and justification is relative to it, like grid
#' text. The layout box becomes a viewport. Scene coordinates (Typst points, y
#' pointing down from the box's top-left corner) are drawn in `"bigpts"` from
#' its bottom-left corner, which also works for boxes of zero width or height
#' (a bare `#line()`). Ink outside the box (overhangs, descenders) is drawn,
#' not clipped.
#'
#' The grob's content is built when it is drawn (see
#' `makeContent.typst_scene_grob()`), so it can draw text as native glyphs or
#' as outlines to suit the device, and fall back to importing the SVG for
#' content neither can express.
#'
#' @param rendered A rendered Typst result from `typst_render()`.
#' @param x,y Position of the anchor.
#' @param default.units Grid units for numeric `x` and `y`.
#' @param scale Scaling factor applied to rendered dimensions.
#' @param hjust,vjust Horizontal and vertical justification values.
#' @param angle Rotation angle in ggplot2/grid semantics.
#' @return A `gTree` of class `"typst_scene_grob"`.
#' @noRd
scene_typst_grob <- function(
  rendered,
  x = 0.5,
  y = 0.5,
  default.units = "npc",
  scale = 1,
  hjust,
  vjust,
  angle = NULL
) {
  just <- rotate_just(angle = angle, hjust = hjust, vjust = vjust)
  width <- rendered$width_pt
  height <- rendered$height_pt

  if (!grid::is.unit(x)) {
    x <- grid::unit(x, default.units)
  }
  if (!grid::is.unit(y)) {
    y <- grid::unit(y, default.units)
  }

  vp <- grid::viewport(
    x = x,
    y = y,
    width = grid::unit(width * scale, "bigpts"),
    height = grid::unit(height * scale, "bigpts"),
    just = c(just$hjust, just$vjust)
  )

  grid::gTree(
    scene = rendered$scene,
    plan = scene_plan(rendered$scene),
    fallback = rendered$fallback,
    scale = scale,
    width_pt = width,
    height_pt = height,
    # Children built for each renderer, reused when the grob is drawn again
    # (window resizes, display list replays, drawing on several devices).
    cache = new.env(parent = emptyenv()),
    vp = vp,
    cl = "typst_scene_grob"
  )
}

#' @noRd
#' @importFrom grid makeContent
#' @export
makeContent.typst_scene_grob <- function(x) {
  mode <- typst_render_mode(x$scene)
  entry <- x$cache[[mode]]
  if (!scene_children_valid(entry, x$scale)) {
    entry <- scene_children(x, mode)
    assign(mode, entry, envir = x$cache)
  }
  grid::setChildren(x, entry$children)
}

# The children drawing scene grob `x` in `mode`, with what decides whether they
# can be reused.
scene_children <- function(x, mode) {
  entry <- list(scale = x$scale, font_dir = NULL, font_files = character())

  # Content grid paths cannot draw yet (gradients, images, colour glyphs):
  # import the SVG, placed on the layout box this viewport spans. Without the
  # optional packages it needs, warn once and draw what grid paths can.
  if (mode == "path" && !is.null(x$fallback)) {
    if (svg_fallback_installed()) {
      svg <- svg_typst_grob(x$fallback, x$height_pt, x$scale)
      entry$children <- grid::gList(svg)
      return(entry)
    }
    warn_svg_fallback_missing()
  }

  font_list <- NULL
  if (mode == "glyph" && length(x$scene$glyphs$id) > 0L) {
    entry$font_dir <- font_cache_dir()
    entry$font_files <- typst_font_files(x$scene$fonts)
    font_list <- glyph_font_list(x$scene$fonts, entry$font_files)
  }
  entry$children <- scene_grobs(
    x$scene,
    x$plan,
    mode,
    font_list,
    x$scale,
    x$width_pt,
    x$height_pt
  )
  entry
}

# Native glyphs refer to font files, so they are only reused while those files
# are still there: a grob read back in a new session, a changed
# `ggtypst.font_cache`, or a deleted font file rebuilds them.
scene_children_valid <- function(entry, scale) {
  if (is.null(entry) || !identical(entry$scale, scale)) {
    return(FALSE)
  }
  is.null(entry$font_dir) ||
    (identical(entry$font_dir, font_cache_dir()) &&
      all(file.exists(entry$font_files)))
}

# The SVG fallback needs rsvg and grImport2, which are optional.
svg_fallback_installed <- function() {
  requireNamespace("rsvg", quietly = TRUE) &&
    requireNamespace("grImport2", quietly = TRUE)
}

warn_svg_fallback_missing <- function() {
  cli::cli_warn(
    c(
      "Some Typst content (gradients, images, or colour glyphs) cannot be drawn as outlines.",
      "i" = "Install {.pkg rsvg} and {.pkg grImport2} to draw it, or draw text as native glyphs (see {.topic ggtypst::ggtypst-options})."
    ),
    .frequency = "once",
    .frequency_id = "ggtypst_svg_fallback"
  )
}

#' Choose how to draw a scene's text: `"glyph"` or `"path"`
#'
#' Follows `getOption("ggtypst.renderer")` (see [ggtypst-options]). With
#' `"auto"`, text is drawn as native glyphs on devices that rasterize or embed
#' fonts, and as outlines on devices that only reference fonts by name (such as
#' [grDevices::pdf()]), where glyphs could be shown in the wrong font. Scenes
#' that glyphs cannot express (stroked or skewed text, ...) are always drawn as
#' outlines.
#'
#' Called when the grob is drawn, so the device in use decides.
#'
#' @param scene The `scene` of a rendered Typst result.
#' @return `"glyph"` or `"path"`.
#' @noRd
typst_render_mode <- function(scene) {
  renderer <- getOption("ggtypst.renderer", "auto")
  if (
    !is.character(renderer) ||
      length(renderer) != 1L ||
      !renderer %in% c("auto", "glyph", "path")
  ) {
    cli::cli_abort(c(
      "Option {.code ggtypst.renderer} must be {.val auto}, {.val glyph}, or {.val path}.",
      "x" = "It is {.val {renderer}}."
    ))
  }
  if (renderer == "path" || !isTRUE(scene$glyph_ok)) {
    return("path")
  }
  if (renderer == "glyph" || device_draws_glyphs()) "glyph" else "path"
}

# Devices that rasterize glyphs or embed their fonts, so native glyphs look
# right wherever the output is viewed. Others (pdf(), postscript(), unknown
# devices) get outlines.
glyph_devices <- c(
  "png",
  "jpeg",
  "tiff",
  "bmp",
  "quartz",
  "quartz_off_screen",
  "X11cairo",
  "cairo_pdf",
  "cairo_ps",
  "svg",
  "devSVG"
)

device_draws_glyphs <- function() {
  name <- names(grDevices::dev.cur())
  known <- startsWith(name, "agg_") || name %in% glyph_devices
  known && isTRUE(grDevices::dev.capabilities("glyphs")$glyphs)
}

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

#' Import Typst SVG output as a picture grob
#'
#' Fallback for pages the scene cannot draw with paths. Imports the SVG through
#' `grImport2` and returns a `pictureGrob` drawn at exactly 1:1 (times `scale`)
#' with Typst's point coordinates, with the layout box's bottom-left corner at
#' the bottom-left corner of the current viewport.
#'
#' The SVG canvas covers the layout box and any ink drawn outside it (italic
#' overhangs, descender tails, moved or rotated content), rounded up to whole
#' points; the layout box's top-left corner sits at (`offset_x_pt`,
#' `offset_y_pt`) on it.
#'
#' @param fallback The `fallback` of a rendered Typst result.
#' @param height Height of the layout box in points.
#' @param scale Scaling factor applied to the rendered output.
#' @return A `grImport2` picture grob of class `"typst_svg_fallback"`.
#' @noRd
svg_typst_grob <- function(fallback, height, scale = 1) {
  # grImport2 only reads Cairo-generated SVG, so re-render it with rsvg. Pass
  # the whole-point canvas size explicitly so rsvg neither truncates nor
  # rescales the drawing. It is oversampled by an integer factor (so the size
  # stays exact) because grImport2 flattens curves with a fixed number of
  # segments per canvas unit: at 1:1 glyph outlines look faceted when zoomed.
  canonical_svg <- rsvg::rsvg_svg(
    fallback$svg,
    width = fallback$width_pt * 8L,
    height = fallback$height_pt * 8L
  )

  # readPicture() hands its argument to XML::xmlParse(), which parses a string
  # starting with "<" as XML content.
  picture <- grImport2::readPicture(rawToChar(canonical_svg), warn = FALSE)

  grob <- grImport2::pictureGrob(
    picture,
    x = 0,
    y = 0,
    width = grid::unit(fallback$width_pt * scale, "bigpts"),
    height = grid::unit(fallback$height_pt * scale, "bigpts"),
    # Anchor the canvas at the layout box's bottom-left corner, given as a
    # fraction of the canvas (grid's vjust counts from the bottom).
    hjust = fallback$offset_x_pt / fallback$width_pt,
    vjust = (fallback$height_pt - fallback$offset_y_pt - height) /
      fallback$height_pt,
    # pictureGrob() pads the picture by 5% per side by default, which would
    # draw Typst output at 1 / 1.1 of its size.
    expansion = 0,
    ext = "clipbbox"
  )
  class(grob) <- c("typst_svg_fallback", class(grob))
  grob
}

#' @noRd
#' @importFrom grid widthDetails
#' @export
widthDetails.typst_grob <- function(x) {
  x$typst_width
}

#' @noRd
#' @importFrom grid heightDetails
#' @export
heightDetails.typst_grob <- function(x) {
  x$typst_height
}
