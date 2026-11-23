#' Build a positioned Typst grob from rendered output
#'
#' @param rendered A rendered Typst result from `typst_svg()`.
#' @param x,y Position of the anchor.
#' @param default.units Grid units for numeric `x` and `y`.
#' @param scale Scaling factor applied to rendered dimensions.
#' @param hjust,vjust Horizontal and vertical justification values.
#' @param angle Rotation angle in ggplot2/grid semantics.
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

  grob <- tryCatch(
    vector_typst_grob(
      rendered = rendered,
      x = x,
      y = y,
      default.units = default.units,
      scale = scale,
      hjust = hjust,
      vjust = vjust,
      angle = angle
    ),
    error = function(cnd) {
      cli::cli_abort(c(
        "Failed to import Typst output as a vector graphic.",
        "i" = conditionMessage(cnd)
      ))
    }
  )

  out <- grid::gTree(
    children = grid::gList(grob),
    typst_width = grid::unit(width_pt, "bigpts"),
    typst_height = grid::unit(height_pt, "bigpts")
  )

  class(out) <- unique(c(class, "typst_grob", class(out)))
  out
}

#' Draw rendered Typst output as a grob
#'
#' The Typst page has no margin, so it is exactly the content's layout box
#' (`width_pt` x `height_pt`), and justification is relative to it, like grid
#' text. The page is drawn from its scene with grid (see
#' `scene_typst_grob()`).
#'
#' @param rendered A rendered Typst result from `typst_svg()`.
#' @param x,y Position of the anchor.
#' @param default.units Grid units for numeric `x` and `y`.
#' @param scale Scaling factor applied to rendered dimensions.
#' @param hjust,vjust Horizontal and vertical justification values.
#' @param angle Rotation angle in ggplot2/grid semantics.
#' @return A grob.
#' @noRd
vector_typst_grob <- function(
  rendered,
  x = 0.5,
  y = 0.5,
  default.units = "npc",
  scale = 1,
  hjust,
  vjust,
  angle = NULL
) {
  scene_typst_grob(
    rendered = rendered,
    x = x,
    y = y,
    default.units = default.units,
    scale = scale,
    hjust = hjust,
    vjust = vjust,
    angle = angle
  )
}

#' Draw a Typst scene with grid
#'
#' The layout box becomes a viewport. Scene coordinates (Typst points, y
#' pointing down from the box's top-left corner) are drawn in `"bigpts"` from
#' its bottom-left corner, which also works for boxes of zero width or height
#' (a bare `#line()`). Ink outside the box (overhangs, descenders) is drawn,
#' not clipped. The grob's
#' content is built when it is drawn (see `makeContent.typst_scene_grob()`),
#' so it can draw text as native glyphs or as outlines to suit the device, and
#' fall back to importing the SVG for content neither can express.
#'
#' @inheritParams vector_typst_grob
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
    rendered = rendered,
    scale = scale,
    width_pt = width,
    height_pt = height,
    vp = vp,
    cl = "typst_scene_grob"
  )
}

#' @noRd
#' @importFrom grid makeContent
#' @export
makeContent.typst_scene_grob <- function(x) {
  mode <- typst_render_mode(x$scene)
  if (mode == "path" && !isTRUE(x$scene$path_ok) && svg_fallback_available()) {
    # Content grid paths cannot draw yet (gradients, images, colour glyphs):
    # import the SVG, placed on the layout box this viewport spans.
    svg <- svg_typst_grob(x$rendered, x = 0, y = 0, scale = x$scale, hjust = 0, vjust = 0)
    class(svg) <- c("typst_svg_fallback", class(svg))
    return(grid::setChildren(x, grid::gList(svg)))
  }
  grid::setChildren(x, scene_grobs(x$scene, mode, x$scale, x$width_pt, x$height_pt))
}

# The SVG fallback needs rsvg and grImport2, which are optional. Without them,
# warn once and draw what grid paths can.
svg_fallback_available <- function() {
  available <- requireNamespace("rsvg", quietly = TRUE) && requireNamespace("grImport2", quietly = TRUE)
  if (!available) {
    cli::cli_warn(
      c(
        "Some Typst content (gradients, images, or colour glyphs) cannot be drawn as outlines.",
        "i" = "Install {.pkg rsvg} and {.pkg grImport2} to draw it, or draw text as native glyphs (see {.topic ggtypst::ggtypst-options})."
      ),
      .frequency = "once",
      .frequency_id = "ggtypst_svg_fallback"
    )
  }
  available
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
  if (!is.character(renderer) || length(renderer) != 1L || !renderer %in% c("auto", "glyph", "path")) {
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

#' Grid grobs drawing a scene, in Typst's drawing order
#'
#' Consecutive glyphs are drawn by one grob: native glyphs or their outlines,
#' depending on `mode`. Each shape is drawn by its own grob.
#'
#' @param scene The `scene` of a rendered Typst result.
#' @param mode `"glyph"` or `"path"`, see `typst_render_mode()`.
#' @param scale Scaling factor applied to the rendered output.
#' @param width,height Size of the layout box in points.
#' @return A `gList`.
#' @noRd
scene_grobs <- function(scene, mode, scale = 1, width, height) {
  glyphs <- scene$glyphs
  shapes <- scene$shapes
  kind <- rep(c("glyph", "shape"), c(length(glyphs$z), length(shapes$z)))
  row <- c(seq_along(glyphs$z), seq_along(shapes$z))
  order <- order(c(glyphs$z, shapes$z))
  kind <- kind[order]
  row <- row[order]
  if (length(kind) == 0L) {
    return(grid::gList())
  }

  clip <- c(glyphs$clip, shapes$clip)[order]

  # A new run starts at every shape, at every glyph that follows a shape, and
  # wherever the clip region changes.
  changed <- c(TRUE, kind[-1] != kind[-length(kind)] | clip[-1] != clip[-length(clip)])
  run <- cumsum(kind == "shape" | changed)
  font_list <- if (mode == "glyph" && "glyph" %in% kind) glyph_font_list(scene$fonts)

  runs <- split(seq_along(kind), run)
  grobs <- lapply(runs, function(i) {
    if (kind[i[1]] == "shape") {
      shape_grob(scene, row[i], scale, height)
    } else if (mode == "glyph") {
      glyph_run_grob(glyphs, row[i], font_list, scale, width, height)
    } else {
      outline_run_grob(scene, row[i], scale, height)
    }
  })

  # Wrap consecutive runs in the same clip region in a viewport clipped to it.
  run_clip <- vapply(runs, function(i) clip[i[1]], integer(1))
  segment <- cumsum(c(TRUE, run_clip[-1] != run_clip[-length(run_clip)]))
  grobs <- lapply(split(seq_along(runs), segment), function(j) {
    children <- Filter(Negate(is.null), grobs[j])
    id <- run_clip[j[1]]
    if (id == 0L || length(children) == 0L) {
      return(children)
    }
    list(grid::gTree(
      children = do.call(grid::gList, children),
      vp = clip_viewport(scene$clips, id, scale, height)
    ))
  })
  do.call(grid::gList, unlist(grobs, recursive = FALSE))
}

# A viewport over the layout box, clipped to clip region `id`.
clip_viewport <- function(clips, id, scale, height) {
  keep <- clips$owner == id
  grid::viewport(
    clip = grid::pathGrob(
      x = scene_x(clips$x[keep], scale),
      y = scene_y(clips$y[keep], scale, height),
      id = clips$sub[keep],
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
  grid::glyphGrob(info, x = 0, y = 0, hjust = 0, vjust = 0, default.units = "npc")
}

glyph_font_list <- function(fonts) {
  files <- typst_font_files(fonts)
  do.call(
    grDevices::glyphFontList,
    lapply(seq_along(files), function(i) {
      grDevices::glyphFont(files[i], fonts$collection_index[i], fonts$family[i], fonts$weight[i], fonts$style[i])
    })
  )
}

# Outlines of some glyph rows as one path grob: one path per glyph (`pathId`),
# made of its contours (`id`).
outline_run_grob <- function(scene, rows, scale = 1, height) {
  outlines <- scene$outlines
  keep <- outlines$owner %in% rows
  if (!any(keep)) {
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

# One shape: a path if it is closed or filled, otherwise a polyline.
shape_grob <- function(scene, i, scale = 1, height) {
  shapes <- scene$shapes
  points <- scene$shape_points
  keep <- points$owner == i
  if (!any(keep)) {
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

  dir <- getOption("ggtypst.font_cache", file.path(tempdir(), "ggtypst-fonts"))
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
#' Fallback for pages the scene cannot describe (see `vector_typst_grob()`).
#' Imports the rendered SVG through `grImport2` and returns a `pictureGrob`
#' drawn at exactly 1:1 (times `scale`) with Typst's point coordinates.
#'
#' The Typst page has no margin, so it is exactly the content's layout box
#' (`width_pt` x `height_pt`). The Rust renderer extends the SVG canvas to
#' cover any ink drawn outside it (italic overhangs, descender tails, moved or
#' rotated content) and rounds the canvas up to whole points; the layout box
#' sits at (`offset_x_pt`, `offset_y_pt`) on it. Justification is given
#' relative to the layout box, like grid text, and is translated here to the
#' canvas.
#'
#' @param rendered A rendered Typst result from `typst_svg()`.
#' @param x,y Position passed to [grImport2::pictureGrob()].
#' @param default.units Grid units for numeric `x` and `y`.
#' @param scale Scaling factor applied to rendered dimensions.
#' @param hjust,vjust Horizontal and vertical justification values.
#' @param angle Rotation angle in ggplot2/grid semantics.
#' @return A `grImport2` picture grob.
#' @noRd
svg_typst_grob <- function(
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
  canvas_width <- rendered$canvas_width_pt
  canvas_height <- rendered$canvas_height_pt
  offset_x <- rendered$offset_x_pt
  offset_y <- rendered$offset_y_pt

  # The layout box sits on the canvas with its top-left corner at
  # (offset_x, offset_y), measured from the canvas's top-left. Express the
  # anchor (hjust, vjust) of the layout box as a fraction of the canvas; grid's
  # vjust counts from the bottom.
  canvas_hjust <- (offset_x + just$hjust * width) / canvas_width
  canvas_vjust <- (canvas_height - offset_y - height + just$vjust * height) / canvas_height

  # grImport2 only reads Cairo-generated SVG, so re-render it with rsvg. Pass
  # the whole-point canvas size explicitly so rsvg neither truncates nor
  # rescales the drawing. It is oversampled by an integer factor (so the size
  # stays exact) because grImport2 flattens curves with a fixed number of
  # segments per canvas unit: at 1:1 glyph outlines look faceted when zoomed.
  canonical_svg <- rsvg::rsvg_svg(
    rendered$svg,
    width = canvas_width * 8L,
    height = canvas_height * 8L
  )

  # readPicture() hands its argument to XML::xmlParse(), which parses a string
  # starting with "<" as XML content.
  picture <- grImport2::readPicture(rawToChar(canonical_svg), warn = FALSE)

  grImport2::pictureGrob(
    picture,
    x = x,
    y = y,
    width = grid::unit(canvas_width * scale, "bigpts"),
    height = grid::unit(canvas_height * scale, "bigpts"),
    default.units = default.units,
    hjust = canvas_hjust,
    vjust = canvas_vjust,
    # pictureGrob() pads the picture by 5% per side by default, which would
    # draw Typst output at 1 / 1.1 of its size.
    expansion = 0,
    ext = "clipbbox"
  )
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
