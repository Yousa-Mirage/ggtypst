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
#' text. The layout box becomes a viewport. Scene coordinates (Typst points
#' from the box's bottom-left corner, y pointing up) are drawn in `"bigpts"`,
#' which also works for boxes of zero width or height (a bare `#line()`). Ink
#' outside the box (overhangs, descenders) is drawn, not clipped.
#'
#' The grob's content is built when it is drawn (see
#' `makeContent.typst_scene_grob()`), so it can draw text as native glyphs or
#' as outlines to suit the device.
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
  entry <- list(
    scale = x$scale,
    groups = device_draws_groups(),
    font_dir = NULL,
    font_files = character()
  )

  font_list <- NULL
  outlines <- NULL
  if (length(x$scene$glyphs$id) > 0L) {
    if (mode == "glyph") {
      entry$font_dir <- font_cache_dir()
      entry$font_files <- typst_font_files(x$scene$fonts)
      font_list <- glyph_font_list(x$scene$fonts, entry$font_files)
    } else {
      outlines <- glyph_outlines(x$scene)
    }
  }
  entry$children <- scene_grobs(
    x$scene,
    mode,
    font_list,
    outlines,
    entry$groups,
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
  if (
    is.null(entry) ||
      !identical(entry$scale, scale) ||
      !identical(entry$groups, device_draws_groups())
  ) {
    return(FALSE)
  }
  is.null(entry$font_dir) ||
    (identical(entry$font_dir, font_cache_dir()) &&
      all(file.exists(entry$font_files)))
}

#' Choose how to draw a scene's text: `"glyph"` or `"path"`
#'
#' Follows `getOption("ggtypst.renderer")` (see [ggtypst-options]). With
#' `"auto"`, text is drawn as native glyphs on devices that rasterize or embed
#' fonts, and as outlines on devices that only reference fonts by name (such as
#' [grDevices::pdf()]), where glyphs could be shown in the wrong font. Scenes
#' that glyphs cannot express (stroked or skewed text, ...) are always drawn as
#' outlines. On R older than 4.5, text with glyphs at different angles is also
#' drawn as outlines (see `glyph_run_grob()`).
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

  # R older than 4.5 cannot give each native glyph its own angle.
  if (!glyph_rot_supported() && !glyph_runs_share_angle(scene)) {
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

# Whether the current device can draw grid groups, which nested clip regions
# need. Devices without them (vdiffr's SVG device, postscript()) report no
# compositing operators.
device_draws_groups <- function() {
  operators <- grDevices::dev.capabilities("compositing")$compositing
  !(is.null(operators) || anyNA(operators) || isFALSE(operators))
}

device_draws_glyphs <- function() {
  name <- names(grDevices::dev.cur())
  known <- startsWith(name, "agg_") || name %in% glyph_devices
  known && isTRUE(grDevices::dev.capabilities("glyphs")$glyphs)
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
