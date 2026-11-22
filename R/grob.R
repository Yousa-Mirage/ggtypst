#' Build a positioned Typst grob from rendered output
#'
#' @param rendered A rendered Typst result containing SVG bytes and dimensions.
#' @param x,y Position passed to [grImport2::pictureGrob()].
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
#' text. The page is drawn from its scene (glyph outlines and shapes as grid
#' paths) when the scene can describe everything on it, and otherwise from its
#' SVG.
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
  draw <- if (isTRUE(rendered$scene$path_ok)) scene_typst_grob else svg_typst_grob
  draw(
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

#' Draw a Typst scene with grid paths
#'
#' The layout box becomes a viewport whose native coordinates are Typst's
#' points with y pointing down, so scene coordinates are used as they are. Ink
#' outside the box (overhangs, descenders) is drawn, not clipped.
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
    just = c(just$hjust, just$vjust),
    xscale = c(0, width),
    yscale = c(height, 0)
  )

  grid::gTree(
    children = scene_path_grobs(rendered$scene, scale),
    vp = vp,
    cl = "typst_scene_grob"
  )
}

#' Grid grobs drawing a scene's glyph outlines and shapes
#'
#' @param scene The `scene` of a rendered Typst result.
#' @param scale Scaling factor, applied to line widths (coordinates are scaled
#'   by the viewport).
#' @return A `gList`.
#' @noRd
scene_path_grobs <- function(scene, scale = 1) {
  grobs <- list()

  outlines <- scene$outlines
  if (length(outlines$x) > 0) {
    # One path per glyph (`pathId`), made of its contours (`id`).
    glyphs <- scene$glyphs
    owners <- unique(outlines$owner)
    grobs <- c(
      grobs,
      list(grid::pathGrob(
        x = outlines$x,
        y = outlines$y,
        id = outlines$sub,
        pathId = outlines$owner,
        rule = "winding",
        default.units = "native",
        gp = grid::gpar(
          fill = glyphs$fill[owners],
          col = na_if_empty(glyphs$stroke[owners]),
          lwd = pt_to_lwd(glyphs$stroke_width[owners] * scale)
        )
      ))
    )
  }

  shapes <- scene$shapes
  points <- scene$shape_points
  for (i in seq_along(shapes$fill)) {
    keep <- points$owner == i
    if (!any(keep)) {
      next
    }
    gp <- grid::gpar(
      fill = na_if_empty(shapes$fill[i]),
      col = na_if_empty(shapes$stroke[i]),
      lwd = pt_to_lwd(shapes$stroke_width[i] * scale),
      lineend = shapes$lineend[i],
      linejoin = shapes$linejoin[i]
    )
    filled <- shapes$closed[i] == 1L || nzchar(shapes$fill[i])
    grobs <- c(
      grobs,
      list(
        if (filled) {
          grid::pathGrob(
            x = points$x[keep],
            y = points$y[keep],
            id = points$sub[keep],
            rule = shapes$rule[i],
            default.units = "native",
            gp = gp
          )
        } else {
          grid::polylineGrob(
            x = points$x[keep],
            y = points$y[keep],
            id = points$sub[keep],
            default.units = "native",
            gp = gp
          )
        }
      )
    )
  }

  do.call(grid::gList, grobs)
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
