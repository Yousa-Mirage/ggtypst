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

#' Import Typst SVG output as a vector grob
#'
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
#' @param rendered A rendered Typst result from [typst_svg()].
#' @param x,y Position passed to [grImport2::pictureGrob()].
#' @param default.units Grid units for numeric `x` and `y`.
#' @param scale Scaling factor applied to rendered dimensions.
#' @param hjust,vjust Horizontal and vertical justification values.
#' @param angle Rotation angle in ggplot2/grid semantics.
#' @return A `grImport2` picture grob.
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
