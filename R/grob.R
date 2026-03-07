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
      svg = rendered$svg,
      x = x,
      y = y,
      default.units = default.units,
      width_pt = width_pt,
      height_pt = height_pt,
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
    typst_width = grid::unit(width_pt, "pt"),
    typst_height = grid::unit(height_pt, "pt")
  )

  class(out) <- unique(c(class, "typst_grob", class(out)))
  out
}

#' Import Typst SVG output as a vector grob
#'
#' Canonicalizes the SVG, imports it through `grImport2`, and returns a
#' `pictureGrob` sized in points.
#'
#' @param svg Raw SVG bytes.
#' @param x,y Position passed to [grImport2::pictureGrob()].
#' @param default.units Grid units for numeric `x` and `y`.
#' @param width_pt,height_pt Output dimensions in points.
#' @param hjust,vjust Horizontal and vertical justification values.
#' @param angle Rotation angle in ggplot2/grid semantics.
#' @return A `grImport2` picture grob.
#' @noRd
vector_typst_grob <- function(
  svg,
  x = 0.5,
  y = 0.5,
  default.units = "npc",
  width_pt,
  height_pt,
  hjust,
  vjust,
  angle = NULL
) {
  just <- rotate_just(angle = angle, hjust = hjust, vjust = vjust)

  canonical_svg <- rsvg::rsvg_svg(svg)

  # TODO: Replace tempfile-based SVG import with an in-memory pipeline.
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path), add = TRUE)
  writeBin(canonical_svg, path, useBytes = TRUE)

  picture <- grImport2::readPicture(path, warn = FALSE)

  grImport2::pictureGrob(
    picture,
    x = x,
    y = y,
    width = grid::unit(width_pt, "pt"),
    height = grid::unit(height_pt, "pt"),
    default.units = default.units,
    hjust = just$hjust,
    vjust = just$vjust,
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
