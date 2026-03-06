#' Build a grid grob from rendered Typst output
#'
#' Wraps the imported vector picture in a lightweight gTree with explicit width
#' and height methods so ggplot can place it consistently.
#'
#' @param rendered A rendered Typst result containing SVG bytes and dimensions.
#' @param scale Scaling factor applied to rendered dimensions.
#' @param hjust,vjust Horizontal and vertical justification values.
#' @return A grid grob with class `"typst_grob"`.
#' @noRd
typst_grob <- function(
  rendered,
  scale = 1,
  hjust = 0.5,
  vjust = 0.5
) {
  width_pt <- rendered$width_pt * scale
  height_pt <- rendered$height_pt * scale

  grob <- tryCatch(
    vector_typst_grob(
      svg = rendered$svg,
      width_pt = width_pt,
      height_pt = height_pt,
      hjust = hjust,
      vjust = vjust
    ),
    error = function(cnd) {
      cli::cli_abort(c(
        "Failed to import Typst output as a vector graphic.",
        "i" = conditionMessage(cnd)
      ))
    }
  )

  grid::gTree(
    children = grid::gList(grob),
    typst_width = grid::unit(width_pt, "pt"),
    typst_height = grid::unit(height_pt, "pt"),
    cl = "typst_grob"
  )
}

#' Import Typst SVG output as a vector grob
#'
#' Canonicalizes the SVG, imports it through `grImport2`, and returns a
#' `pictureGrob` sized in points.
#'
#' @param svg Raw SVG bytes.
#' @param width_pt,height_pt Output dimensions in points.
#' @param hjust,vjust Horizontal and vertical justification values.
#' @return A `grImport2` picture grob.
#' @noRd
vector_typst_grob <- function(svg, width_pt, height_pt, hjust, vjust) {
  canonical_svg <- rsvg::rsvg_svg(svg)

  # TODO: Replace tempfile-based SVG import with an in-memory pipeline.
  path <- tempfile(fileext = ".svg")
  on.exit(unlink(path), add = TRUE)
  writeBin(canonical_svg, path, useBytes = TRUE)

  picture <- grImport2::readPicture(path, warn = FALSE)

  grImport2::pictureGrob(
    picture,
    width = grid::unit(width_pt, "pt"),
    height = grid::unit(height_pt, "pt"),
    hjust = hjust,
    vjust = vjust,
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
