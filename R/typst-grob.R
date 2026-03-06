typst_grob <- function(
  rendered,
  scale = 1,
  hjust = 0.5,
  vjust = 0.5
) {
  scale <- check_positive_number(scale, "scale", allow_null = FALSE)
  hjust <- check_number(hjust, "hjust", allow_null = FALSE)
  vjust <- check_number(vjust, "vjust", allow_null = FALSE)

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
