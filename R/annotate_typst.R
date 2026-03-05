#' Annotate a Plot with Typst-Rendered Text
#'
#' Compiles Typst source code into SVG, rasterizes it, and adds it to a ggplot
#' as a custom annotation layer.
#'
#' @param typst_code Typst source code to render.
#' @param x,y Position where the annotation is placed.
#' @param hjust,vjust Horizontal and vertical justification for the raster grob.
#' @param scale Scaling factor applied to rendered Typst dimensions.
#' @param dpi Rasterization resolution used for SVG to raster conversion.
#' @param size Optional text size in points passed to [build_typst_source()].
#' @param alpha Optional alpha multiplier in `[0, 1]`.
#' @param color Optional text color accepted by [grDevices::col2rgb()].
#' @param family Optional text font family.
#' @param math_family Optional font family for math equations.
#' @param angle Optional text angle in degrees.
#' @return A `ggplot2` layer created by [ggplot2::annotation_custom()].
#' @export
annotate_typst <- function(
  typst_code,
  x,
  y,
  hjust = 0.5,
  vjust = 0.5,
  scale = 1,
  dpi = 300,
  size = NULL,
  alpha = NULL,
  color = NULL,
  family = NULL,
  math_family = NULL,
  angle = NULL
) {
  x <- check_number(x, "x", allow_null = FALSE)
  y <- check_number(y, "y", allow_null = FALSE)
  hjust <- check_number(hjust, "hjust", allow_null = FALSE)
  vjust <- check_number(vjust, "vjust", allow_null = FALSE)
  scale <- check_positive_number(scale, "scale", allow_null = FALSE)
  dpi <- check_positive_number(dpi, "dpi", allow_null = FALSE)

  full_source <- build_typst_source(
    typst_code,
    size = size,
    color = color,
    alpha = alpha,
    family = family,
    math_family = math_family,
    angle = angle
  )
  rendered <- typst_svg(full_source)

  pixel_width <- rendered$width_pt * scale
  pixel_height <- rendered$height_pt * scale
  raster_width <- max(1, ceiling(pixel_width * dpi / 72))
  raster_height <- max(1, ceiling(pixel_height * dpi / 72))

  img_matrix <- rsvg::rsvg_nativeraster(
    rendered$svg,
    width = raster_width,
    height = raster_height
  )

  grob <- grid::rasterGrob(
    img_matrix,
    width = grid::unit(pixel_width, "pt"),
    height = grid::unit(pixel_height, "pt"),
    hjust = hjust,
    vjust = vjust,
    interpolate = TRUE
  )

  ggplot2::annotation_custom(grob, xmin = x, xmax = x, ymin = y, ymax = y)
}
