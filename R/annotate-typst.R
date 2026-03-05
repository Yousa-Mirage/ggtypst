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
#' @return A `ggplot2` layer.
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
  typst_code <- check_single_string(typst_code, "typst_code", allow_null = FALSE)
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

#' Annotate a Plot with Typst Math
#'
#' Wraps `typst_code` in math delimiters and forwards to [annotate_typst()].
#' If `typst_code` is already wrapped with outer `$...$` or `$$...$$`, the outer
#' delimiters are removed before wrapping again.
#'
#' @inheritParams annotate_typst
#' @param typst_math_code Typst source code representing math content. It will be
#'   wrapped in `$ ... $` before rendering.
#' @return A `ggplot2` layer.
#' @export
annotate_math_typst <- function(
  typst_math_code,
  x,
  y,
  hjust = 0.5,
  vjust = 0.5,
  scale = 1,
  dpi = 300,
  size = NULL,
  alpha = NULL,
  color = NULL,
  math_family = NULL,
  angle = NULL
) {
  typst_math_code <- as_typst_math_code(typst_math_code)

  annotate_typst(
    typst_math_code,
    x = x,
    y = y,
    hjust = hjust,
    vjust = vjust,
    scale = scale,
    dpi = dpi,
    size = size,
    alpha = alpha,
    color = color,
    math_family = math_family,
    angle = angle
  )
}

#' Annotate a Plot with MiTeX-Converted LaTeX Math
#'
#' Converts LaTeX math input to Typst math source with [convert_latex_to_typst()]
#' and forwards rendering to [annotate_typst()].
#'
#' @inheritParams annotate_typst
#' @param latex_math_code LaTeX math source string. Outer `$...$` or `$$...$$`
#'   delimiters are optional and are normalized before conversion.
#' @return A `ggplot2` layer.
#' @export
annotate_math_mitex <- function(
  latex_math_code,
  x,
  y,
  hjust = 0.5,
  vjust = 0.5,
  scale = 1,
  dpi = 300,
  size = NULL,
  alpha = NULL,
  color = NULL,
  math_family = NULL,
  angle = NULL
) {
  typst_math_code <- convert_latex_to_typst(latex_math_code)

  annotate_typst(
    typst_math_code,
    x = x,
    y = y,
    hjust = hjust,
    vjust = vjust,
    scale = scale,
    dpi = dpi,
    size = size,
    alpha = alpha,
    color = color,
    math_family = math_family,
    angle = angle
  )
}
