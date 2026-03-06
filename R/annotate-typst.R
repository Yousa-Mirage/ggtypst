#' Annotate a Plot with Typst-Rendered Text
#'
#' Compiles Typst source code into SVG, converts it to a grid grob, and adds it
#' to a ggplot as a custom annotation layer.
#'
#' @param typst_code Typst source code to render.
#' @param x,y Position where the annotation is placed.
#' @param hjust,vjust Horizontal and vertical justification for the annotation grob.
#' @param scale Scaling factor applied to rendered Typst dimensions.
#' @param size Optional text size in points.
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

  grob <- typst_grob(
    rendered,
    scale = scale,
    hjust = hjust,
    vjust = vjust
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
#' @param inline Whether to render as inline math. Default `FALSE` renders as
#'   display-style math.
#' @return A `ggplot2` layer.
#' @export
annotate_math_typst <- function(
  typst_math_code,
  x,
  y,
  hjust = 0.5,
  vjust = 0.5,
  scale = 1,
  size = NULL,
  alpha = NULL,
  color = NULL,
  math_family = NULL,
  angle = NULL,
  inline = FALSE
) {
  typst_math_code <- as_typst_math_code(typst_math_code, inline = inline)

  annotate_typst(
    typst_math_code,
    x = x,
    y = y,
    hjust = hjust,
    vjust = vjust,
    scale = scale,
    size = size,
    alpha = alpha,
    color = color,
    math_family = math_family,
    angle = angle
  )
}

#' Annotate a Plot with MiTeX-Converted LaTeX Math
#'
#' Converts LaTeX math input to Typst math source with MiTeX, wraps it in math delimiters,
#' and forwards rendering to [annotate_typst()].
#'
#' @inheritParams annotate_typst
#' @param latex_math_code LaTeX math source string. Outer `$...$` or `$$...$$`
#'   delimiters are optional and are normalized before conversion.
#' @param inline Whether to render as inline math. Default `FALSE` renders as
#'   display-style math.
#' @return A `ggplot2` layer.
#' @export
annotate_math_mitex <- function(
  latex_math_code,
  x,
  y,
  hjust = 0.5,
  vjust = 0.5,
  scale = 1,
  size = NULL,
  alpha = NULL,
  color = NULL,
  math_family = NULL,
  angle = NULL,
  inline = FALSE
) {
  typst_math_code <- convert_latex_to_typst(latex_math_code, inline = inline)

  annotate_typst(
    typst_math_code,
    x = x,
    y = y,
    hjust = hjust,
    vjust = vjust,
    scale = scale,
    size = size,
    alpha = alpha,
    color = color,
    math_family = math_family,
    angle = angle
  )
}
