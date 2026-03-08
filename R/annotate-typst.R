#' Annotate a Plot with Typst-Rendered Text
#'
#' Compiles Typst source code into SVG, converts it to a grid grob, and adds it
#' to a ggplot as a custom annotation layer.
#'
#' @param typst_code Typst source code to render.
#' @param x,y Position where the annotation is placed.
#' @param hjust,vjust Horizontal and vertical justification for the annotation grob.
#' @param scale Scaling factor applied to rendered Typst dimensions.
#' @param size Optional text size.
#' @param size.unit Unit used to interpret `size`. Defaults to points (`"pt"`).
#'   Use `"mm"` for ggplot2-style text sizes.
#' @param alpha Optional alpha multiplier in `[0, 1]`.
#' @param color Optional text color accepted by [grDevices::col2rgb()].
#' @param colour Alias of `color`.
#' @param family Optional text font family.
#' @param face Optional text face: `"plain"`, `"bold"`, `"italic"`, or
#'   `"bold.italic"`.
#' @param fontface Alias of `face`.
#' @param lineheight Optional line height value. May be negative.
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
  size.unit = "pt",
  alpha = NULL,
  color = NULL,
  colour = NULL,
  family = NULL,
  face = NULL,
  fontface = NULL,
  lineheight = NULL,
  math_family = NULL,
  angle = NULL
) {
  typst_code <- check_single_string(typst_code, "typst_code", allow_null = FALSE)
  x <- check_number(x, "x", allow_null = FALSE)
  y <- check_number(y, "y", allow_null = FALSE)
  hjust <- check_number(hjust, "hjust", allow_null = FALSE)
  vjust <- check_number(vjust, "vjust", allow_null = FALSE)
  scale <- check_positive_number(scale, "scale", allow_null = FALSE)

  color <- resolve_arg_alias(color, colour, "color", "colour")
  face <- resolve_arg_alias(face, fontface, "face", "fontface")

  size <- convert_size_to_pt(size, size.unit = size.unit)

  full_source <- build_typst_source(
    typst_code,
    size = size,
    color = color,
    alpha = alpha,
    family = family,
    face = face,
    lineheight = lineheight,
    math_family = math_family,
    angle = angle
  )
  rendered <- typst_svg(full_source)

  grob <- positioned_typst_grob(
    rendered,
    scale = scale,
    hjust = hjust,
    vjust = vjust,
    angle = angle,
    class = "typst_annotation_grob"
  )

  ggplot2::annotation_custom(grob, xmin = x, xmax = x, ymin = y, ymax = y)
}
