#' Annotate a Plot with Typst Text
#'
#' `annotate_typst()` is a more powerful replacement for [ggplot2::annotate()] that
#' renders a single Typst string and inserts that grob into a `ggplot2` plot.
#' Use it when you want one manually positioned note, callout, or mixed text-and-math label.
#'
#' @param typst_code A single Typst source string to render.
#' @param x,y The annotation position in data coordinates.
#' @param hjust,vjust Horizontal and vertical justification for the rendered
#'   grob (`0` = bottom, `0.5` = center, `1` = top).
#' @param scale A positive scaling factor applied to the rendered Typst size.
#' @param size Optional text size.
#' @param size.unit The unit of `size`. Defaults to points
#'   (`"pt"`). Use `"mm"` for ggplot2-style text sizes.
#' @param alpha Optional color alpha multiplier in `[0, 1]`.
#' @param color,colour Optional text color. RGB or color name are supported.
#' @param face,fontface Optional text face: `"plain"`, `"bold"`, `"italic"`, or
#'   `"bold.italic"`.
#' @param lineheight Optional line height value. May be negative.
#' @param family Optional text font family. The family must be available to Typst. If `NULL` or not found, the default
#'   family will be used. If you want to show specific languages or characters (e.g., Chinese, Japanese, emoji), you may need to set this.
#' @param math_family Optional font family for math content. The default math font is `New Computer Modern Math`. To render a math expression, you don't need to set this and even don't need to have `New Computer Modern Math` installed on your system. Typst has embedded this font by default.
#' @param angle Optional text rotation angle in degrees.
#' @return A `ggplot2` layer.
#' @seealso [annotate_math_typst()], [annotate_math_mitex()]
#'
#' @examples
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   annotate_typst(
#'     typst_code = r"(#rect(radius: 5pt)[*Fuel economy* #linebreak() $sum_(i=1)^n x_i$])",
#'     x = 3.5,
#'     y = 30,
#'     size = 18,
#'     color = "red"
#'   ) +
#'   theme_minimal()
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
  color = NULL,
  colour = NULL,
  alpha = NULL,
  face = NULL,
  fontface = NULL,
  angle = NULL,
  lineheight = NULL,
  family = NULL,
  math_family = NULL
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
