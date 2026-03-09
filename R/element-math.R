#' @include element-typst.R
NULL

# element_math_typst ----------------------------------------------------------

# Math-only Typst theme elements reuse the element_typst property set and add
# an inline flag controlling how outer math delimiters are normalized.
element_math_typst_class <- S7::new_class(
  "element_math_typst",
  parent = element_typst_class,
  properties = list(
    inline = S7::new_property(S7::class_logical, default = FALSE)
  )
)

#' Render Theme Elements with Typst Math
#'
#' `element_math_typst()` is the math-specialized variant of [element_typst()]. It
#' treats the theme element's text as Typst math expression and then renders it as
#' the element's visual representation.
#' It will normalize the outer `$...$` or `$ ... $` delimiters automatically.
#'
#' @inheritParams element_typst
#' @param face,fontface Optional math face. Only `"plain"` and `"bold"` are supported, because math text in Typst is italic by default.
#' @param inline Whether to render inline math. Default `FALSE` renders
#'   display-style math.
#' @return A ggplot2 theme element.
#' @seealso [element_typst()], [element_math_mitex()]
#' @examples
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   labs(
#'     title = r"("Matrix Title:" mat(0, 1; 1, 0))",
#'     y = r"($sum_(i=1)^n c_i$)"
#'   ) +
#'   theme_minimal() +
#'   theme(
#'     plot.title = element_math_typst(size = 18, face = "bold"),
#'     axis.title.y = element_math_typst(size = 14)
#'   )
#' @export
element_math_typst <- function(
  hjust = NULL,
  vjust = NULL,
  scale = NULL,
  size = NULL,
  size.unit = "pt",
  color = NULL,
  colour = NULL,
  alpha = NULL,
  face = NULL,
  fontface = NULL,
  angle = NULL,
  lineheight = NULL,
  math_family = NULL,
  inline = FALSE,

  margin = NULL,
  debug = NULL,
  inherit.blank = FALSE
) {
  face <- resolve_arg_alias(face, fontface, "face", "fontface")
  colour <- resolve_arg_alias(colour, color, "colour", "color")

  face <- normalize_face_values(
    face,
    fn = "element_math_typst",
    supported = c("plain", "bold")
  )
  scale <- check_positive_number(scale, "scale")
  size.unit <- check_size_unit(size.unit)
  alpha <- check_alpha(alpha)
  math_family <- normalize_optional_string(math_family, empty_is_null = TRUE)
  inline <- check_bool(inline, "inline", allow_null = FALSE)

  element_math_typst_class(
    face = face,
    scale = scale,
    size = size,
    colour = colour,
    alpha = alpha,
    hjust = hjust,
    vjust = vjust,
    angle = angle,
    lineheight = lineheight,
    margin = margin,
    math_family = math_family,
    size.unit = size.unit,
    debug = debug,
    inherit.blank = inherit.blank,
    inline = inline
  )
}

# Shared implementation for math element grobs.
# Both element_math_typst and element_math_mitex delegates to this.
element_grob_math_impl <- function(
  element,
  label,
  kind,
  fn,
  x = NULL,
  y = NULL,
  family = NULL,
  face = NULL,
  colour = NULL,
  size = NULL,
  hjust = NULL,
  vjust = NULL,
  angle = NULL,
  lineheight = NULL,
  margin = NULL,
  margin_x = FALSE,
  margin_y = FALSE,
  ...
) {
  if (is.null(label)) {
    return(ggplot2::zeroGrob())
  }

  verb <- if (kind == "typst") "normalize" else "convert"
  noun <- if (kind == "typst") "Typst" else "LaTeX"
  error_msg <- sprintf("Failed to %s a %s math label in {.fn {fn}}.", verb, noun)

  face <- face %||% element$face
  face <- normalize_face_values(
    face,
    fn = fn,
    supported = c("plain", "bold")
  )
  label <- normalize_math_label_values(
    label = label,
    inline = isTRUE(element$inline),
    fn = fn,
    kind = kind,
    static_error = error_msg,
    mapped_error = error_msg,
    static_call = rlang::call2(fn),
    preserve_blank = TRUE
  )

  element_grob.element_typst(
    element = element,
    label = label,
    x = x,
    y = y,
    family = family,
    face = face,
    colour = colour,
    size = size,
    hjust = hjust,
    vjust = vjust,
    angle = angle,
    lineheight = lineheight,
    margin = margin,
    margin_x = margin_x,
    margin_y = margin_y,
    ...
  )
}

#' @noRd
#' @exportS3Method ggplot2::element_grob
element_grob.element_math_typst <- function(
  element,
  label = "",
  x = NULL,
  y = NULL,
  family = NULL,
  face = NULL,
  colour = NULL,
  size = NULL,
  hjust = NULL,
  vjust = NULL,
  angle = NULL,
  lineheight = NULL,
  margin = NULL,
  margin_x = FALSE,
  margin_y = FALSE,
  ...
) {
  element_grob_math_impl(
    element = element,
    label = label,
    kind = "typst",
    fn = "element_math_typst",
    x = x,
    y = y,
    family = family,
    face = face,
    colour = colour,
    size = size,
    hjust = hjust,
    vjust = vjust,
    angle = angle,
    lineheight = lineheight,
    margin = margin,
    margin_x = margin_x,
    margin_y = margin_y,
    ...
  )
}

# element_math_mitex ----------------------------------------------------------

# MiTeX-backed math theme elements reuse the Typst-math property set and only
# change how labels are normalized before rendering.
element_math_mitex_class <- S7::new_class(
  "element_math_mitex",
  parent = element_math_typst_class
)

#' Render Theme Elements with MiTeX-Converted LaTeX Math
#'
#' @description
#' `element_math_mitex()` is the LaTeX-input companion to [element_math_typst()].
#' It accepts LaTeX-style math, converts it to Typst math through [MiTeX](https://github.com/mitex-rs/mitex),
#' and renders the converted result as the element's visual representation.
#' It will normalize the outer `$...$` or `$$...$$` delimiters automatically.
#'
#' This allows you to use LaTeX math syntax in your plot, which you may be more familiar with :).
#'
#' @details
#' [MiTeX](https://github.com/mitex-rs/mitex) is a LaTeX-to-Typst converter. It should be stable and reliable for typical LaTeX math expressions. If you find any LaTeX math that MiTeX fails to convert properly, you can report an issue to `ggtypst` first.
#'
#' @inheritParams element_math_typst
#' @return A ggplot2 theme element.
#' @seealso [element_typst()], [element_math_typst()]
#' @examples
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   labs(x = r"(\eta = \frac{mpg}{wt}, \text{written in LaTeX math})") +
#'   theme_minimal() +
#'   theme(axis.title.x = element_math_mitex(size = 14))
#' @export
element_math_mitex <- function(
  hjust = NULL,
  vjust = NULL,
  scale = NULL,
  size = NULL,
  size.unit = "pt",
  color = NULL,
  colour = NULL,
  alpha = NULL,
  face = NULL,
  fontface = NULL,
  angle = NULL,
  lineheight = NULL,
  math_family = NULL,
  inline = FALSE,

  margin = NULL,
  debug = NULL,
  inherit.blank = FALSE
) {
  face <- resolve_arg_alias(face, fontface, "face", "fontface")
  colour <- resolve_arg_alias(colour, color, "colour", "color")

  face <- normalize_face_values(
    face,
    fn = "element_math_mitex",
    supported = c("plain", "bold")
  )
  scale <- check_positive_number(scale, "scale")
  size.unit <- check_size_unit(size.unit)
  alpha <- check_alpha(alpha)
  math_family <- normalize_optional_string(math_family, empty_is_null = TRUE)
  inline <- check_bool(inline, "inline", allow_null = FALSE)

  element_math_mitex_class(
    face = face,
    scale = scale,
    size = size,
    colour = colour,
    alpha = alpha,
    hjust = hjust,
    vjust = vjust,
    angle = angle,
    lineheight = lineheight,
    margin = margin,
    math_family = math_family,
    size.unit = size.unit,
    debug = debug,
    inherit.blank = inherit.blank,
    inline = inline
  )
}

#' @noRd
#' @exportS3Method ggplot2::element_grob
element_grob.element_math_mitex <- function(
  element,
  label = "",
  x = NULL,
  y = NULL,
  family = NULL,
  face = NULL,
  colour = NULL,
  size = NULL,
  hjust = NULL,
  vjust = NULL,
  angle = NULL,
  lineheight = NULL,
  margin = NULL,
  margin_x = FALSE,
  margin_y = FALSE,
  ...
) {
  element_grob_math_impl(
    element = element,
    label = label,
    kind = "mitex",
    fn = "element_math_mitex",
    x = x,
    y = y,
    family = family,
    face = face,
    colour = colour,
    size = size,
    hjust = hjust,
    vjust = vjust,
    angle = angle,
    lineheight = lineheight,
    margin = margin,
    margin_x = margin_x,
    margin_y = margin_y,
    ...
  )
}
