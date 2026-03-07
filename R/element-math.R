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

#' Theme element that renders Typst math
#'
#' A math-specialized variant of [element_typst()] that treats every non-blank
#' label as Typst math content. Outer `$...$` or `$$...$$` delimiters are
#' normalized automatically before rendering.
#'
#' Math theme elements only support `face = "plain"` and `face = "bold"`.
#'
#' @inheritParams element_typst
#' @param face,fontface Math face. Only `"plain"` and `"bold"` are supported.
#' @param inline Whether to render labels as inline math. Default `FALSE`
#'   renders display-style math.
#' @return A ggplot2 theme element that can be used inside a [ggplot2::theme()]
#'   call.
#' @export
element_math_typst <- function(
  face = NULL,
  fontface = NULL,
  size = NULL,
  colour = NULL,
  hjust = NULL,
  vjust = NULL,
  angle = NULL,
  lineheight = NULL,
  color = NULL,
  margin = NULL,
  math_family = NULL,
  size.unit = "pt",
  debug = NULL,
  inherit.blank = FALSE,
  inline = FALSE
) {
  face <- resolve_arg_alias(face, fontface, "face", "fontface")
  colour <- resolve_arg_alias(colour, color, "colour", "color")

  face <- normalize_math_face(face, "element_math_typst")
  size.unit <- check_size_unit(size.unit)
  math_family <- normalize_optional_string(math_family, empty_is_null = TRUE)
  inline <- check_bool(inline, "inline", allow_null = FALSE)

  element_math_typst_class(
    face = face,
    size = size,
    colour = colour,
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
  if (is.null(label)) {
    return(ggplot2::zeroGrob())
  }

  face <- face %||% element$face
  face <- normalize_math_face(face, "element_math_typst")
  label <- normalize_typst_math_element_labels(
    label,
    inline = isTRUE(element$inline),
    fn = "element_math_typst"
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
