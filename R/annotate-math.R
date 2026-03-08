#' @include annotate-typst.R
NULL

#' Annotate a Plot with Typst Math
#'
#' Wraps `typst_code` in math delimiters and forwards to [annotate_typst()].
#' If `typst_code` is already wrapped with outer `$...$` or `$$...$$`, the outer
#' delimiters are removed before wrapping again.
#'
#' @inheritParams annotate_typst
#' @param typst_math_code Typst source code representing math content. It will be
#'   wrapped in `$ ... $` before rendering.
#' @param face Optional math face. Only `"plain"` and `"bold"` are supported
#'   for math annotations.
#' @param fontface Alias of `face`. Only `"plain"` and `"bold"` are supported
#'   for math annotations.
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
  size.unit = "pt",
  alpha = NULL,
  color = NULL,
  colour = NULL,
  face = NULL,
  fontface = NULL,
  lineheight = NULL,
  math_family = NULL,
  angle = NULL,
  inline = FALSE
) {
  params <- normalize_face_param(
    list(face = face, fontface = fontface),
    fn = "annotate_math_typst",
    supported = c("plain", "bold")
  )
  typst_math_code <- as_typst_math_code(typst_math_code, inline = inline)

  annotate_typst(
    typst_math_code,
    x = x,
    y = y,
    hjust = hjust,
    vjust = vjust,
    scale = scale,
    size = size,
    size.unit = size.unit,
    alpha = alpha,
    color = color,
    colour = colour,
    face = params$face,
    lineheight = lineheight,
    math_family = math_family,
    angle = angle
  )
}

#' Annotate a Plot with MiTeX-Converted LaTeX Math
#'
#' Converts LaTeX math input to Typst math source with MiTeX, wraps it in math delimiters,
#' and forwards rendering to [annotate_typst()]. Details see [convert_latex_to_typst()].
#'
#' @inheritParams annotate_typst
#' @param latex_math_code A single LaTeX math string. Outer `$...$` or `$$...$$`
#'   delimiters are optional and will be normalized.
#' @param face Optional math face. Only `"plain"` and `"bold"` are supported
#'   for math annotations.
#' @param fontface Alias of `face`. Only `"plain"` and `"bold"` are supported
#'   for math annotations.
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
  size.unit = "pt",
  alpha = NULL,
  color = NULL,
  colour = NULL,
  face = NULL,
  fontface = NULL,
  lineheight = NULL,
  math_family = NULL,
  angle = NULL,
  inline = FALSE
) {
  params <- normalize_face_param(
    list(face = face, fontface = fontface),
    fn = "annotate_math_mitex",
    supported = c("plain", "bold")
  )
  typst_math_code <- tryCatch(
    convert_latex_to_typst(latex_math_code, inline = inline),
    error = function(cnd) {
      preview <- trimws(latex_math_code)
      if (nchar(preview) > 80) {
        preview <- paste0(substr(preview, 1, 77), "...")
      }

      cli::cli_abort(
        c(
          "Failed to convert LaTeX math in {.fn annotate_math_mitex}.",
          "i" = "Input: {.val {preview}}"
        ),
        parent = cnd
      )
    }
  )

  annotate_typst(
    typst_math_code,
    x = x,
    y = y,
    hjust = hjust,
    vjust = vjust,
    scale = scale,
    size = size,
    size.unit = size.unit,
    alpha = alpha,
    color = color,
    colour = colour,
    face = params$face,
    lineheight = lineheight,
    math_family = math_family,
    angle = angle
  )
}
