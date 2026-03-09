#' @include annotate-typst.R
NULL

#' Annotate a Plot with Typst Math
#'
#' `annotate_math_typst()` is the math-specialized variant of [annotate_typst()].
#' It treats the annotation text as Typst math expression and then renders it to a
#' math visual representation.
#' It will normalize the outer `$...$` or `$ ... $` delimiters automatically.
#'
#' @inheritParams annotate_typst
#' @param typst_math_code A single Typst math string. Outer `$...$` or `$ ... $`
#'   delimiters are optional.
#' @param face,fontface Optional math face. Only `"plain"` and `"bold"` are supported, because math text in Typst is italic by default.
#' @param inline Whether to render inline math. Default `FALSE` renders
#'   display-style math.
#' @return A `ggplot2` layer.
#' @seealso [annotate_typst()], [annotate_math_mitex()]
#'
#' @examples
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   annotate_math_typst(
#'     typst_math_code = r"(sum_(i=1)^n x_i)",
#'     x = 3.5,
#'     y = 30,
#'     size = 18,
#'     face = "bold"
#'   ) +
#'   theme_minimal()
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
  color = NULL,
  colour = NULL,
  alpha = NULL,
  face = NULL,
  fontface = NULL,
  angle = NULL,
  lineheight = NULL,
  math_family = NULL,
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
#' @description
#' `annotate_math_mitex()` is the LaTeX-input companion to [annotate_math_typst()].
#' It accepts LaTeX-style math, converts it to Typst math through [MiTeX](https://github.com/mitex-rs/mitex),
#' and renders the converted result as a single annotation.
#' It will normalize the outer `$...$` or `$$...$$` delimiters automatically.
#'
#' This allows you to use LaTeX math syntax in your plot, which you may be more familiar with :).
#'
#' @details
#' [MiTeX](https://github.com/mitex-rs/mitex) is a LaTeX-to-Typst converter. It should be stable and reliable for typical LaTeX math expressions. If you find any LaTeX math that MiTeX fails to convert properly, you can report an issue to `ggtypst` first.
#'
#' @inheritParams annotate_typst
#' @param latex_math_code A single LaTeX math string. Outer `$...$` or `$$...$$`
#'   delimiters are optional.
#' @param face,fontface Optional math face. Only `"plain"` and `"bold"` are supported, because math text in Typst is italic by default.
#' @param inline Whether to render inline math. Default `FALSE` renders
#'   display-style math.
#' @return A `ggplot2` layer.
#' @seealso [annotate_typst()], [annotate_math_typst()]
#'
#' @examples
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   annotate_math_mitex(
#'     latex_math_code = r"(\eta = \frac{mpg}{wt}, \text{written in LaTeX math})",
#'     x = 3.5,
#'     y = 30,
#'     size = 18,
#'     face = "bold"
#'   ) +
#'   theme_minimal()
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
  color = NULL,
  colour = NULL,
  alpha = NULL,
  face = NULL,
  fontface = NULL,
  angle = NULL,
  lineheight = NULL,
  math_family = NULL,
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
