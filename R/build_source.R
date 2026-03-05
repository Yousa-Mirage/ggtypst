#' Build Typst Source with Rendering Defaults
#'
#' Wraps user Typst code with page and text settings used by ggtypst.
#'
#' @param typst_code A single Typst source string.
#' @param size Font size in points (pt).
#' @param color Text color in any format accepted by [grDevices::col2rgb()].
#' @param alpha Optional alpha multiplier in `[0, 1]`. If provided, it is
#'   multiplied with the alpha channel embedded in `color`.
#' @param family Font family name. The font must be available in the Typst rendering
#'   environment (e.g., system fonts or embedded fonts).
#' @param math_family Optional font family for math mode. Defaults to `New Computer Modern Math`.
#' @param angle Text angle in degrees. Positive values rotate counter-clockwise.
#' @return A single UTF-8 Typst source string.
build_typst_source <- function(
  typst_code,
  size = NULL,
  color = NULL,
  alpha = NULL,
  family = NULL,
  math_family = NULL,
  angle = NULL
) {
  typst_code <- check_single_string(typst_code, "typst_code", allow_null = FALSE)

  color <- check_single_string(color, "color")
  size <- check_positive_number(size, "size")
  alpha <- check_alpha(alpha)
  family <- check_single_string(family, "family")
  math_family <- check_single_string(math_family, "math_family")
  angle <- check_number(angle, "angle")

  text_args <- c()

  # Color & Alpha
  if (!is.null(color) || !is.null(alpha)) {
    base_color <- if (is.null(color)) "black" else color
    fill_hex <- color_to_hex(base_color, alpha)
    text_args <- c(text_args, sprintf('#set text(fill: rgb("%s"))', fill_hex))
  }

  # Size
  if (!is.null(size)) {
    size_str <- format_typst_number(size)
    text_args <- c(text_args, sprintf('#set text(size: %spt)', size_str))
  }

  # Family
  if (!is.null(family)) {
    text_args <- c(text_args, sprintf('#set text(font: "%s")', family))
  }

  # Math Family
  if (!is.null(math_family)) {
    text_args <- c(text_args, sprintf('#show math.equation: set text(font: "%s")', math_family))
  }

  # Angle
  if (!is.null(angle)) {
    angle <- format_typst_number(angle)
    typst_code <- sprintf("#rotate(%sdeg, reflow: true)[%s]", angle, typst_code)
  }

  preamble_set <- c(
    r"(#set page(width: auto, height: auto, margin: 0.1em, fill: none))",
    r"(#set text(top-edge: "bounds", bottom-edge: "bounds"))"
  )

  paste(c(preamble_set, text_args, typst_code), collapse = "\n")
}

#' Convert LaTeX Math to Typst Math Source
#'
#' Converts a LaTeX math expression to Typst math code using MiTeX and wraps it
#' with the MiTeX scope prelude required for Typst compilation.
#'
#' @param latex_code A single LaTeX math string. Outer `$...$` or `$$...$$`
#'   delimiters are optional and will be normalized.
#' @return A single Typst source string.
convert_latex_to_typst <- function(latex_code) {
  latex_code <- as_latex_math_code(latex_code)
  converted <- rs_convert_latex_to_typst(latex_code)

  if (inherits(converted, "typst_error")) {
    abort_typst_error(converted)
  }

  converted <- check_single_string(converted$typst_code, "converted", allow_null = FALSE)

  paste(
    c(
      r"(#import "/specs/mod.typ": mitex-scope)",
      rs_mitex_alias_prelude(),
      sprintf("$ %s $", converted)
    ),
    collapse = "\n"
  )
}
