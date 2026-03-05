#' Convert LaTeX Math to Typst Math Source
#'
#' Converts a LaTeX math expression to Typst math code using MiTeX and wraps it
#' with the mitex-scope.
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
      sprintf(r"(#let _ggtypst_mitex_expr = "%s")", converted),
      r"(#eval("$ " + _ggtypst_mitex_expr + " $", scope: mitex-scope))"
    ),
    collapse = "\n"
  )
}
