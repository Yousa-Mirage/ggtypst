#' Render Typst Source to SVG Bytes
#'
#' Compiles Typst code and returns SVG bytes together with rendered dimensions.
#' If Typst compilation fails, this function throws a structured CLI error with
#' diagnostics and hints.
#'
#' @param text Typst source code to compile.
#' @return A list with fields `svg`, `width_pt`, `height_pt`, and `warnings`.
#' @export
typst_svg <- function(text) {
  result <- rs_typst_svg(text)

  if (inherits(result, "typst_error")) {
    abort_typst_error(result)
  }
  if (is.list(result$warnings) && length(result$warnings) > 0) {
    warn_typst_warnings(result$warnings)
  }

  result
}
