#' Render Typst source for drawing
#'
#' Compiles Typst code and describes its first page. If Typst compilation
#' fails, this function throws a structured CLI error with diagnostics and
#' hints; Typst warnings are signalled as R warnings.
#'
#' @param text Typst source code to compile.
#' @return A list with fields:
#'   - `width_pt`, `height_pt`: size of the layout box, in points;
#'   - `descent_pt`: the largest font descent of the text, in points;
#'   - `scene`: what the page draws (see `scene_typst_grob()`);
#'   - `fallback`: `NULL`, or the page as SVG when the scene cannot be drawn
#'     with paths (fields `svg`, `width_pt`, `height_pt`, `offset_x_pt`,
#'     `offset_y_pt`, see `svg_typst_grob()`);
#'   - `warnings`: Typst warnings.
#' @noRd
typst_render <- function(text) {
  result <- rs_typst_render(text)

  if (inherits(result, "typst_error")) {
    abort_typst_errors(result)
  }
  if (is.list(result$warnings) && length(result$warnings) > 0) {
    warn_typst_warnings(result$warnings)
  }

  result
}
