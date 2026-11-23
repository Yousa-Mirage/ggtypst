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
#'   - `scene`: what the page draws, ready for grid (see `scene_grobs()`).
#'     Gradients, tilings, images, and colour glyphs are images in it, drawn
#'     at `getOption("ggtypst.raster_dpi")`;
#'   - `warnings`: Typst warnings.
#' @noRd
typst_render <- function(text) {
  result <- rs_typst_render(text, raster_dpi())

  if (inherits(result, "typst_error")) {
    abort_typst_errors(result)
  }
  if (is.list(result$warnings) && length(result$warnings) > 0) {
    warn_typst_warnings(result$warnings)
  }

  result
}

# Resolution of the images a scene draws content grid cannot (see
# `ggtypst-options`).
raster_dpi <- function() {
  dpi <- getOption("ggtypst.raster_dpi", 600)
  if (!is.numeric(dpi) || length(dpi) != 1L || !is.finite(dpi) || dpi <= 0) {
    cli::cli_abort(c(
      "Option {.code ggtypst.raster_dpi} must be a positive number.",
      "x" = "It is {.val {dpi}}."
    ))
  }
  as.numeric(dpi)
}
