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
#'   - `warnings`: Typst warnings;
#'   - `key`: a string identifying the scene (see
#'     `makeContent.typst_scene_grob()`).
#' @param call The call named in error and warning messages.

#' @noRd
typst_render <- function(text, call = rlang::caller_env()) {
  dpi <- raster_dpi(call = call)
  result <- rs_typst_render(text, dpi)

  if (inherits(result, "typst_error")) {
    abort_typst_errors(result, call = call)
  }
  if (is.list(result$warnings) && length(result$warnings) > 0) {
    warn_typst_warnings(result$warnings, call = call)
  }

  # The source and the raster resolution determine the scene.
  result$key <- rlang::hash(list(text, dpi))
  result
}

# Resolution of the images a scene draws content grid cannot (see
# `ggtypst-options`).
raster_dpi <- function(call = rlang::caller_env()) {
  dpi <- getOption("ggtypst.raster_dpi", 600)
  if (!is.numeric(dpi) || length(dpi) != 1L || !is.finite(dpi) || dpi <= 0) {
    cli::cli_abort(
      c(
        "Option {.code ggtypst.raster_dpi} must be a positive number.",
        "x" = "It is {.val {dpi}}."
      ),
      call = call
    )
  }
  as.numeric(dpi)
}
