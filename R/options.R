#' Options for drawing Typst output
#'
#' @description
#' ggtypst lays text out with Typst and draws the result with grid. These
#' options control how it is drawn; set them with [options()].
#'
#' - `ggtypst.renderer`: how text is drawn.
#'   - `"auto"` (default): as native glyphs, which the graphics device draws
#'     with the font itself exactly like [ggplot2::element_text()] (hinted and
#'     crisp on screen, real fonts embedded in [grDevices::cairo_pdf()] output),
#'     on devices that rasterize or embed fonts: ragg, `png()` and friends,
#'     [grDevices::cairo_pdf()], [grDevices::cairo_ps()], svglite, and Quartz.
#'     On other devices, such as [grDevices::pdf()] and
#'     [grDevices::postscript()], which only reference fonts by name, text is
#'     drawn as outlines so it looks the same on every computer.
#'   - `"glyph"`: native glyphs on every device.
#'   - `"path"`: outlines on every device.
#'
#'   Native glyphs need R 4.3 or later. Text they cannot express, such as text
#'   with a stroke, is always drawn as outlines.
#' - `ggtypst.font_cache`: directory where fonts built into ggtypst (Typst's
#'   default fonts) are written so that graphics devices can load them for
#'   native glyphs. Defaults to a directory in [tempdir()], written once per
#'   session.
#'
#' @name ggtypst-options
#' @examples
#' # Always draw text as outlines
#' old <- options(ggtypst.renderer = "path")
#' options(old)
NULL
