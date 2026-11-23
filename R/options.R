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
#'   Text that native glyphs cannot express, such as text with a stroke or in
#'   a variable font (whose axes R's native glyphs cannot set), is always drawn
#'   as outlines. On R older than 4.5, the same applies to a
#'   label whose glyphs have different angles, such as a label with one
#'   rotated word in it. A label rotated as a whole still uses native glyphs.
#' - `ggtypst.raster_dpi`: resolution, in dots per inch, of the images that
#'   draw content grid cannot draw as text or lines: gradients, tilings,
#'   images, and color glyphs (emoji). Defaults
#'   to 600, twice the usual print resolution: an image drawn into an output
#'   of the same resolution is resampled and looks slightly soft, while one of
#'   twice the resolution looks the same as the vector original. Each image is
#'   limited to 4 million pixels; larger content gets a lower resolution. The
#'   images are made when the label is created, so set the option before
#'   creating the plot.
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
