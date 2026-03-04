#' Build Typst Source with Rendering Defaults
#'
#' Wraps user Typst code with page and text settings used by ggtypst.
#'
#' @param text A single Typst source string.
#' @param size Font size in points (pt).
#' @param color Text color in any format accepted by [grDevices::col2rgb()].
#' @param alpha Optional alpha multiplier in `[0, 1]`. If provided, it is
#'   multiplied with the alpha channel embedded in `color`.
#' @param family Font family name. The font must be available in the Typst rendering
#'   environment (e.g., system fonts or embedded fonts).
#' @param angle Text angle in degrees. Positive values rotate counter-clockwise.
#' @return A single UTF-8 Typst source string.
build_typst_source <- function(
  text,
  size = NULL,
  color = NULL,
  alpha = NULL,
  family = NULL,
  angle = NULL
) {
  text <- check_single_string(text, "text", allow_null = FALSE)

  color <- check_single_string(color, "color")
  size <- check_positive_number(size, "size")
  alpha <- check_alpha(alpha)
  family <- check_single_string(family, "family")
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

  # Angle
  if (!is.null(angle)) {
    angle <- format_typst_number(angle)
    text <- sprintf("#rotate(%sdeg, reflow: true)[%s]", angle, text)
  }

  page_set <- "#set page(width: auto, height: auto, margin: 0.2em, fill: none)"
  paste(c(page_set, text_args, text), collapse = "\n")
}
