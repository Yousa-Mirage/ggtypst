#' Build Typst Source with Rendering Defaults
#'
#' Wraps user Typst code with page and text settings used by ggtypst.
#'
#' @param typst_code A single Typst source string.
#' @param size Font size in points (pt).
#' @param color Text color in any format accepted by [grDevices::col2rgb()].
#' @param colour Alias of `color`.
#' @param alpha Optional alpha multiplier in `[0, 1]`. If provided, it is
#'   multiplied with the alpha channel embedded in `color`.
#' @param family Font family name. The font must be available in the Typst rendering
#'   environment (e.g., system fonts or embedded fonts).
#' @param face Optional text face: `"plain"`, `"bold"`, `"italic"`, or
#'   `"bold.italic"`.
#' @param fontface Alias of `face`.
#' @param fontweight Optional font weight: a whole number from 100 to 900 or
#'   one of Typst's weight names. It replaces the weight `face` sets and keeps
#'   its italic.
#' @param weight Alias of `fontweight`.
#' @param lineheight Optional line height multiplier with grid's meaning:
#'   baselines of consecutive lines are `1.2 * lineheight` font sizes apart.
#'   Defaults to 1.2, `geom_text()`'s default.
#' @param math_family Optional font family for math mode. Defaults to `New Computer Modern Math`.
#' @param angle Text angle in degrees. Positive values rotate counter-clockwise.
#' @param hjust Optional horizontal justification. Aligns the lines of
#'   multi-line content to the left (`hjust < 0.25`), centre, or right
#'   (`hjust > 0.75`), the closest Typst can get to grid's per-line `hjust`.
#'   Alignment set inside `typst_code` still takes precedence.
#' @param call The call named in error messages.
#' @return A single UTF-8 Typst source string.
#' @noRd
build_typst_source <- function(
  typst_code,
  size = NULL,
  color = NULL,
  colour = NULL,
  alpha = NULL,
  family = NULL,
  face = NULL,
  fontface = NULL,
  fontweight = NULL,
  weight = NULL,
  lineheight = NULL,
  math_family = NULL,
  angle = NULL,
  hjust = NULL,
  call = rlang::caller_env()
) {
  typst_code <- check_single_string(
    typst_code,
    "typst_code",
    allow_null = FALSE,
    call = call
  )

  color <- resolve_arg_alias(color, colour, "color", "colour", call = call)
  face <- resolve_arg_alias(face, fontface, "face", "fontface", call = call)
  fontweight <- resolve_arg_alias(
    fontweight,
    weight,
    "fontweight",
    "weight",
    call = call
  )

  color <- check_single_string(color, "color", call = call)
  size <- check_positive_number(size, "size", call = call)
  alpha <- check_alpha(alpha, call = call)
  family <- check_single_string(family, "family", call = call)
  lineheight <- check_number(lineheight, "lineheight", call = call)
  math_family <- check_single_string(math_family, "math_family", call = call)
  angle <- check_number(angle, "angle", call = call)
  hjust <- check_number(hjust, "hjust", call = call)
  face <- normalize_face(face, "face", call = call)
  fontweight <- normalize_fontweight(fontweight, call = call)

  text_args <- c()

  # Alignment of lines within multi-line content
  if (!is.null(hjust)) {
    align <- if (hjust < 0.25) {
      "left"
    } else if (hjust > 0.75) {
      "right"
    } else {
      "center"
    }
    text_args <- c(text_args, sprintf("#set align(%s)", align))
  }

  # Color & Alpha
  if (!is.null(color) || !is.null(alpha)) {
    base_color <- if (is.null(color)) "black" else color
    fill_hex <- color_to_hex(base_color, alpha, call = call)
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

  # Face
  if (!is.null(face)) {
    text_args <- c(text_args, face_to_typst_styles(face))
  }

  # Weight. After the face, so it replaces the weight of a bold face.
  if (!is.null(fontweight)) {
    text_args <- c(
      text_args,
      sprintf("#set text(weight: %d)", fontweight),
      sprintf("#show math.equation: set text(weight: %d)", fontweight)
    )
  }

  # Math Family
  if (!is.null(math_family)) {
    text_args <- c(
      text_args,
      sprintf('#show math.equation: set text(font: "%s")', math_family)
    )
  }

  # Line height. grid puts baselines `1.2 * lineheight` font sizes apart. With
  # the cap-height/baseline line edges set below, Typst puts them the cap
  # height plus the paragraph leading apart, so set the leading to make up the
  # difference, measuring the cap height of the font actually in use. Inline
  # math gets the same top edge, so lines with formulas are spaced and sized
  # like text lines. The cap height is kept as a fraction of the font size
  # and both lengths are in em, so they follow a font size set inside
  # `typst_code`. This comes after all text settings so the measurement sees
  # them.
  text_args <- c(
    text_args,
    paste0(
      "#show: body => context {\n",
      "  let cap-em = measure([H]).height / text.size\n",
      "  show math.equation: set text(top-edge: cap-em * 1em)\n",
      sprintf(
        "  set par(leading: (%s * 1.2 - cap-em) * 1em)\n",
        format_typst_number(lineheight %||% 1.2)
      ),
      "  body\n",
      "}"
    )
  )

  # Angle
  if (!is.null(angle) && (angle %% 360 != 0)) {
    # Typst's visible SVG rotation direction is opposite to grid's text rotation.
    angle <- format_typst_number(-angle)
    typst_code <- sprintf("#rotate(%sdeg, reflow: true)[%s]", angle, typst_code)
  }

  # Like grid text, the layout box runs from the cap height of the first line to
  # the baseline of the last. Ink outside it (descenders, accents) is kept by the
  # renderer's canvas.
  preamble_set <- c(
    r"(#set page(width: auto, height: auto, margin: 0pt, fill: none))",
    r"(#set text(top-edge: "cap-height", bottom-edge: "baseline"))"
  )

  paste(c(preamble_set, text_args, typst_code), collapse = "\n")
}
