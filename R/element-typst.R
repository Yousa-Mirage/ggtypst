#' @include helper.R grob.R build-source.R typst-svg.R
NULL

# element_typst ----------------------------------------------------------------

# ggplot2 4.x theme elements are S7 objects. Defining element_typst as an S7
# subclass of element_text lets ggplot2 resolve inheritance, including
# inherit.blank, before element_grob() is called.
element_typst_class <- S7::new_class(
  "element_typst",
  parent = ggplot2::element_text,
  properties = list(
    size.unit = S7::new_property(S7::class_character, default = "pt"),
    math_family = S7::new_property(
      S7::new_union(S7::class_character, NULL),
      default = NULL
    )
  )
)

# ggplot2's default S7 element merge expects parent elements to expose the same
# property set. element_typst extends element_text with math_family and
# size.unit, so merge only the fields that exist on the parent.
merge_element_element_typst <- function(new, old, ...) {
  # Match ggplot2's element semantics: a concrete element overrides an existing
  # element_blank() at the same theme slot. inherit.blank is resolved later when
  # calc_element() combines parent and child theme elements.
  if (inherits(old, "ggplot2::element_blank") || inherits(old, "element_blank")) {
    return(new)
  }

  if (!(inherits(old, "ggplot2::element") || inherits(old, "element"))) {
    cli::cli_abort("Can only merge with other ggplot2 theme elements.")
  }

  if (!inherits(new, class(old)[1])) {
    cli::cli_abort("Only elements of the same class can be merged.")
  }

  old_props <- if (S7::S7_inherits(old)) {
    S7::props(old)
  } else {
    old
  }

  idx <- lengths(S7::props(new)) == 0
  idx <- intersect(names(idx[idx]), names(old_props))

  if (length(idx) > 0) {
    S7::props(new)[idx] <- old_props[idx]
  }

  new
}

#' Theme element that renders text with Typst
#'
#' A drop-in replacement for [ggplot2::element_text()] that compiles each label
#' through the Typst typesetting engine, enabling rich text formatting and
#' mathematical formulas in plot titles, axis labels, tick labels, strip text,
#' legend text, and any other position that accepts `element_text()`.
#'
#' By default, `size` is interpreted in points (`"pt"`). Set `size.unit = "mm"`
#' to match ggplot2 geom text size conventions.
#'
#' @param family Font family name.
#' @param face,fontface Font face (`"plain"`, `"bold"`, `"italic"`, or `"bold.italic"`).
#' @param size Font size (default unit controlled by `size.unit`).
#' @param colour,color Text colour.
#' @param hjust Horizontal justification (0 = left, 0.5 = centre, 1 = right).
#' @param vjust Vertical justification (0 = bottom, 0.5 = centre, 1 = top).
#' @param angle Rotation angle in degrees.
#' @param lineheight Line height value. May be negative.
#' @param margin Margins around the text, created with [ggplot2::margin()].
#' @param math_family Optional font family for Typst math mode.
#' @param size.unit Unit used to interpret `size`. Either `"pt"` (default) or
#'   `"mm"`.
#' @param debug If `TRUE`, draw a coloured rectangle behind each label for
#'   debugging layout.
#' @param inherit.blank If `TRUE`, `element_blank` parents suppress this element.
#' @return A ggplot2 theme element that can be used inside a [ggplot2::theme()]
#'   call.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#'
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   ggtitle("$ E = m c^2 $") +
#'   theme(plot.title = element_typst())
#' }
#'
#' @seealso [geom_typst()], [annotate_typst()]
#' @export
element_typst <- function(
  family = NULL,
  face = NULL,
  fontface = NULL,

  size = NULL,
  colour = NULL,
  hjust = NULL,
  vjust = NULL,
  angle = NULL,
  lineheight = NULL,
  color = NULL,
  margin = NULL,
  math_family = NULL,
  size.unit = "pt",
  debug = NULL,
  inherit.blank = FALSE
) {
  face <- resolve_arg_alias(face, fontface, "face", "fontface")
  colour <- resolve_arg_alias(colour, color, "colour", "color")

  size.unit <- check_size_unit(size.unit)
  math_family <- normalize_optional_string(math_family, empty_is_null = TRUE)

  element_typst_class(
    family = family,
    face = face,
    size = size,
    colour = colour,
    hjust = hjust,
    vjust = vjust,
    angle = angle,
    lineheight = lineheight,
    margin = margin,
    math_family = math_family,
    size.unit = size.unit,
    debug = debug,
    inherit.blank = inherit.blank
  )
}


# element_grob dispatching ----------------------------------------------------

element_grob.element_typst <- function(
  element,
  label = "",
  x = NULL,
  y = NULL,
  family = NULL,
  face = NULL,
  colour = NULL,
  size = NULL,
  hjust = NULL,
  vjust = NULL,
  angle = NULL,
  lineheight = NULL,
  margin = NULL,
  margin_x = FALSE,
  margin_y = FALSE,
  ...
) {
  if (is.null(label)) {
    return(ggplot2::zeroGrob())
  }

  # Resolve parameters: call-time overrides > element fields > defaults
  hjust <- hjust %||% element$hjust %||% 0.5
  vjust <- vjust %||% element$vjust %||% 0.5
  angle <- angle %||% element$angle %||% 0
  lineheight <- lineheight %||% element$lineheight
  margin <- margin %||% element$margin %||% ggplot2::margin(0, 0, 0, 0)
  size <- size %||% element$size
  colour <- colour %||% element$colour
  family <- family %||% element$family
  face <- face %||% element$face
  math_family <- element$math_family
  size.unit <- element$size.unit %||% "pt"
  debug <- isTRUE(element$debug)

  # Compute default position from rotated justification
  # TODO: if this can be made in Typst?
  just <- rotate_just(angle, hjust, vjust)

  n <- max(length(label), length(x), length(y), 1)
  x <- x %||% grid::unit(rep(just$hjust, n), "npc")
  y <- y %||% grid::unit(rep(just$vjust, n), "npc")
  if (!grid::is.unit(x)) {
    x <- grid::unit(x, "npc")
  }
  if (!grid::is.unit(y)) {
    y <- grid::unit(y, "npc")
  }

  # Drop blank labels and synchronise x/y positions
  keep <- !grepl("^\\s*$", label)
  if (!all(keep)) {
    label <- label[keep]
    if (length(label) == 0) {
      return(ggplot2::zeroGrob())
    }
    x <- x[keep]
    y <- y[keep]
  }

  # Normalize other args
  face <- normalize_face(face, "face")
  size_pt <- convert_size_to_pt(size, size.unit = size.unit)
  lineheight <- check_number(lineheight, "lineheight")
  family <- normalize_optional_string(family, empty_is_null = TRUE)
  math_family <- normalize_optional_string(math_family, empty_is_null = TRUE)

  typst_title_grob(
    label = label,
    x = x,
    y = y,
    hjust = hjust,
    vjust = vjust,
    size = size_pt,
    colour = colour,
    family = family,
    face = face,
    lineheight = lineheight,
    math_family = math_family,
    angle = angle,
    margin = margin,
    margin_x = margin_x,
    margin_y = margin_y,
    debug = debug
  )
}


# Internal grob builders ------------------------------------------------------

#' Build a complete Typst title grob with margin support
#'
#' Handles vectorised labels (e.g. axis tick labels), renders each through Typst,
#' and wraps the result in a margin-aware layout compatible with ggplot2's
#' `titleGrob` class.
#'
#' @param label Character vector of Typst source labels.
#' @param x,y Grid units for label positions.
#' @param hjust,vjust Justification values.
#' @param size Font size in points (already converted).
#' @param colour Text colour string.
#' @param family Font family or `NULL`.
#' @param face Normalised face string or `NULL`.
#' @param lineheight Line height value or `NULL`.
#' @param math_family Math font family or `NULL`.
#' @param angle Rotation angle in degrees.
#' @param margin Margin object.
#' @param margin_x,margin_y Whether margins apply in each direction.
#' @param debug Whether to draw debugging rectangles.
#' @return A `gTree` with class `"titleGrob"`.
#' @noRd
typst_title_grob <- function(
  label,
  x,
  y,
  hjust,
  vjust,
  size,
  colour,
  family,
  face,
  lineheight,
  math_family,
  angle,
  margin,
  margin_x,
  margin_y,
  debug
) {
  n <- length(label)

  grobs <- vector("list", n)
  widths <- numeric(n)
  heights <- numeric(n)

  for (i in seq_len(n)) {
    xi <- x[i]
    yi <- y[i]

    result <- typst_element_grob(
      label = label[[i]],
      x = xi,
      y = yi,
      hjust = hjust,
      vjust = vjust,
      size = size,
      colour = colour,
      family = family,
      face = face,
      lineheight = lineheight,
      math_family = math_family,
      angle = angle
    )

    grobs[[i]] <- result$grob
    widths[i] <- result$width_pt
    heights[i] <- result$height_pt
  }

  content <- grid::gTree(
    children = do.call(grid::gList, grobs),
    typst_width = grid::unit(max(widths), "pt"),
    typst_height = grid::unit(max(heights), "pt"),
    cl = "typst_grob"
  )

  # Margins set maybe have issues.
  add_typst_margins(
    grob = content,
    margin = margin,
    margin_x = margin_x,
    margin_y = margin_y,
    debug = debug
  )
}


#' Render a single Typst label to a positioned grob
#'
#' @param label A single Typst source string.
#' @param x,y Grid units for position.
#' @param hjust,vjust Justification values.
#' @param size Font size in points or `NULL`.
#' @param colour Text colour string.
#' @param family Font family or `NULL`.
#' @param face Normalised face string or `NULL`.
#' @param lineheight Line height value or `NULL`.
#' @param math_family Math font family or `NULL`.
#' @param angle Rotation angle in degrees.
#' @return A list with `$grob`, `$width_pt`, and `$height_pt`.
#' @noRd
typst_element_grob <- function(
  label,
  x,
  y,
  hjust,
  vjust,
  size,
  colour,
  family,
  face,
  lineheight,
  math_family,
  angle
) {
  angle <- if (angle %% 360 != 0) angle else NULL

  source <- build_typst_source(
    typst_code = label,
    size = size,
    color = colour,
    family = family,
    face = face,
    lineheight = lineheight,
    math_family = math_family,
    angle = angle
  )

  rendered <- typst_svg(source)

  width_pt <- rendered$width_pt
  height_pt <- rendered$height_pt

  grob <- vector_typst_grob(
    svg = rendered$svg,
    x = x,
    y = y,
    default.units = "npc",
    width_pt = width_pt,
    height_pt = height_pt,
    hjust = hjust,
    vjust = vjust,
    angle = angle
  )

  list(grob = grob, width_pt = width_pt, height_pt = height_pt)
}


# Margin wrapper ---------------------------------------------------------------

#' Wrap a grob with ggplot2-compatible margins
#'
#' Modelled after ggtext's `add_margins()` and ggplot2's `titleGrob` layout.
#' Returns a `gTree` with class `"titleGrob"` and `$widths`/`$heights` fields
#' that ggplot2's layout engine uses via `widthDetails.titleGrob()` and
#' `heightDetails.titleGrob()`.
#'
#' @param grob A grid grob (the rendered Typst content).
#' @param margin A `margin()` object (top, right, bottom, left).
#' @param margin_x,margin_y Whether margins apply horizontally/vertically.
#' @param debug Whether to draw a debugging background.
#' @return A `gTree` with class `"titleGrob"`.
#' @noRd
add_typst_margins <- function(
  grob,
  margin = NULL,
  margin_x = FALSE,
  margin_y = FALSE,
  debug = FALSE
) {
  if (is.null(margin)) {
    margin <- ggplot2::margin(0, 0, 0, 0)
  }

  width <- grid::grobWidth(grob)
  height <- grid::grobHeight(grob)

  margin_x <- isTRUE(margin_x)
  margin_y <- isTRUE(margin_y)

  if (margin_x && margin_y) {
    widths <- grid::unit.c(margin[4], width, margin[2])
    heights <- grid::unit.c(margin[1], height, margin[3])

    vp <- grid::viewport(
      layout = grid::grid.layout(3, 3, heights = heights, widths = widths)
    )
    child_vp <- grid::viewport(layout.pos.row = 2, layout.pos.col = 2)
  } else if (margin_x) {
    widths <- grid::unit.c(margin[4], width, margin[2])
    heights <- grid::unit(1, "null")

    vp <- grid::viewport(layout = grid::grid.layout(1, 3, widths = widths))
    child_vp <- grid::viewport(layout.pos.col = 2)
  } else if (margin_y) {
    widths <- grid::unit(1, "null")
    heights <- grid::unit.c(margin[1], height, margin[3])

    vp <- grid::viewport(layout = grid::grid.layout(3, 1, heights = heights))
    child_vp <- grid::viewport(layout.pos.row = 2)
  } else {
    # No margins active — simple wrapper
    children <- if (isTRUE(debug)) {
      grid::gList(grid::rectGrob(gp = grid::gpar(fill = "cornsilk", col = NA)), grob)
    } else {
      grid::gList(grob)
    }

    return(grid::gTree(
      children = children,
      widths = width,
      heights = height,
      cl = "titleGrob"
    ))
  }

  children <- if (isTRUE(debug)) {
    grid::gList(grid::rectGrob(gp = grid::gpar(fill = "cornsilk", col = NA)), grob)
  } else {
    grid::gList(grob)
  }

  grid::gTree(
    children = children,
    vp = grid::vpTree(vp, grid::vpList(child_vp)),
    widths = widths,
    heights = heights,
    cl = "titleGrob"
  )
}
