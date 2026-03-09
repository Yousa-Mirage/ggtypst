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
    scale = S7::new_property(
      S7::new_union(S7::class_numeric, NULL),
      default = NULL
    ),
    alpha = S7::new_property(
      S7::new_union(S7::class_numeric, NULL),
      default = NULL
    ),
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

#' Render Theme Elements with Typst Text
#'
#' `element_typst()` is a more powerful replacement for [ggplot2::element_text()] and [ggtext::element_markdown()] that
#' renders each theme element through Typst. Use it for plot titles, subtitles,
#' axis titles, axis text, facet strips, legend text, and other theme slots that
#' accept text elements.
#'
#' @param face,fontface Optional text face: `"plain"`, `"bold"`, `"italic"`, or
#'   `"bold.italic"`.
#' @param scale A positive scaling factor applied to the rendered Typst size.
#' @param size Optional font size.
#' @param size.unit The unit of `size`. Defaults to points
#'   (`"pt"`). Use `"mm"` for ggplot2-style text sizes.
#' @param color,colour Optional text color. RGB or color name are supported.
#' @param alpha Optional color alpha multiplier in `[0, 1]`.
#' @param hjust,vjust Horizontal and vertical justification for the rendered
#'   grob (`0` = bottom, `0.5` = center, `1` = top).
#' @param angle Optional text rotation angle in degrees.
#' @param lineheight Optional line height value. May be negative.
#' @param family Optional text font family. The family must be available to Typst. If `NULL` or not found, the default
#'   family will be used. If you want to show specific languages or characters (e.g., Chinese, Japanese, emoji), you may need to set this.
#' @param math_family Optional font family for math content. The default math font is `New Computer Modern Math`. To render a math expression, you don't need to set this and even don't need to have `New Computer Modern Math` installed on your system. Typst has embedded this font by default.
#' @param margin Optional text margins created with [ggplot2::margin()].
#' @param debug If `TRUE`, draw a coloured rectangle behind each label for
#'   debugging layout.
#' @param inherit.blank If `TRUE`, `element_blank()` parents suppress this
#'   element.
#' @return A ggplot2 theme element.
#' @seealso [element_math_typst()], [element_math_mitex()]
#'
#' @examples
#' ggplot(mtcars, aes(wt, mpg)) +
#'   geom_point() +
#'   labs(
#'     title = r"(*Typst Title* #linebreak() with inline math $E = m c^2$)",
#'     x = r"(X-axis powered by _Typst_: $hat(y) = beta_0 + beta_1 x$)"
#'   ) +
#'   theme_minimal() +
#'   theme(
#'     plot.title = element_typst(size = 18, color = "red"),
#'     axis.title.x = element_typst(size = 14, color = "#40a2b4")
#'   )
#'
#' @export
element_typst <- function(
  hjust = NULL,
  vjust = NULL,
  scale = NULL,
  size = NULL,
  size.unit = "pt",
  color = NULL,
  colour = NULL,
  alpha = NULL,
  face = NULL,
  fontface = NULL,
  angle = NULL,
  lineheight = NULL,
  family = NULL,
  math_family = NULL,

  margin = NULL,
  debug = NULL,
  inherit.blank = FALSE
) {
  face <- resolve_arg_alias(face, fontface, "face", "fontface")
  colour <- resolve_arg_alias(colour, color, "colour", "color")

  scale <- check_positive_number(scale, "scale")
  size.unit <- check_size_unit(size.unit)
  alpha <- check_alpha(alpha)
  math_family <- normalize_optional_string(math_family, empty_is_null = TRUE)

  element_typst_class(
    family = family,
    face = face,
    scale = scale,
    size = size,
    colour = colour,
    alpha = alpha,
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

#' @noRd
#' @exportS3Method ggplot2::element_grob
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
  scale <- element$scale %||% 1
  alpha <- element$alpha
  math_family <- element$math_family
  size.unit <- element$size.unit %||% "pt"
  debug <- isTRUE(element$debug)

  # Compute default position from rotated justification
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
    scale = scale,
    size = size_pt,
    colour = colour,
    alpha = alpha,
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
#' @param scale Scaling factor applied after Typst rendering.
#' @param size Font size in points (already converted).
#' @param colour Text colour string.
#' @param alpha Optional alpha multiplier.
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
  scale,
  size,
  colour,
  alpha,
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
      scale = scale,
      size = size,
      colour = colour,
      alpha = alpha,
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
#' @param scale Scaling factor applied after Typst rendering.
#' @param size Font size in points or `NULL`.
#' @param colour Text colour string.
#' @param alpha Optional alpha multiplier.
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
  scale,
  size,
  colour,
  alpha,
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
    alpha = alpha,
    family = family,
    face = face,
    lineheight = lineheight,
    math_family = math_family,
    angle = angle
  )

  rendered <- typst_svg(source)

  width_pt <- rendered$width_pt * scale
  height_pt <- rendered$height_pt * scale

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
