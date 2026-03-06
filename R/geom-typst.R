#' Typst text labels
#'
#' Draw raw Typst labels at data positions, similar to [ggplot2::geom_text()].
#' Each label is compiled independently with Typst and rendered as a vector grob.
#' By default, `size` is interpreted in points (`"pt"`), not millimeters.
#' Set `size.unit = "mm"` to match ggplot2 text size conventions.
#'
#' @section Aesthetics:
#' `geom_typst()` understands the following aesthetics (required aesthetics are
#' in bold):
#'
#' - **`x`**
#' - **`y`**
#' - **`label`** Raw Typst source code.
#' - `alpha`
#' - `angle`
#' - `colour`
#' - `family`
#' - `hjust`
#' - `math_family`
#' - `size`
#' - `vjust`
#'
#' @inheritParams ggplot2::geom_text
#' @param size.unit Unit used to interpret the `size` aesthetic. Defaults to
#'   points (`"pt"`). Use `"mm"` for ggplot2-style text sizes.
#' @return A ggplot2 layer that can be added to a plot.
#' @export
geom_typst <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  nudge_x = 0,
  nudge_y = 0,
  size.unit = "pt",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  if (!missing(nudge_x) || !missing(nudge_y)) {
    if (!missing(position)) {
      cli::cli_abort(
        "You must specify either {.arg position} or {.arg nudge_x}/{.arg nudge_y}, not both."
      )
    }

    position <- ggplot2::position_nudge(nudge_x, nudge_y)
  }

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTypst,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      size.unit = size.unit,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom_typst
#' @format NULL
#' @usage NULL
GeomTypst <- ggplot2::ggproto(
  "GeomTypst",
  ggplot2::Geom,
  required_aes = c("x", "y", "label"),
  default_aes = ggplot2::aes(
    colour = "black",
    size = 11,
    angle = 0,
    hjust = 0.5,
    vjust = 0.5,
    alpha = NA,
    family = "",
    math_family = NA
  ),
  draw_panel = function(data, panel_params, coord, na.rm = FALSE, size.unit = "pt") {
    if (nrow(data) == 0) {
      return(ggplot2::zeroGrob())
    }

    # Transform data coordinates to panel coordinates
    data <- coord$transform(data, panel_params)

    # Resolve justifications
    if (is.character(data$vjust)) {
      data$vjust <- compute_typst_just(data$vjust, data$y)
    }
    if (is.character(data$hjust)) {
      data$hjust <- compute_typst_just(data$hjust, data$x)
    }

    data$label <- as.character(data$label)

    # Remove rows with missing required aesthetics
    data <- ggplot2::remove_missing(
      data,
      na.rm = na.rm,
      vars = c("x", "y", "label", "hjust", "vjust", "colour"),
      name = "geom_typst"
    )

    if (nrow(data) == 0) {
      return(ggplot2::zeroGrob())
    }

    # Render labels for each row and combine into a gTree
    grobs <- Map(
      f = geom_typst_row_grob,
      label = data$label,
      x = data$x,
      y = data$y,
      size = data$size,
      alpha = data$alpha,
      colour = data$colour,
      family = data$family,
      math_family = data$math_family,
      angle = data$angle,
      size.unit = size.unit,
      hjust = data$hjust,
      vjust = data$vjust
    )

    grid::gTree(
      children = do.call(grid::gList, grobs),
      cl = "geom_typst_grob"
    )
  },
  draw_key = ggplot2::draw_key_text
)

#' Build one positioned Typst label grob
#'
#' @param label Raw Typst source code.
#' @param x,y Position in transformed panel coordinates.
#' @param size A ggplot2 text size in millimeters.
#' @param alpha Optional alpha multiplier.
#' @param colour Optional text colour.
#' @param family Optional text font family.
#' @param math_family Optional math font family.
#' @param angle Optional rotation angle in degrees.
#' @param size.unit Unit used to interpret `size`.
#' @param hjust,vjust Horizontal and vertical justification values.
#' @return A positioned grob for the rendered label.
#' @noRd
geom_typst_row_grob <- function(
  label,
  x,
  y,
  size,
  alpha,
  colour,
  family,
  math_family,
  angle,
  size.unit,
  hjust,
  vjust
) {
  source <- build_typst_source(
    typst_code = label,
    size = convert_size_to_pt(
      geom_typst_optional_number(size),
      size.unit = size.unit
    ),
    alpha = geom_typst_optional_number(alpha),
    color = geom_typst_optional_string(colour),
    family = geom_typst_optional_string(family, empty_is_null = TRUE),
    math_family = geom_typst_optional_string(math_family, empty_is_null = TRUE),
    angle = geom_typst_optional_number(angle)
  )

  rendered <- typst_svg(source)

  positioned_typst_grob(
    rendered,
    x = x,
    y = y,
    default.units = "native",
    hjust = hjust,
    vjust = vjust,
    class = "typst_label_grob"
  )
}

#' Normalize optional scalar numbers for geom_typst
#'
#' @param x Value to normalize.
#' @return A scalar number or `NULL`.
#' @noRd
geom_typst_optional_number <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return(NULL)
  }
  as.numeric(x)
}

#' Normalize optional scalar strings for geom_typst
#'
#' @param x Value to normalize.
#' @param empty_is_null Whether `""` should be treated as `NULL`.
#' @return A scalar string or `NULL`.
#' @noRd
geom_typst_optional_string <- function(x, empty_is_null = FALSE) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return(NULL)
  }

  x <- as.character(x)
  if (empty_is_null && identical(x, "")) {
    return(NULL)
  }

  x
}

#' Resolve text justification strings for geom_typst
#'
#' @param just Character justifications.
#' @param axis Numeric coordinates after transformation.
#' @return Numeric justification values.
#' @noRd
compute_typst_just <- function(just, axis) {
  inward <- just == "inward"
  just[inward] <- c("left", "middle", "right")[typst_just_dir(axis[inward])]

  outward <- just == "outward"
  just[outward] <- c("right", "middle", "left")[typst_just_dir(axis[outward])]

  just_values <- c(
    left = 0,
    center = 0.5,
    right = 1,
    bottom = 0,
    middle = 0.5,
    top = 1
  )[just]
  unname(just_values)
}

#' Classify justification direction from panel coordinates
#'
#' @param axis Numeric coordinates after transformation.
#' @param tol Tolerance around the panel center.
#' @return Integer direction codes.
#' @noRd
typst_just_dir <- function(axis, tol = 0.001) {
  out <- rep(2L, length(axis))
  out[axis < 0.5 - tol] <- 1L
  out[axis > 0.5 + tol] <- 3L
  out
}
