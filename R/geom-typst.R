#' Typst text labels
#'
#' Draw raw Typst labels at data positions, similar to [ggplot2::geom_text()].
#' Each label is compiled independently with Typst and rendered as a vector grob.
#' By default, `size` is interpreted in points (`"pt"`), not millimeters.
#' Set `size.unit = "mm"` to match ggplot2 text size conventions.
#'
#' `label` can be mapped in [ggplot2::aes()] or supplied as a constant value,
#' for example `geom_typst(label = "Hello")`, to render the same Typst label
#' for every row in the layer.
#'
#' Static layer parameters also accept `fontface` as an alias of `face`.
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
#' - `face`
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
  size.unit <- check_size_unit(size.unit, "size.unit")
  params <- list(...)

  params$face <- resolve_arg_alias(params$face, params$fontface, "face", "fontface")
  params$fontface <- NULL

  if ("face" %in% names(params)) {
    params$face <- normalize_face(params$face, "face")
  }

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
    params = c(list(
      size.unit = size.unit,
      na.rm = na.rm
    ), params)
  )
}

#' Draw a compact legend key for Typst labels
#'
#' Uses a neutral point glyph instead of ggplot2's default text legend key,
#' which would otherwise display a large placeholder letter.
#'
#' @param data,params,size Standard ggplot2 legend key inputs.
#' @return A grid grob for the legend key.
#' @noRd
draw_key_typst <- function(data, params, size) {
  key_size <- normalize_optional_number(data$size)
  size_unit <- params$size.unit
  if (is.null(size_unit)) {
    size_unit <- "pt"
  }

  if (is.null(key_size)) {
    key_size <- 3
  } else if (size_unit == "pt") {
    key_size <- key_size / ggplot2::.pt
  }

  data$shape <- 16
  data$stroke <- 0
  data$size <- min(max(key_size, 1.5), 4.5)

  ggplot2::draw_key_point(data, params, size)
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
    face = "plain",
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
      data$vjust <- compute_just(data$vjust, data$y)
    }
    if (is.character(data$hjust)) {
      data$hjust <- compute_just(data$hjust, data$x)
    }

    data$label <- as.character(data$label)
    data$.ggtypst_row <- seq_len(nrow(data))

    # Remove rows with missing required aesthetics
    data <- ggplot2::remove_missing(
      data,
      na.rm = na.rm,
      vars = c("x", "y", "label", "hjust", "vjust", "colour", "face", "family"),
      name = "geom_typst"
    )

    if (nrow(data) == 0) {
      return(ggplot2::zeroGrob())
    }

    if (!is.null(data$face)) {
      u_faces <- unique(data$face)
      clean_u_faces <- vapply(
        seq_along(u_faces),
        function(i) {
          face_value <- u_faces[[i]]

          tryCatch(
            normalize_face(face_value, "face", allow_null = FALSE),
            error = function(cnd) {
              bad_idx <- which(as.character(data$face) == as.character(face_value))[1]
              original_row <- data$.ggtypst_row[[bad_idx]]
              cli::cli_abort(
                c(
                  "Invalid {.arg face} aesthetic in {.fn geom_typst}.",
                  "x" = "Problem in row {original_row} with value {.val {as.character(face_value)}}.",
                  "i" = "Supported values are \"plain\", \"bold\", \"italic\", or \"bold.italic\" (or numeric codes 1-4)."
                ),
                call = NULL
              )
            }
          )
        },
        FUN.VALUE = character(1),
        USE.NAMES = FALSE
      )
    }

    # Render labels for each row and combine into a gTree
    grobs <- Map(
      f = geom_typst_row_grob,
      index = data$.ggtypst_row,
      label = data$label,
      x = data$x,
      y = data$y,
      size = data$size,
      size.unit = size.unit,
      alpha = data$alpha,
      colour = data$colour,
      face = data$face,
      family = data$family,
      math_family = data$math_family,
      angle = data$angle,
      hjust = data$hjust,
      vjust = data$vjust
    )

    grid::gTree(
      children = do.call(grid::gList, grobs),
      cl = "geom_typst_grob"
    )
  },
  draw_key = draw_key_typst
)

#' Build one positioned Typst label grob
#'
#' @param index Row index of the label inside the layer data.
#' @param label Raw Typst source code.
#' @param x,y Position in transformed panel coordinates.
#' @param size Text size interpreted according to `size.unit`.
#' @param size.unit Unit used to interpret `size`.
#' @param alpha Optional alpha multiplier.
#' @param colour Optional text colour.
#' @param face Optional text face.
#' @param family Optional text font family.
#' @param math_family Optional math font family.
#' @param angle Optional rotation angle in degrees.
#' @param hjust,vjust Horizontal and vertical justification values.
#' @return A positioned grob for the rendered label.
#' @noRd
geom_typst_row_grob <- function(
  index,
  label,
  x,
  y,
  size,
  size.unit,
  alpha,
  colour,
  face,
  family,
  math_family,
  angle,
  hjust,
  vjust
) {
  tryCatch(
    {
      source <- build_typst_source(
        typst_code = label,
        size = convert_size_to_pt(
          normalize_optional_number(size),
          size.unit = size.unit
        ),
        alpha = normalize_optional_number(alpha),
        color = normalize_optional_string(colour),
        face = face,
        family = normalize_optional_string(family, empty_is_null = TRUE),
        math_family = normalize_optional_string(math_family, empty_is_null = TRUE),
        angle = normalize_optional_number(angle)
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
    },
    error = function(cnd) {
      label_preview <- geom_typst_error_preview(label)

      cli::cli_abort(
        c(
          "Failed to render a Typst label in {.fn geom_typst}.",
          "x" = "Problem in row {index}.",
          "i" = "Label: {.val {label_preview}}"
        ),
        parent = cnd
      )
    }
  )
}

#' Create a compact label preview for geom_typst errors
#'
#' @param label Raw Typst source code.
#' @param max_chars Maximum preview length before truncation.
#' @return An escaped, quoted preview string.
#' @noRd
geom_typst_error_preview <- function(label, max_chars = 80) {
  preview <- trimws(label)
  if (nchar(preview) > max_chars) {
    preview <- paste0(substr(preview, 1, max_chars - 3), "...")
  }
  preview
}
