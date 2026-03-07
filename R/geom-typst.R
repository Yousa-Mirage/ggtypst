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
#' - `lineheight`
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
  params <- normalize_face_param(list(...))
  position <- resolve_typst_position(
    position,
    nudge_x = nudge_x,
    nudge_y = nudge_y,
    fn = "geom_typst",
    position_supplied = !missing(position),
    nudge_supplied = !missing(nudge_x) || !missing(nudge_y)
  )

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomTypst,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = c(
      list(
        size.unit = size.unit,
        na.rm = na.rm
      ),
      params
    )
  )
}

# ggproto objects

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

  if (is.null(key_size)) {
    key_size <- 3
  } else if (is.null(params$size.unit) || params$size.unit == "pt") {
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
    lineheight = NA,
    vjust = 0.5,
    alpha = NA,
    family = "",
    math_family = NA
  ),

  setup_data = function(data, params) {
    data$face <- normalize_face_values(
      data$face,
      rows = seq_len(nrow(data)),
      fn = "geom_typst",
      detail = r"(Supported values are "plain", "bold", "italic", or "bold.italic" (or numeric codes 1-4).)"
    )
    data
  },

  draw_panel = function(data, panel_params, coord, na.rm = FALSE, size.unit = "pt") {
    if (nrow(data) == 0) {
      return(ggplot2::zeroGrob())
    }

    data <- coord$transform(data, panel_params)

    # Resolve character justifications ("inward"/"outward") to numeric
    if (is.character(data$vjust)) {
      data$vjust <- compute_just(data$vjust, data$y)
    }
    if (is.character(data$hjust)) {
      data$hjust <- compute_just(data$hjust, data$x)
    }

    data$label <- as.character(data$label)
    data$.ggtypst_row <- seq_len(nrow(data))

    data <- ggplot2::remove_missing(
      data,
      na.rm = na.rm,
      vars = c("x", "y", "label", "size", "hjust", "vjust", "colour", "face", "family"),
      name = "geom_typst"
    )

    if (nrow(data) == 0) {
      return(ggplot2::zeroGrob())
    }

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
      lineheight = data$lineheight,
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

# Row rendering

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
#' @param lineheight Optional line height value. May be negative.
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
  lineheight,
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
        lineheight = normalize_optional_number(lineheight),
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
        angle = normalize_optional_number(angle),
        class = "typst_label_grob"
      )
    },
    error = function(cnd) {
      cli::cli_abort(
        c(
          "Failed to render a Typst label in {.fn geom_typst}.",
          "x" = "Problem in row {index}.",
          "i" = "Label: {.val {geom_typst_error_preview(label)}}"
        ),
        parent = cnd
      )
    }
  )
}

# Helper functions
resolve_typst_position <- function(
  position,
  nudge_x,
  nudge_y,
  fn,
  position_supplied,
  nudge_supplied
) {
  if (!nudge_supplied) {
    return(position)
  }

  if (position_supplied) {
    cli::cli_abort(
      "You must specify either {.arg position} or {.arg nudge_x}/{.arg nudge_y}, not both.",
      call = rlang::call2(fn)
    )
  }

  ggplot2::position_nudge(nudge_x, nudge_y)
}

normalize_face_param <- function(params, fn = NULL, supported = NULL) {
  params$face <- resolve_arg_alias(params$face, params$fontface, "face", "fontface")
  params$fontface <- NULL

  if (!("face" %in% names(params))) {
    return(params)
  }

  params$face <- normalize_face_values(
    params$face,
    fn = fn,
    supported = supported
  )

  params
}

normalize_face_values <- function(face, fn = NULL, detail = NULL, supported = NULL, rows = NULL) {
  if (is.null(face) || length(face) == 0) {
    return(face)
  }

  # Scalar path: validate a single static parameter value
  if (is.null(rows)) {
    face <- normalize_face(face, "face")
    if (!is.null(supported) && !(face %in% supported)) {
      cli::cli_abort(
        "{.arg face} for {.fn {fn}} must be either \"plain\" or \"bold\".",
        call = rlang::call2(fn)
      )
    }
    return(face)
  }

  # Vector path: validate each unique mapped aesthetic value
  valid <- !is.na(face)
  if (!any(valid)) {
    return(as.character(face))
  }

  face_values <- face[valid]
  face_keys <- as.character(face_values)
  unique_faces <- unique(face_values)
  unique_face_keys <- as.character(unique_faces)
  first_rows <- rows[valid][match(unique_face_keys, face_keys)]

  normalized_unique <- vapply(
    seq_along(unique_faces),
    function(i) {
      face_value <- unique_faces[[i]]
      row <- first_rows[[i]]

      abort_invalid_face <- function() {
        cli::cli_abort(
          c(
            "Invalid {.arg face} aesthetic in {.fn {fn}}.",
            "x" = "Problem in row {row} with value {.val {face_value}}.",
            "i" = detail
          ),
          call = NULL
        )
      }

      tryCatch(
        {
          normalized <- normalize_face(face_value, "face", allow_null = FALSE)
          if (!is.null(supported) && !(normalized %in% supported)) {
            abort_invalid_face()
          }
          normalized
        },
        error = function(cnd) abort_invalid_face()
      )
    },
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )

  result <- as.character(face)
  result[valid] <- normalized_unique[match(face_keys, unique_face_keys)]
  result
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
