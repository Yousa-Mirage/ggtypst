#' @include geom-typst.R
NULL

#' Typst math labels
#'
#' Draw Typst math labels at data positions, similar to [geom_typst()]. Each
#' `label` is treated as Typst math content; outer `$...$` or `$$...$$`
#' delimiters are optional and will be normalized before rendering.
#'
#' Static layer parameters also accept `fontface` as an alias of `face`.
#' Math labels only support `face = "plain"` and `face = "bold"`.
#'
#' @inheritParams geom_typst
#' @param inline Whether to render labels as inline math. Default `FALSE`
#'   renders display-style math.
#' @return A ggplot2 layer that can be added to a plot.
#' @export
geom_math_typst <- function(
  mapping = NULL,
  data = NULL,
  stat = "identity",
  position = "identity",
  ...,
  nudge_x = 0,
  nudge_y = 0,
  inline = FALSE,
  size.unit = "pt",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE
) {
  size.unit <- check_size_unit(size.unit, "size.unit")
  inline <- check_bool(inline, "inline", allow_null = FALSE)
  params <- normalize_face_param(
    list(...),
    fn = "geom_math_typst",
    supported = c("plain", "bold")
  )
  params$label <- normalize_math_labels(
    params$label,
    inline = inline,
    fn = "geom_math_typst"
  )
  position <- resolve_typst_position(
    position,
    nudge_x = nudge_x,
    nudge_y = nudge_y,
    fn = "geom_math_typst",
    position_supplied = !missing(position),
    nudge_supplied = !missing(nudge_x) || !missing(nudge_y)
  )

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomMathTypst,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = c(
      list(
        inline = inline,
        size.unit = size.unit,
        na.rm = na.rm
      ),
      params
    )
  )
}

normalize_math_labels <- function(label, inline, fn, rows = NULL) {
  if (is.null(label) || length(label) == 0) {
    return(label)
  }

  valid <- !is.na(label)
  if (!any(valid)) {
    return(label)
  }

  labels <- as.character(label[valid])
  unique_labels <- unique(labels)

  first_rows <- NULL
  if (!is.null(rows)) {
    first_rows <- rows[valid][match(unique_labels, labels)]
  }

  wrapped <- vapply(
    seq_along(unique_labels),
    function(i) {
      label_value <- unique_labels[[i]]

      tryCatch(
        as_typst_math_code(label_value, inline = inline),
        error = function(cnd) {
          if (is.null(first_rows)) {
            cli::cli_abort(
              c(
                "Failed to normalize the static {.arg label} parameter in {.fn {fn}}.",
                "i" = "Label: {.val {geom_typst_error_preview(label_value)}}"
              ),
              parent = cnd,
              call = NULL
            )
          }

          cli::cli_abort(
            c(
              "Failed to normalize a Typst math label in {.fn {fn}}.",
              "x" = "Problem in row {first_rows[[i]]}.",
              "i" = "Label: {.val {geom_typst_error_preview(label_value)}}"
            ),
            parent = cnd,
            call = NULL
          )
        }
      )
    },
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )

  label[valid] <- wrapped[match(labels, unique_labels)]
  label
}

#' @rdname geom_math_typst
#' @format NULL
#' @usage NULL
GeomMathTypst <- ggplot2::ggproto(
  "GeomMathTypst",
  GeomTypst,
  extra_params = c("na.rm", "size.unit", "inline"),
  setup_data = function(data, params) {
    if (is.null(data$label) && !is.null(params$label)) {
      data$label <- rep(params$label, length.out = nrow(data))
    }

    if (is.null(data$label)) {
      return(GeomTypst$setup_data(data, params))
    }

    data$label <- normalize_math_labels(
      data$label,
      inline = isTRUE(params$inline),
      fn = "geom_math_typst",
      rows = seq_len(nrow(data))
    )

    data$face <- normalize_face_values(
      data$face,
      rows = seq_len(nrow(data)),
      fn = "geom_math_typst",
      detail = "Math labels only support \"plain\" and \"bold\".",
      supported = c("plain", "bold")
    )

    data
  }
)