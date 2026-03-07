#' @include geom-typst.R
NULL

# geom_math_typst

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

#' @rdname geom_math_typst
#' @format NULL
#' @usage NULL
GeomMathTypst <- ggplot2::ggproto(
  "GeomMathTypst",
  GeomTypst,
  extra_params = c("na.rm", "size.unit", "inline"),
  setup_data = function(data, params) {
    from_params <- is.null(data$label) && !is.null(params$label)
    if (from_params) {
      data$label <- rep(params$label, length.out = nrow(data))
    }

    if (is.null(data$label)) {
      return(GeomTypst$setup_data(data, params))
    }

    if (!from_params) {
      data$label <- normalize_math_labels(
        data$label,
        inline = isTRUE(params$inline),
        fn = "geom_math_typst",
        rows = seq_len(nrow(data))
      )
    }

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

normalize_math_labels <- function(label, inline, fn, rows = NULL) {
  normalize_math_label_values(
    label = label,
    inline = inline,
    fn = fn,
    rows = rows,
    kind = "typst",
    static_error = "Failed to normalize the static {.arg label} parameter in {.fn {fn}}.",
    mapped_error = "Failed to normalize a Typst math label in {.fn {fn}}."
  )
}

# geom_math_mitex

#' MiTeX-converted LaTeX math labels
#'
#' Draw LaTeX math labels at data positions by converting them to Typst via
#' MiTeX. Each `label` is treated as LaTeX math content; outer `$...$` or
#' `$$...$$` delimiters are optional and will be normalized before conversion.
#' See [convert_latex_to_typst()] for conversion details.
#'
#' Static layer parameters also accept `fontface` as an alias of `face`.
#' Math labels only support `face = "plain"` and `face = "bold"`.
#'
#' @inheritParams geom_typst
#' @param inline Whether to render labels as inline math. Default `FALSE`
#'   renders display-style math.
#' @return A ggplot2 layer that can be added to a plot.
#' @export
geom_math_mitex <- function(
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
    fn = "geom_math_mitex",
    supported = c("plain", "bold")
  )
  params$label <- normalize_mitex_labels(
    params$label,
    inline = inline,
    fn = "geom_math_mitex"
  )
  position <- resolve_typst_position(
    position,
    nudge_x = nudge_x,
    nudge_y = nudge_y,
    fn = "geom_math_mitex",
    position_supplied = !missing(position),
    nudge_supplied = !missing(nudge_x) || !missing(nudge_y)
  )

  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomMathMitex,
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

#' @rdname geom_math_mitex
#' @format NULL
#' @usage NULL
GeomMathMitex <- ggplot2::ggproto(
  "GeomMathMitex",
  GeomTypst,
  extra_params = c("na.rm", "size.unit", "inline"),

  setup_data = function(data, params) {
    from_params <- is.null(data$label) && !is.null(params$label)
    if (from_params) {
      data$label <- rep(params$label, length.out = nrow(data))
    }

    if (is.null(data$label)) {
      return(GeomTypst$setup_data(data, params))
    }

    # Static labels were already converted in geom_math_mitex();
    # only convert mapped labels from data.
    if (!from_params) {
      data$label <- normalize_mitex_labels(
        data$label,
        inline = isTRUE(params$inline),
        fn = "geom_math_mitex",
        rows = seq_len(nrow(data))
      )
    }

    data$face <- normalize_face_values(
      data$face,
      rows = seq_len(nrow(data)),
      fn = "geom_math_mitex",
      detail = "Math labels only support \"plain\" and \"bold\".",
      supported = c("plain", "bold")
    )

    data
  }
)

normalize_mitex_labels <- function(label, inline, fn, rows = NULL) {
  normalize_math_label_values(
    label = label,
    inline = inline,
    fn = fn,
    rows = rows,
    kind = "mitex",
    static_error = "Failed to convert the static {.arg label} parameter in {.fn {fn}}.",
    mapped_error = "Failed to convert a LaTeX math label in {.fn {fn}}.",
    static_call = rlang::call2(fn)
  )
}
