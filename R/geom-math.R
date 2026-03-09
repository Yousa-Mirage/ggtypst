#' @include geom-typst.R
NULL

# geom_math_typst

#' Plot Labels in Data with Typst Math
#'
#' `geom_math_typst()` is the math-specialized variant of [geom_typst()] that
#' renders data-driven Typst math labels. Each `label` is treated as Typst math
#' expression. It will normalize the outer `$...$` or `$ ... $` delimiters
#' automatically.
#'
#' @inheritParams geom_typst
#' @param inline Whether to render inline math. Default `FALSE` renders
#'   display-style math.
#' @return A ggplot2 layer that can be added to a plot.
#' @seealso [geom_typst()], [geom_math_mitex()], [ggplot2::geom_label()]
#' @examples
#' math_labels <- data.frame(
#'   wt = c(2.1, 3.2, 4.8),
#'   mpg = c(32, 24, 16),
#'   label = c(r"(sum_(i=1)^n x_i)", r"(x^2 + y^2)", r"(integral_0^1 x dif x)")
#' )
#'
#' ggplot(math_labels, aes(wt, mpg, label = label)) +
#'   geom_point() +
#'   geom_math_typst(nudge_y = 1, size = 14, show.legend = FALSE) +
#'   theme_minimal()
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
  params <- list(...)
  params <- normalize_colour_param(params)
  params <- normalize_face_param(
    params,
    fn = "geom_math_typst",
    supported = c("plain", "bold")
  )
  params$label <- normalize_math_label_values(
    label = params$label,
    inline = inline,
    fn = "geom_math_typst",
    kind = "typst",
    static_error = "Failed to normalize the static {.arg label} parameter in {.fn {fn}}.",
    mapped_error = "Failed to normalize a Typst math label in {.fn {fn}}."
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

# Shared setup_data logic for math geoms.
# Handles label propagation from params, math label normalization,
# and face validation -- identical for typst and mitex backends.
setup_data_math <- function(data, params, kind, fn) {
  from_params <- is.null(data$label) && !is.null(params$label)
  if (from_params) {
    data$label <- rep(params$label, length.out = nrow(data))
  }

  if (is.null(data$label)) {
    return(GeomTypst$setup_data(data, params))
  }

  # Static labels were already normalized/converted in the geom constructor;
  # only process mapped labels from data.
  if (!from_params) {
    verb <- if (kind == "typst") "normalize" else "convert"
    noun <- if (kind == "typst") "Typst" else "LaTeX"

    data$label <- normalize_math_label_values(
      label = data$label,
      inline = isTRUE(params$inline),
      fn = fn,
      rows = seq_len(nrow(data)),
      kind = kind,
      static_error = sprintf(
        "Failed to %s the static {.arg label} parameter in {.fn {fn}}.",
        verb
      ),
      mapped_error = sprintf(
        "Failed to %s a %s math label in {.fn {fn}}.",
        verb,
        noun
      ),
      static_call = if (kind == "mitex") rlang::call2(fn)
    )
  }

  data$face <- normalize_face_values(
    data$face,
    rows = seq_len(nrow(data)),
    fn = fn,
    detail = "Math labels only support \"plain\" and \"bold\".",
    supported = c("plain", "bold")
  )

  data
}

#' @rdname geom_math_typst
#' @format NULL
#' @usage NULL
GeomMathTypst <- ggplot2::ggproto(
  "GeomMathTypst",
  GeomTypst,
  extra_params = c("na.rm", "size.unit", "inline"),
  setup_data = function(data, params) {
    setup_data_math(data, params, kind = "typst", fn = "geom_math_typst")
  }
)

# geom_math_mitex

#' Plot Labels in Data with MiTeX-Converted LaTeX Math
#'
#' @description
#' `geom_math_mitex()` is the LaTeX-input companion to [geom_math_typst()] that
#' renders data-driven LaTeX-style math labels by converting each `label` to Typst
#' through [MiTeX](https://github.com/mitex-rs/mitex) before rendering. It will
#' normalize the outer `$...$` or `$$...$$` delimiters automatically.
#'
#' This allows you to use LaTeX math syntax in your plot, which you may be more familiar with :).
#'
#' @details
#' [MiTeX](https://github.com/mitex-rs/mitex) is a LaTeX-to-Typst converter. It should be stable and reliable for typical LaTeX math expressions. If you find any LaTeX math that MiTeX fails to convert properly, you can report an issue to `ggtypst` first.
#'
#' @inheritParams geom_typst
#' @param inline Whether to render inline math. Default `FALSE` renders
#'   display-style math.
#' @return A ggplot2 layer that can be added to a plot.
#' @seealso [geom_typst()], [geom_math_typst()], [ggplot2::geom_label()]
#' @examples
#' math_labels <- data.frame(
#'   wt = c(2.1, 3.2, 4.8),
#'   mpg = c(32, 24, 16),
#'   label = c(
#'     r"(\sum_{i=1}^n x_i)",
#'     r"(x^2 + y^2)",
#'     r"(\int_0^1 x \, dx)"
#'   )
#' )
#'
#' ggplot(math_labels, aes(wt, mpg, label = label)) +
#'   geom_point() +
#'   geom_math_mitex(nudge_y = 1, size = 14, show.legend = FALSE) +
#'   theme_minimal()
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
  params <- list(...)
  params <- normalize_colour_param(params)
  params <- normalize_face_param(
    params,
    fn = "geom_math_mitex",
    supported = c("plain", "bold")
  )
  params$label <- normalize_math_label_values(
    label = params$label,
    inline = inline,
    fn = "geom_math_mitex",
    kind = "mitex",
    static_error = "Failed to convert the static {.arg label} parameter in {.fn {fn}}.",
    mapped_error = "Failed to convert a LaTeX math label in {.fn {fn}}.",
    static_call = rlang::call2("geom_math_mitex")
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
    setup_data_math(data, params, kind = "mitex", fn = "geom_math_mitex")
  }
)
