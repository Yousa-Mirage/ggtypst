#' Validate a scalar character input
#'
#' @param x Value to validate.
#' @param arg Argument name used in error messages.
#' @param allow_null Whether `NULL` is accepted.
#' @return `x` unchanged when valid.
#' @noRd
check_single_string <- function(x, arg, allow_null = TRUE) {
  if (allow_null && is.null(x)) {
    return(NULL)
  }
  if (!is.character(x) || length(x) != 1 || is.na(x)) {
    cli::cli_abort("{.arg {arg}} must be a single non-missing string.")
  }
  x
}

#' Validate a positive numeric scalar
#'
#' @param x Value to validate.
#' @param arg Argument name used in error messages.
#' @param allow_null Whether `NULL` is accepted.
#' @return `x` unchanged when valid.
#' @noRd
check_positive_number <- function(x, arg, allow_null = TRUE) {
  if (allow_null && is.null(x)) {
    return(NULL)
  }
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || !is.finite(x) || x <= 0) {
    cli::cli_abort("{.arg {arg}} must be a single positive finite number.")
  }
  x
}

#' Validate a finite numeric scalar
#'
#' @param x Value to validate.
#' @param arg Argument name used in error messages.
#' @param allow_null Whether `NULL` is accepted.
#' @return `x` unchanged when valid.
#' @noRd
check_number <- function(x, arg, allow_null = TRUE) {
  if (allow_null && is.null(x)) {
    return(NULL)
  }
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || !is.finite(x)) {
    cli::cli_abort("{.arg {arg}} must be a single finite number.")
  }
  x
}

#' Validate a scalar logical input
#'
#' @param x Value to validate.
#' @param arg Argument name used in error messages.
#' @param allow_null Whether `NULL` is accepted.
#' @return `x` unchanged when valid.
#' @noRd
check_bool <- function(x, arg, allow_null = TRUE) {
  if (allow_null && is.null(x)) {
    return(NULL)
  }
  if (!is.logical(x) || length(x) != 1 || is.na(x)) {
    cli::cli_abort("{.arg {arg}} must be TRUE or FALSE.")
  }
  x
}

#' Validate a supported size unit
#'
#' @param size.unit Size unit name.
#' @param arg Argument name used in error messages.
#' @return The validated size unit.
#' @noRd
check_size_unit <- function(size.unit, arg = "size.unit") {
  if (
    !is.character(size.unit) ||
      length(size.unit) != 1 ||
      is.na(size.unit) ||
      !(size.unit %in% c("pt", "mm"))
  ) {
    cli::cli_abort("{.arg {arg}} must be one of \"pt\" or \"mm\".")
  }

  size.unit
}

#' Resolve an argument alias
#'
#' @param value Canonical argument value.
#' @param alias_value Alias argument value.
#' @param arg Canonical argument name.
#' @param alias Alias argument name.
#' @return The resolved value.
#' @noRd
resolve_arg_alias <- function(value, alias_value, arg, alias_arg) {
  if (!is.null(value) && !is.null(alias_value)) {
    cli::cli_abort(c(
      "Can't supply both {.arg {arg}} and {.arg {alias_arg}}.",
      "i" = "Use {.arg {arg}}; {.arg {alias_arg}} is an alias."
    ))
  }

  if (is.null(value)) {
    alias_value
  } else {
    value
  }
}

#' Validate a supported text face
#'
#' @param face Face name or ggplot-style numeric code.
#' @param arg Argument name used in error messages.
#' @param allow_null Whether `NULL` is accepted.
#' @return The normalized face name.
#' @noRd
normalize_face <- function(face, arg = "face", allow_null = TRUE) {
  if (is.null(face) || length(face) == 0 || (length(face) == 1 && is.na(face))) {
    if (allow_null) return(NULL)
  } else if (length(face) == 1) {
    if (is.numeric(face) && (face %in% 1:4)) {
      # Fast-path genuine numeric ggplot2 face codes before string normalization.
      return(c("plain", "bold", "italic", "bold.italic")[[face]])
    } else {
      normalized <- gsub("[[:space:]_.-]+", "", tolower(as.character(face)))

      out <- switch(
        normalized,
        `1` = ,
        plain = "plain",
        `2` = ,
        bold = "bold",
        `3` = ,
        italic = "italic",
        `4` = ,
        bolditalic = "bold.italic"
      )

      if (!is.null(out)) {
        return(out)
      }
    }
  }

  cli::cli_abort(
    "{.arg {arg}} must be one of \"plain\", \"bold\", \"italic\", or \"bold.italic\" (or numeric codes 1-4)."
  )
}

#' Validate an alpha multiplier
#'
#' @param alpha Optional numeric alpha value in `[0, 1]`.
#' @return `alpha` unchanged when valid.
#' @noRd
check_alpha <- function(alpha) {
  if (is.null(alpha)) {
    return(NULL)
  }

  if (
    !is.numeric(alpha) ||
      length(alpha) != 1 ||
      is.na(alpha) ||
      !is.finite(alpha) ||
      alpha < 0 ||
      alpha > 1
  ) {
    cli::cli_abort("{.arg alpha} must be NULL or a single number in [0, 1].")
  }

  alpha
}

#' Convert a color to Typst-compatible RGBA hex
#'
#' @param color A color understood by [grDevices::col2rgb()].
#' @param alpha Optional alpha multiplier in `[0, 1]`.
#' @return A single `#RRGGBBAA` string.
#' @noRd
color_to_hex <- function(color, alpha = NULL) {
  rgba <- grDevices::col2rgb(color, alpha = TRUE)[, 1]

  if (!is.null(alpha)) {
    rgba[4] <- round(rgba[4] * alpha)
  }

  sprintf("#%02X%02X%02X%02X", rgba[1], rgba[2], rgba[3], rgba[4])
}

#' Format numeric values for Typst source
#'
#' @param x A numeric scalar.
#' @return A compact decimal string without trailing zeros.
#' @noRd
format_typst_number <- function(x) {
  out <- sprintf("%.6f", x)
  out <- sub("\\.?0+$", "", out)
  out
}

#' Convert text size to Typst points
#'
#' @param size Size value to convert.
#' @param size.unit Unit of `size`, either `"pt"` or `"mm"`.
#' @param arg Argument name used in error messages.
#' @return A size in Typst points or `NULL`.
#' @noRd
convert_size_to_pt <- function(size, size.unit = "pt", arg = "size") {
  size.unit <- check_size_unit(size.unit, arg = "size.unit")
  size <- check_positive_number(size, arg = arg)

  if (is.null(size)) {
    NULL
  } else if (size.unit == "mm") {
    size * ggplot2::.pt
  } else {
    size
  }
}

#' Normalize an optional scalar number
#'
#' @param x Value to normalize.
#' @return A scalar number or `NULL`.
#' @noRd
normalize_optional_number <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return(NULL)
  }

  as.numeric(x)
}

#' Normalize an optional scalar string
#'
#' @param x Value to normalize.
#' @param empty_is_null Whether `""` should be treated as `NULL`.
#' @return A scalar string or `NULL`.
#' @noRd
normalize_optional_string <- function(x, empty_is_null = FALSE) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return(NULL)
  }

  x <- as.character(x)
  if (empty_is_null && identical(x, "")) {
    return(NULL)
  }

  x
}

#' Normalize math labels for Typst- and MiTeX-backed APIs
#'
#' @param label Label vector.
#' @param inline Whether to use inline math delimiters.
#' @param fn Calling function name for error messages.
#' @param rows Optional row indices for mapped aesthetics.
#' @param kind Either `"typst"` or `"mitex"`.
#' @param static_error Error header for static labels.
#' @param mapped_error Error header for mapped labels.
#' @param static_call Optional call object for static-label errors.
#' @param preserve_blank Whether blank strings should be preserved unchanged.
#' @return A character vector with normalized math labels.
#' @noRd
normalize_math_label_values <- function(
  label,
  inline,
  fn,
  rows = NULL,
  kind,
  static_error,
  mapped_error,
  static_call = NULL,
  preserve_blank = FALSE
) {
  if (is.null(label) || length(label) == 0) {
    return(label)
  }

  label <- as.character(label)
  valid <- !is.na(label)
  if (preserve_blank) {
    valid <- valid & !grepl("^\\s*$", label)
  }

  if (!any(valid)) {
    return(label)
  }

  labels <- label[valid]
  unique_labels <- unique(labels)

  first_rows <- NULL
  if (!is.null(rows)) {
    first_rows <- rows[valid][match(unique_labels, labels)]
  }

  wrapped <- vapply(
    seq_along(unique_labels),
    function(i) {
      label_value <- unique_labels[[i]]

      preview <- trimws(label_value)
      if (nchar(preview) > 80) {
        preview <- paste0(substr(preview, 1, 77), "...")
      }

      tryCatch(
        switch(
          kind,
          typst = as_typst_math_code(label_value, inline = inline),
          mitex = convert_latex_to_typst(label_value, inline = inline)
        ),
        error = function(cnd) {
          if (is.null(first_rows)) {
            cli::cli_abort(
              c(
                static_error,
                "i" = "Label: {.val {preview}}"
              ),
              parent = cnd,
              call = static_call
            )
          }

          cli::cli_abort(
            c(
              mapped_error,
              "x" = "Problem in row {first_rows[[i]]}.",
              "i" = "Label: {.val {preview}}"
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

#' Map a normalized face to Typst text settings
#'
#' @param face A normalized face string.
#' @return A character vector of Typst commands.
#' @noRd
face_to_typst_styles <- function(face) {
  # Keep math styling conservative: Typst math text only inherits bold here.
  # Italic is left to Typst's default math styling instead of being forced.
  switch(
    face,
    plain = character(0),
    bold = c(
      '#set text(weight: "bold")',
      '#show math.equation: set text(weight: "bold")'
    ),
    italic = c('#set text(style: "italic")'),
    `bold.italic` = c(
      '#set text(weight: "bold")',
      '#set text(style: "italic")',
      '#show math.equation: set text(weight: "bold")'
    )
  )
}

#' Resolve character justifications against panel coordinates
#'
#' @param just Character justifications.
#' @param axis Numeric coordinates after transformation.
#' @return Numeric justification values.
#' @noRd
compute_just <- function(just, axis) {
  inward <- just == "inward"
  just[inward] <- c("left", "middle", "right")[just_dir(axis[inward])]

  outward <- just == "outward"
  just[outward] <- c("right", "middle", "left")[just_dir(axis[outward])]

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
just_dir <- function(axis, tol = 0.001) {
  out <- rep(2L, length(axis))
  out[axis < 0.5 - tol] <- 1L
  out[axis > 0.5 + tol] <- 3L
  out
}

#' Remove outer math dollar delimiters
#'
#' @param typst_code A single math code string.
#' @return The trimmed inner math code when wrapped in outer `$...$` or
#'   `$$...$$`, otherwise the trimmed input.
#' @noRd
unwrap_math_dollar_delimiters <- function(typst_code) {
  trimmed <- trimws(typst_code)
  n <- nchar(trimmed)

  if (n < 2L) {
    return(trimmed)
  }
  if (n >= 4L && startsWith(trimmed, "$$") && endsWith(trimmed, "$$")) {
    return(trimws(substr(trimmed, 3L, n - 2L)))
  }
  if (startsWith(trimmed, "$") && endsWith(trimmed, "$")) {
    return(trimws(substr(trimmed, 2L, n - 1L)))
  }

  trimmed
}

#' Reject unescaped dollar signs inside math code
#'
#' @param core A math code string with outer delimiters already removed.
#' @param arg Argument name used in error messages.
#' @return `core` unchanged when valid.
#' @noRd
validate_no_unescaped_dollar <- function(core, arg = "math_code") {
  if (grepl("(?<!\\\\)\\$", core, perl = TRUE)) {
    cli::cli_abort(c(
      "Invalid math input: Unexpected unescaped `$` found inside the code.",
      "x" = "Offending argument: {.arg {arg}}.",
      "i" = "If you want to use a literal dollar sign, escape it as `\\$`."
    ))
  }

  core
}

#' Normalize LaTeX math input
#'
#' @param latex_code A single LaTeX math string.
#' @return The normalized inner LaTeX math code.
#' @noRd
as_latex_math_code <- function(latex_code) {
  latex_code <- check_single_string(latex_code, "latex_math_code", allow_null = FALSE)
  core <- unwrap_math_dollar_delimiters(latex_code)
  validate_no_unescaped_dollar(core, arg = "latex_math_code")
}

#' Normalize Typst math input and apply delimiters
#'
#' @param typst_code A single Typst math string.
#' @param inline Whether to wrap as inline math.
#' @return Typst math code wrapped with normalized outer dollar delimiters.
#' @noRd
as_typst_math_code <- function(typst_code, inline = FALSE) {
  typst_code <- check_single_string(typst_code, "typst_math_code", allow_null = FALSE)
  inline <- check_bool(inline, "inline", allow_null = FALSE)
  core <- unwrap_math_dollar_delimiters(typst_code)
  validate_no_unescaped_dollar(core, arg = "typst_math_code")

  if (inline) {
    sprintf("$%s$", core)
  } else {
    sprintf("$ %s $", core)
  }
}

#' Rotate justification values for angled text
#'
#' Computes rotated default positions so that `x` and `y` fall on the correct
#' anchor point when the Typst content is rotated.  Ported from ggtext / ggplot2.
#'
#' @param angle Rotation angle in degrees.
#' @param hjust,vjust Original justification values.
#' @return A list with `$hjust` and `$vjust`.
#' @noRd
rotate_just <- function(angle, hjust, vjust) {
  angle <- (angle %||% 0) %% 360
  q <- (angle %/% 90) + 1L # 1=[0,90) 2=[90,180) 3=[180,270) 4=[270,360)

  list(
    hjust = c(hjust, 1 - vjust, 1 - hjust, vjust)[q],
    vjust = c(vjust, hjust, 1 - vjust, 1 - hjust)[q]
  )
}
