check_single_string <- function(x, arg, allow_null = TRUE) {
  if (allow_null && is.null(x)) {
    return(NULL)
  }
  if (!is.character(x) || length(x) != 1 || is.na(x)) {
    cli::cli_abort("{.arg {arg}} must be a single non-missing string.")
  }
  x
}

check_positive_number <- function(x, arg, allow_null = TRUE) {
  if (allow_null && is.null(x)) {
    return(NULL)
  }
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || !is.finite(x) || x <= 0) {
    cli::cli_abort("{.arg {arg}} must be a single positive finite number.")
  }
  x
}

check_number <- function(x, arg, allow_null = TRUE) {
  if (allow_null && is.null(x)) {
    return(NULL)
  }
  if (!is.numeric(x) || length(x) != 1 || is.na(x) || !is.finite(x)) {
    cli::cli_abort("{.arg {arg}} must be a single finite number.")
  }
  x
}

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

color_to_hex <- function(color, alpha = NULL) {
  rgba <- grDevices::col2rgb(color, alpha = TRUE)[, 1]

  if (!is.null(alpha)) {
    rgba[4] <- round(rgba[4] * alpha)
  }

  sprintf("#%02X%02X%02X%02X", rgba[1], rgba[2], rgba[3], rgba[4])
}

format_typst_number <- function(x) {
  out <- sprintf("%.6f", x)
  out <- sub("\\.?0+$", "", out)
  out
}

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

as_latex_math_code <- function(latex_code) {
  core <- unwrap_math_dollar_delimiters(latex_code)
  validate_no_unescaped_dollar(core, arg = "latex_code")
}

as_typst_math_code <- function(typst_code) {
  core <- unwrap_math_dollar_delimiters(typst_code)
  validate_no_unescaped_dollar(core, arg = "typst_code")

  sprintf("$ %s $", core)
}
