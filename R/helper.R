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
