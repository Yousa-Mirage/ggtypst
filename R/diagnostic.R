#' Escape cli inline markup in plain text
#'
#' Doubles curly braces so arbitrary diagnostic text can be passed through
#' `cli` bullet formatting without being interpreted as markup.
#'
#' @param x A character vector.
#' @return A character vector with escaped braces.
#' @noRd
escape_cli <- function(x) {
  x <- gsub("{", "{{", x, fixed = TRUE)
  x <- gsub("}", "}}", x, fixed = TRUE)
  x
}

#' Signal Typst warnings with cli formatting
#'
#' Formats structured Typst warnings into a single `cli` warning condition while
#' preserving the original diagnostics on the condition object.
#'
#' @param warnings A list of Typst diagnostic objects.
#' @return Called for its side effect of signaling a warning.
#' @noRd
warn_typst_warnings <- function(warnings) {
  bullets <- c("Typst emitted warnings during rendering.")

  diag_bullets <- unlist(
    lapply(warnings, function(diagnostic) {
      bullet <- if (identical(diagnostic$severity, "Warning")) "!" else "x"
      msg <- stats::setNames(escape_cli(diagnostic$message), bullet)

      hints <- diagnostic$hints
      if (length(hints) > 0) {
        msg <- c(msg, stats::setNames("{.strong Hint:}", "i"))
        msg <- c(msg, stats::setNames(escape_cli(hints), rep(" ", length(hints))))
      }

      msg
    })
  )

  bullets <- c(bullets, diag_bullets)

  cli::cli_warn(
    bullets,
    class = c("ggtypst_typst_warning", "typst_warning"),
    typst_warnings = warnings,
    call = rlang::caller_env()
  )
}

#' Abort with a structured Typst error
#'
#' Converts a structured Typst error object into a user-facing `cli` error with
#' formatted diagnostics and hints.
#'
#' @param err A Typst error object returned from the Rust backend.
#' @return Called for its side effect of signaling an error.
#' @noRd
abort_typst_errors <- function(err) {
  kind <- err$kind

  if (kind == "EmptySvg") {
    cli::cli_abort(
      "Typst rendered an empty SVG. Check your input.",
      class = c("ggtypst_typst_error", "typst_error"),
      typst_error = err,
      call = rlang::caller_env()
    )
  }

  diagnostics <- err$diagnostics
  bullets <- escape_cli(err$message)

  if (is.list(diagnostics) && length(diagnostics) > 0) {
    diag_bullets <- unlist(
      lapply(diagnostics, function(diagnostic) {
        bullet <- if (identical(diagnostic$severity, "Warning")) "!" else "x"

        msg_text <- escape_cli(diagnostic$message)
        msg <- stats::setNames(msg_text, bullet)

        hints <- diagnostic$hints
        if (length(hints) > 0) {
          msg <- c(msg, stats::setNames("{.strong Hint:}", "i"))
          hints_text <- escape_cli(hints)
          hints_named <- stats::setNames(hints_text, rep(" ", length(hints)))
          msg <- c(msg, hints_named)
        }

        msg
      })
    )

    bullets <- c(bullets, diag_bullets)
  }

  cli::cli_abort(
    bullets,
    class = c("ggtypst_typst_error", "typst_error"),
    typst_error = err,
    call = rlang::caller_env()
  )
}
