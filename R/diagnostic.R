escape_cli <- function(x) {
  x <- gsub("{", "{{", x, fixed = TRUE)
  x <- gsub("}", "}}", x, fixed = TRUE)
  x
}

warn_typst_warnings <- function(warnings) {
  bullets <- c("Typst emitted warnings during rendering.")

  diag_bullets <- unlist(
    lapply(warnings, function(diagnostic) {
      bullet <- if (identical(diagnostic$severity, "Warning")) "!" else "x"
      msg <- setNames(escape_cli(diagnostic$message), bullet)

      hints <- diagnostic$hints
      if (length(hints) > 0) {
        msg <- c(msg, setNames("{.strong Hint:}", "i"))
        msg <- c(msg, setNames(escape_cli(hints), rep(" ", length(hints))))
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

abort_typst_error <- function(err) {
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
        msg <- setNames(msg_text, bullet)

        hints <- diagnostic$hints
        if (length(hints) > 0) {
          msg <- c(msg, setNames("{.strong Hint:}", "i"))
          hints_text <- escape_cli(hints)
          hints_named <- setNames(hints_text, rep(" ", length(hints)))
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
