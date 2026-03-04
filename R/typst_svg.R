#' Render Typst Source to SVG Bytes
#'
#' Compiles Typst code and returns SVG bytes together with rendered dimensions.
#' If Typst compilation fails, this function throws a structured CLI error with
#' diagnostics and hints.
#'
#' @param text Typst source code to compile.
#' @return A list with fields `svg`, `width_pt`, and `height_pt`.
#' @export
typst_svg <- function(text) {
  result <- typst_svg_impl(text)
  if (inherits(result, "typst_error")) {
    abort_typst_error(result)
  }
  result
}

escape_cli <- function(x) {
  x <- gsub("{", "{{", x, fixed = TRUE)
  x <- gsub("}", "}}", x, fixed = TRUE)
  x
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
    # 使用 lapply 向量化处理，避免 for 循环中不断扩张向量
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
