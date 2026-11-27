#' List the Fonts Typst Can Use
#'
#' `typst_fonts()` lists the fonts Typst can use in ggtypst: the
#' fonts built into ggtypst and the fonts installed on the system. Use the
#' names in its `family` column for `family` and `math_family` arguments, and
#' in Typst code such as `#set text(font: "...")`.
#'
#' Typst names a font by its typographic family, which can differ from the name
#' other software shows. On Windows, for example, R and
#' [systemfonts](https://systemfonts.r-lib.org) may report
#' `"NewComputerModern Math"` for the font Typst calls
#' `"New Computer Modern Math"`. Typst reads font files directly, so variants
#' registered with `systemfonts::register_variant()` are not available to it.
#'
#' @param family Optional regular expression. If given, only families matching
#'   it (ignoring case) are listed.
#' @param detail If `FALSE` (the default), list each family once. If `TRUE`,
#'   list each font of a family, with its weight, style, and width.
#' @return A data frame sorted by family name. By default it has one row per
#'   family and columns:
#'   - `family`: the family name Typst uses;
#'   - `variable`: whether any of its fonts is a variable font (see
#'     [annotate_typst()]);
#'   - `math`: whether any of its fonts can typeset math, so it can be a
#'     `math_family`.
#'
#'   With `detail = TRUE`, it has one row per font, `variable` and `math` refer
#'   to that font, and it also has the columns:
#'   - `weight`: the weight, from 100 (thin) to 900 (black);
#'   - `style`: `"normal"`, `"italic"`, or `"oblique"`;
#'   - `stretch`: the width, in percent of the normal width.
#' @seealso The `family`, `math_family`, and `fontweight` arguments of
#'   [annotate_typst()], [geom_typst()], and [element_typst()].
#'
#' @examples
#' # Families that can typeset math
#' fonts <- typst_fonts()
#' fonts$family[fonts$math]
#'
#' # The weights and styles of a family
#' typst_fonts("^Libertinus Serif$", detail = TRUE)
#' @export
typst_fonts <- function(family = NULL, ..., detail = FALSE) {
  rlang::check_dots_empty()

  family <- check_single_string(family, "family")
  detail <- check_bool(detail, "detail", allow_null = FALSE)

  fonts <- unique(as.data.frame(rs_typst_fonts()))

  if (!is.null(family)) {
    fonts <- fonts[grepl(family, fonts$family, ignore.case = TRUE), ]
  }

  if (detail) {
    fonts <- fonts[
      order(
        tolower(fonts$family),
        fonts$style != "normal",
        fonts$weight,
        fonts$stretch
      ),
    ]
  } else {
    families <- unique(fonts$family)
    fonts <- data.frame(
      family = families,
      variable = families %in% fonts$family[fonts$variable],
      math = families %in% fonts$family[fonts$math]
    )
    fonts <- fonts[order(tolower(fonts$family)), ]
  }

  rownames(fonts) <- NULL
  fonts
}

#' Hints for families Typst doesn't know
#'
#' @param warnings Typst diagnostics.
#' @return Named `cli` bullets: the family Typst knows that differs from a
#'   missing one only in spaces, punctuation, or case, and a pointer to
#'   [typst_fonts()].
#' @noRd
unknown_family_hints <- function(warnings) {
  prefix <- "unknown font family: "
  messages <- vapply(warnings, function(d) d$message %||% "", "")
  missing <- substring(
    messages[startsWith(messages, prefix)],
    nchar(prefix) + 1
  )
  if (length(missing) == 0) {
    return(character())
  }

  squash <- function(x) gsub("[^[:alnum:]]", "", tolower(x))
  families <- unique(rs_typst_fonts()$family)

  # Typst reports the missing family in lower case.
  known <- families[match(squash(unique(missing)), squash(families))]
  known <- known[!is.na(known)]

  c(
    stats::setNames(
      sprintf("Did you mean {.val %s}?", escape_cli(known)),
      rep("i", length(known))
    ),
    i = "Run {.run ggtypst::typst_fonts()} to see the font families Typst can use."
  )
}
