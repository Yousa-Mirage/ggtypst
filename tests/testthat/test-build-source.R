preamble_set <- c(
  r"(#set page(width: auto, height: auto, margin: 0pt, fill: none))",
  r"(#set text(top-edge: "cap-height", bottom-edge: "baseline"))"
)

# The rule that spaces baselines `1.2 * lineheight` font sizes apart.
leading_rule <- function(lineheight = "1.2") {
  c(
    "#show: body => context {",
    "  let cap-em = measure([H]).height / text.size",
    "  show math.equation: set text(top-edge: cap-em * 1em)",
    sprintf("  set par(leading: (%s * 1.2 - cap-em) * 1em)", lineheight),
    "  body",
    "}"
  )
}

source_lines <- function(...) {
  strsplit(build_typst_source(...), "\n", fixed = TRUE)[[1]]
}

test_that("build_typst_source wraps text with page defaults", {
  expect_equal(
    source_lines(r"(Hello)"),
    c(preamble_set, leading_rule(), r"(Hello)")
  )
})

test_that("build_typst_source adds color, alpha, and size directives", {
  lines <- source_lines(
    typst_code = r"(A)",
    color = "#33669980",
    alpha = 0.5,
    size = 12.5
  )

  expect_equal(
    lines,
    c(
      preamble_set,
      '#set text(fill: rgb("#33669940"))',
      "#set text(size: 12.5pt)",
      leading_rule(),
      r"(A)"
    )
  )
})

test_that("build_typst_source supports alpha-only style", {
  src <- build_typst_source(typst_code = r"(A)", alpha = 0.5)
  expect_match(src, '#set text\\(fill: rgb\\("#00000080"\\)\\)')
})

test_that("build_typst_source maps lineheight to the baseline distance", {
  lines <- source_lines(typst_code = "line1\nline2", lineheight = 1.25)
  expect_equal(lines, c(preamble_set, leading_rule("1.25"), "line1", "line2"))
})

test_that("build_typst_source allows negative lineheight", {
  lines <- source_lines(typst_code = "A", lineheight = -0.25)
  expect_equal(lines, c(preamble_set, leading_rule("-0.25"), "A"))
})

test_that("build_typst_source preserves user text content", {
  text <- r"(line1
line2 {x} % y)"

  expect_equal(
    source_lines(text, size = 11),
    c(
      preamble_set,
      "#set text(size: 11pt)",
      leading_rule(),
      "line1",
      "line2 {x} % y"
    )
  )
})

test_that("build_typst_source maps face to Typst text styles", {
  expect_equal(
    source_lines("A", face = "bold.italic"),
    c(
      preamble_set,
      '#set text(weight: "bold")',
      '#set text(style: "italic")',
      '#show math.equation: set text(weight: "bold")',
      leading_rule(),
      "A"
    )
  )
})

test_that("build_typst_source supports colour and fontface aliases", {
  expect_equal(
    source_lines("A", colour = "#33669980", fontface = "bold.italic"),
    c(
      preamble_set,
      '#set text(fill: rgb("#33669980"))',
      '#set text(weight: "bold")',
      '#set text(style: "italic")',
      '#show math.equation: set text(weight: "bold")',
      leading_rule(),
      "A"
    )
  )
})

test_that("build_typst_source does not force italic styling onto math", {
  lines <- source_lines("$ x $", face = "italic")

  expect_equal(
    lines,
    c(preamble_set, '#set text(style: "italic")', leading_rule(), "$ x $")
  )
  expect_false(any(grepl("#show math\\.equation: set text\\(weight:", lines)))
})

test_that("build_typst_source negates angle to match grid semantics", {
  lines <- source_lines("A", angle = 45)
  expect_equal(lines[length(lines)], "#rotate(-45deg, reflow: true)[A]")
})

test_that("build_typst_source maps hjust to the nearest Typst alignment", {
  align_of <- function(hjust) {
    src <- build_typst_source("A", hjust = hjust)
    regmatches(src, regexpr("#set align\\([a-z]+\\)", src))
  }

  expect_equal(align_of(0), "#set align(left)")
  expect_equal(align_of(0.2), "#set align(left)")
  expect_equal(align_of(0.5), "#set align(center)")
  expect_equal(align_of(0.8), "#set align(right)")
  expect_equal(align_of(1), "#set align(right)")
  expect_false(grepl("#set align", build_typst_source("A"), fixed = TRUE))

  # The user's own alignment comes after ours, so it wins.
  src <- build_typst_source("#align(right)[A]", hjust = 0)
  expect_lt(
    regexpr("#set align(left)", src, fixed = TRUE),
    regexpr("#align(right)", src, fixed = TRUE)
  )
})

test_that("build_typst_source validates arguments", {
  expect_snapshot(build_typst_source(NULL), error = TRUE)
  expect_snapshot(build_typst_source("A", size = 0), error = TRUE)
  expect_snapshot(build_typst_source("A", alpha = 1.1), error = TRUE)
  expect_snapshot(build_typst_source("A", color = "not-a-color"), error = TRUE)
  expect_snapshot(
    build_typst_source("A", color = "red", colour = "blue"),
    error = TRUE
  )
  expect_snapshot(build_typst_source("A", face = "oblique"), error = TRUE)
  expect_snapshot(
    build_typst_source("A", face = "plain", fontface = "bold"),
    error = TRUE
  )
})

test_that("convert_latex_to_typst returns mitex-ready typst code", {
  expect_type(rs_convert_latex_to_typst(r"(\frac{1}{2})"), "character")

  src <- convert_latex_to_typst(r"(\frac{1}{2} + \sqrt{3})")

  expect_type(src, "character")
  expect_length(src, 1)
  expect_match(src, r"(#import "/specs/mod.typ": mitex-scope)", fixed = TRUE)
  expect_match(
    src,
    r"(#let _ggtypst_mitex_expr = ")",
    fixed = TRUE
  )
  expect_match(
    src,
    r"(#eval("$ " + _ggtypst_mitex_expr + " $", scope: mitex-scope))",
    fixed = TRUE
  )
})

test_that("convert_latex_to_typst normalizes outer dollar wrappers", {
  bare <- convert_latex_to_typst(r"(\frac{1}{2})")
  single <- convert_latex_to_typst(r"($ \frac{1}{2} $)")
  double <- convert_latex_to_typst(r"($$ \frac{1}{2} $$)")

  expect_equal(single, bare)
  expect_equal(double, bare)
})

test_that("convert_latex_to_typst reports mitex conversion errors", {
  expect_snapshot(convert_latex_to_typst(r"(\end{})"), error = TRUE)
})

test_that("convert_latex_to_typst supports matrix environments without alias", {
  src <- convert_latex_to_typst(
    r"(\begin{pmatrix}1 & 2 \\ 3 & 4\end{pmatrix})"
  )

  expect_no_error(typst_render(build_typst_source(src)))
})
