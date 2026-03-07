preamble_set <- c(
  r"(#set page(width: auto, height: auto, margin: 0.1em, fill: none))",
  r"(#set text(top-edge: "bounds", bottom-edge: "bounds"))"
)

test_that("build_typst_source wraps text with page defaults", {
  src <- build_typst_source(r"(Hello)")
  lines <- strsplit(src, "\n", fixed = TRUE)[[1]]

  expect_equal(lines, c(preamble_set, r"(Hello)"))
})

test_that("build_typst_source adds color, alpha, and size directives", {
  src <- build_typst_source(
    typst_code = r"(A)",
    color = "#33669980",
    alpha = 0.5,
    size = 12.5
  )
  lines <- strsplit(src, "\n", fixed = TRUE)[[1]]

  expect_equal(lines[1], preamble_set[1])
  expect_equal(lines[2], preamble_set[2])
  expect_equal(lines[3], '#set text(fill: rgb("#33669940"))')
  expect_equal(lines[4], "#set text(size: 12.5pt)")
  expect_equal(lines[5], r"(A)")
})

test_that("build_typst_source supports alpha-only style", {
  src <- build_typst_source(typst_code = r"(A)", alpha = 0.5)
  expect_match(src, '#set text\\(fill: rgb\\("#00000080"\\)\\)')
})

test_that("build_typst_source maps lineheight to Typst paragraph leading", {
  src <- build_typst_source(typst_code = "line1\nline2", lineheight = 1.25)
  lines <- strsplit(src, "\n", fixed = TRUE)[[1]]

  expect_equal(lines[1], preamble_set[1])
  expect_equal(lines[2], preamble_set[2])
  expect_equal(lines[3], "#set par(leading: 1.25em)")
  expect_equal(lines[4], "line1")
  expect_equal(lines[5], "line2")
})

test_that("build_typst_source allows negative lineheight", {
  src <- build_typst_source(typst_code = "A", lineheight = -0.25)
  lines <- strsplit(src, "\n", fixed = TRUE)[[1]]

  expect_equal(lines[3], "#set par(leading: -0.25em)")
  expect_equal(lines[4], "A")
})

test_that("build_typst_source preserves user text content", {
  text <- r"(line1
line2 {x} % y)"
  src <- build_typst_source(text, size = 11)
  lines <- strsplit(src, "\n", fixed = TRUE)[[1]]

  expect_equal(
    lines,
    c(preamble_set, "#set text(size: 11pt)", "line1", "line2 {x} % y")
  )
})

test_that("build_typst_source maps face to Typst text styles", {
  src <- build_typst_source("A", face = "bold.italic")
  lines <- strsplit(src, "\n", fixed = TRUE)[[1]]

  expect_equal(lines[1], preamble_set[1])
  expect_equal(lines[2], preamble_set[2])
  expect_equal(lines[3], '#set text(weight: "bold")')
  expect_equal(lines[4], '#set text(style: "italic")')
  expect_equal(lines[5], '#show math.equation: set text(weight: "bold")')
  expect_equal(lines[6], "A")
})

test_that("build_typst_source supports colour and fontface aliases", {
  src <- build_typst_source("A", colour = "#33669980", fontface = "bold.italic")
  lines <- strsplit(src, "\n", fixed = TRUE)[[1]]

  expect_equal(lines[3], '#set text(fill: rgb("#33669980"))')
  expect_equal(lines[4], '#set text(weight: "bold")')
  expect_equal(lines[5], '#set text(style: "italic")')
  expect_equal(lines[6], '#show math.equation: set text(weight: "bold")')
  expect_equal(lines[7], "A")
})

test_that("build_typst_source does not force italic styling onto math", {
  src <- build_typst_source("$ x $", face = "italic")
  lines <- strsplit(src, "\n", fixed = TRUE)[[1]]

  expect_equal(lines[1], preamble_set[1])
  expect_equal(lines[2], preamble_set[2])
  expect_equal(lines[3], '#set text(style: "italic")')
  expect_false(any(grepl("#show math\\.equation: set text\\(weight:", lines)))
  expect_equal(lines[4], "$ x $")
})

test_that("build_typst_source validates arguments", {
  expect_snapshot(build_typst_source(NULL), error = TRUE)
  expect_snapshot(build_typst_source("A", size = 0), error = TRUE)
  expect_snapshot(build_typst_source("A", alpha = 1.1), error = TRUE)
  expect_snapshot(build_typst_source("A", color = "not-a-color"), error = TRUE)
  expect_snapshot(build_typst_source("A", color = "red", colour = "blue"), error = TRUE)
  expect_snapshot(build_typst_source("A", face = "oblique"), error = TRUE)
  expect_snapshot(build_typst_source("A", face = "plain", fontface = "bold"), error = TRUE)
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

  expect_no_error(typst_svg(build_typst_source(src)))
})
