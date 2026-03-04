page_line <- "#set page(width: auto, height: auto, margin: 0.2em, fill: none)"

test_that("build_typst_source wraps text with page defaults", {
  src <- build_typst_source(r"(Hello)")
  lines <- strsplit(src, "\n", fixed = TRUE)[[1]]

  expect_equal(lines, c(page_line, r"(Hello)"))
})

test_that("build_typst_source adds color, alpha, and size directives", {
  src <- build_typst_source(
    text = r"(A)",
    color = "#33669980",
    alpha = 0.5,
    size = 12.5
  )
  lines <- strsplit(src, "\n", fixed = TRUE)[[1]]

  expect_equal(lines[1], page_line)
  expect_equal(lines[2], '#set text(fill: rgb("#33669940"))')
  expect_equal(lines[3], "#set text(size: 12.5pt)")
  expect_equal(lines[4], r"(A)")
})

test_that("build_typst_source supports alpha-only style", {
  src <- build_typst_source(text = r"(A)", alpha = 0.5)
  expect_match(src, '#set text\\(fill: rgb\\("#00000080"\\)\\)')
})

test_that("build_typst_source preserves user text content", {
  text <- r"(line1
line2 {x} % y)"
  src <- build_typst_source(text, size = 11)
  lines <- strsplit(src, "\n", fixed = TRUE)[[1]]

  expect_equal(
    lines,
    c(page_line, "#set text(size: 11pt)", "line1", "line2 {x} % y")
  )
})

test_that("build_typst_source validates arguments", {
  expect_error(
    build_typst_source(NULL),
    "single non-missing string"
  )
  expect_error(
    build_typst_source("A", size = 0),
    "positive finite number"
  )
  expect_error(
    build_typst_source("A", alpha = 1.1),
    "\\[0, 1\\]"
  )
  expect_error(
    build_typst_source("A", color = "not-a-color"),
    "invalid color"
  )
})
