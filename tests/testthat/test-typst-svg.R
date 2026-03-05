test_that("typst_svg returns raw svg and size", {
  res <- typst_svg("Hello, Typst!")

  expect_type(res, "list")
  expect_true(all(c("svg", "width_pt", "height_pt", "warnings") %in% names(res)))

  expect_type(res$svg, "raw")
  expect_type(res$width_pt, "double")
  expect_type(res$height_pt, "double")
  expect_type(res$warnings, "list")

  svg_txt <- rawToChar(res$svg)
  expect_true(grepl("^\\s*<svg\\b", svg_txt))
  expect_true(grepl("</svg>\\s*$", svg_txt))
})

test_that("typst_svg reports EmptySvg for empty-like Typst input", {
  empty_like_cases <- c(
    "",
    " ",
    "\n",
    "\t",
    "\r\n",
    "// comment",
    "/* comment */",
    "#let x = 1",
    "#set text(size: 12pt)",
    "#show heading: it => it",
    "#parbreak()",
    "#h(0pt)",
    "#v(0pt)",
    "#[]",
    "#box[]",
    "#text(\"\")",
    "#raw(\"\")",
    "$ $",
    "#if false [x]",
    "#for x in () [x]"
  )

  for (code in empty_like_cases) {
    expect_error(
      typst_svg(build_typst_source(code)),
      "Typst rendered an empty SVG"
    )
  }
})

test_that("typst_svg reports compilation failure for invalid Typst syntax", {
  invalid_syntax_cases <- c(
    "#let x =",
    "#set text(size: )",
    "#show heading: it =>",
    "$hello$",
    "`",
    "*"
  )

  for (code in invalid_syntax_cases) {
    expect_error(
      typst_svg(build_typst_source(code)),
      "Typst compilation failed"
    )
  }
})

test_that("different font families produce different text widths", {
  testthat::skip_on_ci()
  testthat::skip_on_cran()

  sample_text <- "iiiiiiiiiiWWWWWWWWWW 12345"

  arial <- suppressWarnings(typst_svg(build_typst_source(sample_text, family = "Arial")))
  courier <- suppressWarnings(typst_svg(build_typst_source(sample_text, family = "Courier New")))

  if (length(arial$warnings) > 0 || length(courier$warnings) > 0) {
    skip("Required system fonts are not available: Arial/Courier New")
  }

  expect_gt(arial$width_pt, 0)
  expect_gt(courier$width_pt, 0)
  expect_true(arial$width_pt != courier$width_pt)
})
