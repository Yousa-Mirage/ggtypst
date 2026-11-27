test_that("typst_render returns the page size and scene", {
  res <- typst_render("Hello, Typst!")

  expect_type(res, "list")
  expect_named(
    res,
    c("width_pt", "height_pt", "descent_pt", "scene", "warnings", "key")
  )
  expect_type(res$width_pt, "double")
  expect_type(res$height_pt, "double")
  expect_type(res$scene, "list")
  expect_type(res$warnings, "list")
  expect_type(res$key, "character")
})

test_that("typst_render reports EmptyPage for empty-like Typst input", {
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
      typst_render(build_typst_source(code)),
      "Typst output has nothing to draw"
    )
  }
})

test_that("typst_render reports compilation failure for invalid Typst syntax", {
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
      typst_render(build_typst_source(code)),
      "Typst compilation failed"
    )
  }
})

test_that("different font families produce different text widths", {
  testthat::skip_on_ci()
  testthat::skip_on_cran()

  sample_text <- "iiiiiiiiiiWWWWWWWWWW 12345"

  arial <- suppressWarnings(typst_render(build_typst_source(
    sample_text,
    family = "Arial"
  )))
  courier <- suppressWarnings(typst_render(build_typst_source(
    sample_text,
    family = "Courier New"
  )))

  if (length(arial$warnings) > 0 || length(courier$warnings) > 0) {
    skip("Required system fonts are not available: Arial/Courier New")
  }

  expect_gt(arial$width_pt, 0)
  expect_gt(courier$width_pt, 0)
  expect_true(arial$width_pt != courier$width_pt)
})

test_that("starting the font warm-up again after loading is harmless", {
  # .onLoad has already started it; later calls do nothing.
  expect_null(rs_warm_up_fonts())
  expect_null(rs_warm_up_fonts())
  rendered <- typst_render(build_typst_source("Warm fonts"))
  expect_gt(rendered$width_pt, 0)
})
