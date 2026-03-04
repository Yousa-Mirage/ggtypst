test_that("typst_svg returns raw svg and size", {
  res <- typst_svg("Hello, Typst!")

  expect_type(res, "list")
  expect_true(all(c("svg", "width_pt", "height_pt") %in% names(res)))

  expect_type(res$svg, "raw")
  expect_type(res$width_pt, "double")
  expect_type(res$height_pt, "double")

  svg_txt <- rawToChar(res$svg)
  expect_true(grepl("^\\s*<svg\\b", svg_txt))
  expect_true(grepl("</svg>\\s*$", svg_txt))
})

test_that("build_typst_source empty-like input triggers empty render error", {
  expect_error(
    typst_svg(build_typst_source("")),
    "Typst rendered an empty SVG"
  )
  expect_error(
    typst_svg(build_typst_source(" ")),
    "Typst rendered an empty SVG"
  )
})
