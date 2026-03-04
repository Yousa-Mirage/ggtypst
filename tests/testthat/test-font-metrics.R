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
