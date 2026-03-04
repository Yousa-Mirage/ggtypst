test_that("typst_svg returns structured warnings for missing font family", {
  src <- build_typst_source("Hello", family = "ggtypst_missing_font")

  expect_warning(
    typst_svg(src),
    regexp = "Typst emitted warnings during rendering.",
    class = "ggtypst_typst_warning"
  )

  res <- suppressWarnings(typst_svg(src))

  expect_type(res$warnings, "list")
  expect_gt(length(res$warnings), 0)
  expect_true(all(c("severity", "message", "hints") %in% names(res$warnings[[1]])))
})
