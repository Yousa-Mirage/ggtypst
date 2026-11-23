test_that("typst_render returns structured warnings for missing font family", {
  src <- build_typst_source("Hello", family = "ggtypst_missing_font")

  expect_snapshot_warning(typst_render(src))

  res <- suppressWarnings(typst_render(src))

  expect_type(res$warnings, "list")
  expect_gt(length(res$warnings), 0)
  expect_true(all(c("severity", "message", "hints") %in% names(res$warnings[[1]])))
})
