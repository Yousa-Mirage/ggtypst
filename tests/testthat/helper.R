library(ggplot2)
options(device = function(...) grDevices::pdf(file = NULL, ...))

skip_if_no_vdiffr <- function() {
  testthat::skip_if_not_installed("vdiffr")

  run_vdiffr <- Sys.getenv(
    "VDIFFR_RUN_TESTS",
    unset = ifelse(nzchar(Sys.getenv("CI")), "false", "true")
  ) |>
    tolower()

  if (!run_vdiffr %in% c("true", "1", "yes")) {
    testthat::skip("Skipping visual regression tests.")
  }
}

# Draw a grob on a 4 x 3 in (288 x 216 pt) svglite device and return the extent
# of everything drawn, in device points (svg y axis points down).
ink_extent <- function(grob) {
  s <- svglite::svgstring(width = 4, height = 3, standalone = FALSE)
  grid::grid.newpage()
  grid::grid.draw(grob)
  grDevices::dev.off()

  svg <- as.character(s())
  # Leading whitespace keeps `d=` from matching inside `id=`.
  attrs <- unlist(regmatches(svg, gregexpr("\\s(points|d)='[^']+'", svg)))
  nums <- as.numeric(unlist(regmatches(attrs, gregexpr("-?[0-9]+(\\.[0-9]+)?", attrs))))
  xs <- nums[c(TRUE, FALSE)]
  ys <- nums[c(FALSE, TRUE)]

  list(left = min(xs), right = max(xs), top = min(ys), bottom = max(ys))
}

# svglite writes coordinates with two decimals.
expect_pt_equal <- function(object, expected, tolerance = 0.02) {
  expect_lt(abs(object - expected), tolerance)
}
