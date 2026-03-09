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
