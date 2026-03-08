suppressPackageStartupMessages({
  library(ggplot2)
  library(patchwork)
})

if (!requireNamespace("ragg", quietly = TRUE)) {
  stop("Package 'ragg' is required to render showcase PNG files.", call. = FALSE)
}

source("inst/examples/annotate-showcase.R", local = TRUE)
source("inst/examples/geom-showcase.R", local = TRUE)
source("inst/examples/element-showcase.R", local = TRUE)

showcase_dimensions <- list(
  panel_width = 10,
  panel_height = 6,
  combined_width = 10,
  combined_height = 13.75,
  dpi = 320
)

build_showcase_plots <- function() {
  list(
    annotate = build_annotate_showcase(),
    geom = build_geom_showcase(),
    element = build_element_showcase()
  )
}

build_combined_showcase <- function(plots) {
  patchwork::wrap_plots(
    plots$annotate,
    plots$geom,
    plots$element,
    ncol = 1,
    guides = "keep"
  )
}

save_showcase_png <- function(plot, output, width, height, dpi) {
  elapsed <- system.time({
    ggplot2::ggsave(
      filename = output,
      plot = plot,
      device = ragg::agg_png,
      width = width,
      height = height,
      units = "in",
      dpi = dpi,
      limitsize = FALSE,
      bg = "white"
    )
  })[["elapsed"]]

  message(sprintf("Rendered %s in %.2f s", basename(output), elapsed))
  invisible(output)
}

render_showcase_panels <- function(
  output_dir = "man/figures",
  width = showcase_dimensions$panel_width,
  height = showcase_dimensions$panel_height,
  dpi = showcase_dimensions$dpi,
  plots = build_showcase_plots()
) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  outputs <- list(
    annotate = save_showcase_png(
      plots$annotate,
      file.path(output_dir, "annotate-showcase.png"),
      width,
      height,
      dpi
    ),
    geom = save_showcase_png(
      plots$geom,
      file.path(output_dir, "geom-showcase.png"),
      width,
      height,
      dpi
    ),
    element = save_showcase_png(
      plots$element,
      file.path(output_dir, "element-showcase.png"),
      width,
      height,
      dpi
    )
  )

  invisible(outputs)
}

render_combined_showcase <- function(
  output = "man/figures/ggtypst-showcase.png",
  width = showcase_dimensions$combined_width,
  height = showcase_dimensions$combined_height,
  dpi = showcase_dimensions$dpi,
  plots = build_showcase_plots()
) {
  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  save_showcase_png(build_combined_showcase(plots), output, width, height, dpi)
}

render_all_showcases <- function(
  output_dir = "man/figures",
  panel_width = showcase_dimensions$panel_width,
  panel_height = showcase_dimensions$panel_height,
  combined_width = showcase_dimensions$combined_width,
  combined_height = showcase_dimensions$combined_height,
  dpi = showcase_dimensions$dpi
) {
  plots <- build_showcase_plots()

  panel_outputs <- render_showcase_panels(
    output_dir = output_dir,
    width = panel_width,
    height = panel_height,
    dpi = dpi,
    plots = plots
  )

  combined_output <- render_combined_showcase(
    output = file.path(output_dir, "ggtypst-showcase.png"),
    width = combined_width,
    height = combined_height,
    dpi = dpi,
    plots = plots
  )

  invisible(c(panel_outputs, combined = combined_output))
}

if (sys.nframe() == 0) {
  render_all_showcases()
}
