if (!requireNamespace("ragg", quietly = TRUE)) {
  stop("Package 'ragg' is required to render showcase PNG files.", call. = FALSE)
}

source("inst/examples/annotate-showcase.R", local = TRUE)
source("inst/examples/geom-showcase.R", local = TRUE)
source("inst/examples/element-showcase.R", local = TRUE)

showcase_dimensions <- list(
  width = 10,
  height = 6,
  dpi = 320
)

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

render_all_showcases <- function(
  output_dir = "man/figures",
  width = showcase_dimensions$width,
  height = showcase_dimensions$height,
  dpi = showcase_dimensions$dpi
) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  outputs <- list(
    annotate = save_showcase_png(
      build_annotate_showcase(),
      file.path(output_dir, "annotate-showcase.png"),
      width,
      height,
      dpi
    ),
    geom = save_showcase_png(
      build_geom_showcase(),
      file.path(output_dir, "geom-showcase.png"),
      width,
      height,
      dpi
    ),
    element = save_showcase_png(
      build_element_showcase(),
      file.path(output_dir, "element-showcase.png"),
      width,
      height,
      dpi
    )
  )

  invisible(outputs)
}

if (sys.nframe() == 0) {
  render_all_showcases()
}
