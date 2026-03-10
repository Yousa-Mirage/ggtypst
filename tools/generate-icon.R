required_packages <- c("ggplot2", "ggtypst", "grid", "hexSticker", "jpeg", "rsvg", "pkgdown", "cli", "sysfonts")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  stop(
    sprintf("Missing required packages: %s", paste(missing_packages, collapse = ", ")),
    call. = FALSE
  )
}

font_regular <- path.expand("~/.local/share/fonts/NewCMMath-Regular.otf")
font_bold <- path.expand("~/.local/share/fonts/NewCMMath-Bold.otf")

if (file.exists(font_regular) && file.exists(font_bold)) {
  sysfonts::font_add(
    "New Computer Modern Math",
    regular = font_regular,
    bold = font_bold
  )
}

pkgdown_dir <- "pkgdown"
favicon_dir <- file.path(pkgdown_dir, "favicon")
logo_output_path <- file.path("man", "figures", "logo.png")
typst_svg_path <- file.path(pkgdown_dir, "typst.svg")

dir.create(dirname(logo_output_path), recursive = TRUE, showWarnings = FALSE)
dir.create(favicon_dir, recursive = TRUE, showWarnings = FALSE)

make_typst_logo_raster <- function(fill = "#FFFFFF", width = 420, height = 420) {
  svg <- paste(readLines(typst_svg_path, warn = FALSE), collapse = "")
  svg <- sub("<path ", sprintf("<path fill='%s' ", fill), svg, fixed = TRUE)

  tmp <- tempfile(fileext = ".svg")
  writeLines(svg, tmp)
  on.exit(unlink(tmp), add = TRUE)

  rsvg::rsvg_nativeraster(tmp, width = width, height = height)
}

build_icon_plot <- function() {
  typst_logo <- make_typst_logo_raster(fill = "#FFFFFF")

  ggplot2::ggplot() +
    ggplot2::annotate(
      "segment",
      x = 2.2,
      xend = 8.5,
      y = 1.2,
      yend = 1.2,
      colour = "#FFFFFF",
      linewidth = 1.2,
      lineend = "round",
      linejoin = "round",
      arrow = grid::arrow(length = grid::unit(8, "pt"), type = "closed")
    ) +
    ggplot2::annotate(
      "segment",
      x = 2.2,
      xend = 2.2,
      y = 1.2,
      yend = 7.55,
      colour = "#FFFFFF",
      linewidth = 1.2,
      lineend = "round",
      linejoin = "round",
      arrow = grid::arrow(length = grid::unit(8, "pt"), type = "closed")
    ) +
    ggplot2::annotation_custom(
      grid::rasterGrob(typst_logo, interpolate = TRUE),
      xmin = 2.6,
      xmax = 7.6,
      ymin = 2.2,
      ymax = 7.2
    ) +
    ggtypst::annotate_math_typst(
      "hat(y) = beta_0 + beta_1 x",
      x = 1.35,
      y = 4,
      size = 8,
      color = "#FFFFFF",
      angle = 90,
      face = "bold"
    ) +
    ggtypst::annotate_math_typst(
      "e^(i pi) + 1 = 0",
      x = 6.9,
      y = 7.8,
      size = 9,
      color = "#FFFFFF",
      angle = -30
    ) +
    ggtypst::annotate_typst(
      r"($"\frac{d}{dx}"$)",
      x = 8.5,
      y = 4.0,
      size = 8,
      angle = -90,
      color = "#FFFFFF"
    ) +
    ggplot2::coord_cartesian(xlim = c(1, 9), ylim = c(0.5, 9.5), expand = FALSE, clip = "off") +
    ggplot2::theme_void() +
    ggplot2::theme(plot.margin = ggplot2::margin(0, 0, 0, 0))
}

build_icon_assets <- function() {
  plot <- build_icon_plot()

  hexSticker::sticker(
    subplot = plot,
    package = "ggtypst",
    s_x = 1,
    s_y = 1.15,
    s_width = 1.4,
    s_height = 1.4,
    p_x = 1,
    p_y = 0.44,
    p_color = "#FFFFFF",
    p_size = 34,
    p_family = "New Computer Modern Math",
    p_fontface = "bold",
    h_fill = "#239DAD",
    h_color = "#FFFFFF",
    h_size = 1.8,
    spotlight = FALSE,
    white_around_sticker = FALSE,
    filename = logo_output_path,
    dpi = 600
  )

  pkgdown::build_favicons(overwrite = TRUE)

  invisible(logo_output_path)
}

build_icon_assets()
cli::cli_alert_success("Generated master logo at '{logo_output_path}'")
cli::cli_alert_success("Regenerated pkgdown favicon assets in '{favicon_dir}'")
