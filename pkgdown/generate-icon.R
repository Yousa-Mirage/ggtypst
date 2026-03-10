required_packages <- c("ggplot2", "ggtypst", "grid", "hexSticker", "png", "jpeg", "rsvg")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]

if (length(missing_packages) > 0) {
  stop(
    sprintf("Missing required packages: %s", paste(missing_packages, collapse = ", ")),
    call. = FALSE
  )
}

sysfonts::font_add(
  "New Computer Modern Math",
  regular = "~/.local/share/fonts/NewCMMath-Regular.otf",
  bold = "~/.local/share/fonts/NewCMMath-Bold.otf"
)

pkgdown_dir <- "pkgdown"
favicon_dir <- file.path(pkgdown_dir, "favicon")
dir.create(favicon_dir, recursive = TRUE, showWarnings = FALSE)

typst_svg_path <- file.path(pkgdown_dir, "typst.svg")
master_plot_path <- file.path(favicon_dir, "ggtypst-icon-plot.png")
master_hex_path <- file.path(favicon_dir, "ggtypst-hex.png")

make_typst_logo_raster <- function(fill = "#FFFFFF", width = 420, height = 420) {
  svg <- paste(readLines(typst_svg_path, warn = FALSE), collapse = "")
  svg <- sub("<path ", sprintf("<path fill='%s' ", fill), svg, fixed = TRUE)

  tmp <- tempfile(fileext = ".svg")
  writeLines(svg, tmp)
  on.exit(unlink(tmp), add = TRUE)

  rsvg::rsvg_nativeraster(tmp, width = width, height = height)
}

build_icon_plot <- function() {
  # 1. 加载白色 Typst Logo 图像
  typst_logo <- make_typst_logo_raster(fill = "#FFFFFF")

  ggplot2::ggplot() +
    # 2. 绘制 x 轴和 y 轴 (带箭头)
    ggplot2::annotate(
      "segment",
      x = 2.1,
      xend = 8.5,
      y = 1.1,
      yend = 1.1,
      colour = "#FFFFFF",
      linewidth = 1.2,
      lineend = "round",
      linejoin = "round",
      arrow = grid::arrow(length = grid::unit(8, "pt"), type = "closed")
    ) +
    ggplot2::annotate(
      "segment",
      x = 2.1,
      xend = 2.2,
      y = 1.1,
      yend = 7.5,
      colour = "#FFFFFF",
      linewidth = 1.2,
      lineend = "round",
      linejoin = "round",
      arrow = grid::arrow(length = grid::unit(8, "pt"), type = "closed")
    ) +

    # 3. 将 Typst Logo 放置在坐标轴中间偏右上的位置
    ggplot2::annotation_custom(
      grid::rasterGrob(typst_logo, interpolate = TRUE),
      xmin = 2.6,
      xmax = 7.6,
      ymin = 2.2,
      ymax = 7.2
    ) +

    # 4. 在 Logo 周围散落分布简短的数学公式
    # 正上方：经典的线性模型
    ggtypst::annotate_math_typst(
      "hat(y) = beta_0 + beta_1 x",
      x = 1.35,
      y = 4,
      size = 8,
      color = "#FFFFFF",
      angle = 90,
      face = "bold"
    ) +
    # 左下角/坐标轴下方：欧拉公式
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

save_plot_png <- function(plot, path, width = 6, height = 6, dpi = 500) {
  ggplot2::ggsave(
    filename = path,
    plot = plot,
    width = width,
    height = height,
    units = "in",
    dpi = dpi,
    bg = "transparent"
  )
}

save_resized_png <- function(input, output, size) {
  img <- png::readPNG(input)
  grDevices::png(output, width = size, height = size, bg = "transparent")
  grid::grid.newpage()
  grid::grid.raster(img, width = grid::unit(1, "npc"), height = grid::unit(1, "npc"), interpolate = TRUE)
  grDevices::dev.off()
}

build_icon_assets <- function() {
  plot <- build_icon_plot()
  save_plot_png(plot, master_plot_path)

  hexSticker::sticker(
    subplot = plot,
    package = "ggtypst",
    s_x = 1,
    s_y = 1.15, # 微调图表高度以适应下方包名
    s_width = 1.4,
    s_height = 1.4,
    p_x = 1,
    p_y = 0.44,
    p_color = "#FFFFFF", # 纯白包名
    p_size = 34, # 醒目大字
    p_family = "New Computer Modern Math",
    p_fontface = "bold",
    h_fill = "#239DAD", # 【关键】Typst 官方青蓝色背景
    h_color = "#FFFFFF", # 纯白六边形边框，呼应内部线条
    h_size = 1.8,
    spotlight = FALSE, # 关闭光影，拥抱纯粹的扁平化质感
    white_around_sticker = FALSE,
    filename = master_hex_path,
    dpi = 600
  )

  sizes <- c(
    `favicon-512.png` = 512,
    `favicon-192.png` = 192,
    `apple-touch-icon.png` = 180,
    `favicon-64.png` = 64,
    `favicon-32.png` = 32,
    `favicon-16.png` = 16
  )

  for (name in names(sizes)) {
    save_resized_png(master_hex_path, file.path(favicon_dir, name), sizes[[name]])
  }

  invisible(master_hex_path)
}

build_icon_assets()
cli::cli_alert_success("Icon assets generated in '{favicon_dir}'")
