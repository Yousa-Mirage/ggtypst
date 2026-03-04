annotate_typst <- function(
  typst_code,
  x,
  y,
  hjust = 0.5,
  vjust = 0.5,
  scale = 1,
  dpi = 300,
  size = NULL,
  alpha = NULL,
  color = NULL,
  family = NULL,
  math_family = NULL,
  angle = NULL
) {
  full_source <- build_typst_source(
    typst_code,
    size = size,
    color = color,
    alpha = alpha,
    family = family,
    math_family = math_family,
    angle = angle
  )
  rendered <- typst_svg(full_source)

  pixel_width <- rendered$width_pt * scale
  pixel_height <- rendered$height_pt * scale

  img_matrix <- rsvg::rsvg_nativeraster(
    rendered$svg,
    width = ceiling(pixel_width * dpi / 72),
    height = ceiling(pixel_height * dpi / 72)
  )

  grob <- grid::rasterGrob(
    img_matrix,
    width = grid::unit(pixel_width, "pt"),
    height = grid::unit(pixel_height, "pt"),
    hjust = hjust,
    vjust = vjust
  )

  ggplot2::annotation_custom(grob, xmin = x, xmax = x, ymin = y, ymax = y)
}
