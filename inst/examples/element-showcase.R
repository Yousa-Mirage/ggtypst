suppressPackageStartupMessages({
  library(ggplot2)
  library(ggtypst)
})

build_element_showcase <- function() {
  cars <- transform(
    mtcars,
    transmission = factor(am, levels = c(0, 1), labels = c("Automatic", "Manual")),
    gear_panel = factor(gear, levels = c(3, 4, 5), labels = c("3 gears", "4 gears", "5 gears"))
  )

  ggplot(cars, aes(factor(cyl), mpg, fill = transmission)) +
    geom_boxplot(width = 0.72, alpha = 0.92, outlier.alpha = 0.35) +
    facet_wrap(~gear_panel) +
    scale_fill_manual(values = c("Automatic" = "#A9D6E5", "Manual" = "#EE9B00")) +
    labs(
      title = r"(`element_typst()` turns theme text into Typst-rendered grobs)",
      subtitle = r"(Axis text, legend labels, strips, and titles all inherit `ggplot2` layout while gaining Typst markup)",
      caption = r"(Math titles can use Typst or MiTeX depending on the element helper you choose.)",
      x = r"("X-Axis Title:" sum_(i=1)^n c_i)",
      y = r"(\text{Y-Axis Title:} \eta = \frac{mpg}{wt})",
      fill = r"(*Transmission*)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title.position = "plot",
      plot.title = element_typst(size = 18, face = "bold", colour = "#102A43", lineheight = 0.95),
      plot.subtitle = element_typst(size = 12, colour = "#486581", lineheight = 1.05),
      plot.caption = element_typst(size = 10, colour = "#7B8794"),
      axis.title.x = element_math_typst(size = 14, face = "bold", colour = "#1D3557"),
      axis.title.y = element_math_mitex(size = 14, face = "bold", colour = "#1D3557"),
      axis.text.x = element_typst(size = 12, face = "bold", colour = "#243B53", angle = 45),
      axis.text.y = element_typst(size = 12, colour = "#52606D"),
      legend.title = element_typst(size = 12, face = "bold", colour = "#102A43"),
      legend.text = element_typst(size = 12, face = "italic", colour = "#334E68"),
      strip.text = element_typst(size = 12, face = "bold.italic", colour = "#8C2F39"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      plot.margin = margin(18, 18, 12, 12)
    )
}

if (sys.nframe() == 0) {
  print(build_element_showcase())
}
