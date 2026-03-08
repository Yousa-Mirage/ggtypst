suppressPackageStartupMessages({
  library(ggplot2)
  library(ggtypst)
})

build_annotate_showcase <- function() {
  cars <- transform(mtcars, model = rownames(mtcars))
  focus <- cars[cars$model %in% c("Toyota Corolla", "Ferrari Dino", "Maserati Bora"), ]

  ggplot(cars, aes(wt, mpg)) +
    geom_point(color = "#9AA5B1", alpha = 0.45, size = 2.4) +
    geom_point(
      data = focus,
      aes(fill = model),
      shape = 21,
      color = "#0B1F2A",
      stroke = 0.8,
      size = 4.4,
      show.legend = FALSE
    ) +
    scale_fill_manual(
      values = c(
        "Toyota Corolla" = "#F4D35E",
        "Ferrari Dino" = "#EE964B",
        "Maserati Bora" = "#F95738"
      )
    ) +
    annotate_typst(
      typst_code = paste(
        "*annotate_typst()* #linebreak()",
        "Rich Typst markup, inline styling, and equations#linebreak()",
        "all live directly inside the plotting layer.#linebreak()",
        "#text(fill: rgb(\"#0B3954\"))[Styled note] +",
        "#text(fill: rgb(\"#C1121F\"))[vector math] +",
        "$integral_0^1 x^2 dif x = 1 / 3$"
      ),
      x = 1.9,
      y = 34.7,
      hjust = 0,
      vjust = 1,
      size = 12.5,
      lineheight = 1.1,
      color = "#12344D"
    ) +
    annotate_typst(
      typst_code = "*Toyota Corolla* #linebreak() light, efficient, and easy to call out",
      x = 1.92,
      y = 15,
      hjust = 0,
      size = 11,
      color = "#0B3954"
    ) +
    annotate_typst(
      typst_code = r"(_Maserati Bora_ #linebreak() rotated annotation with bold emphasis)",
      x = 4.5,
      y = 13.4,
      hjust = 1,
      vjust = 0,
      angle = -11,
      size = 10.5,
      face = "bold",
      color = "#8C2F39"
    ) +
    annotate_math_mitex(
      latex_math_code = r"(\eta = \frac{mpg}{wt})",
      x = 4.95,
      y = 33.2,
      hjust = 1,
      size = 16,
      color = "#1D4E89",
      face = "bold"
    ) +
    coord_cartesian(xlim = c(1.55, 5.6), ylim = c(10, 35.5), clip = "off") +
    labs(
      title = "Annotation Showcase",
      subtitle = "Narrative callouts, Typst styling, and LaTeX-to-Typst math in one plot",
      x = "Weight (1000 lbs)",
      y = "Miles per gallon"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title.position = "plot",
      plot.title = element_text(face = "bold", color = "#102A43"),
      plot.subtitle = element_text(color = "#486581"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#E4E7EB"),
      axis.title = element_text(color = "#243B53"),
      axis.text = element_text(color = "#52606D"),
      plot.margin = margin(18, 18, 12, 12)
    )
}

if (sys.nframe() == 0) {
  print(build_annotate_showcase())
}
