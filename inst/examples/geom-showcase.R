suppressPackageStartupMessages({
  library(ggplot2)
  library(ggtypst)
})

build_geom_showcase <- function() {
  labels <- data.frame(
    wt = c(1.84, 2.32, 2.77, 3.57, 5.25),
    mpg = c(33.9, 30.4, 19.7, 15.2, 15.0),
    label = c(
      r"(*Toyota Corolla* #linebreak() 33.9 mpg)",
      r"(*Fiat 128* #linebreak() nimble + efficient)",
      r"(*Ferrari Dino* #linebreak() $v_6$ character)",
      r"(*Hornet Sportabout* #linebreak() long-wheelbase cruiser)",
      r"(*Maserati Bora* #linebreak() $v_8$ flagship)"
    ),
    colour = c("#2A6F97", "#3A86A8", "#C44536", "#6B705C", "#7A1E48"),
    face = c("bold", "plain", "italic", "plain", "bold.italic"),
    angle = c(0, 0, -6, 6, -10),
    hjust = c(0, 0, 0.5, 1, 1)
  )

  frontier <- mtcars[
    match(c("Toyota Corolla", "Fiat 128", "Ferrari Dino", "Hornet Sportabout", "Maserati Bora"), rownames(mtcars)),
  ]

  ggplot(mtcars, aes(wt, mpg)) +
    geom_point(color = "#BCCCDC", alpha = 0.35, size = 2.2) +
    geom_path(
      data = frontier,
      color = "#8D99AE",
      linewidth = 0.7,
      linetype = "22"
    ) +
    geom_typst(
      data = labels,
      aes(
        wt,
        mpg,
        label = label,
        colour = colour,
        face = face,
        angle = angle,
        hjust = hjust
      ),
      size = 12,
      size.unit = "pt",
      nudge_y = 0.85,
      show.legend = FALSE
    ) +
    geom_math_mitex(
      data = data.frame(wt = 3.55, mpg = 26.4, label = r"(\eta = \frac{mpg}{wt})"),
      aes(wt, mpg, label = label),
      color = "#1D4E89",
      size = 16,
      nudge_x = 0.45,
      nudge_y = 1.8,
      show.legend = FALSE
    ) +
    scale_colour_identity() +
    coord_cartesian(xlim = c(1.55, 5.7), ylim = c(10, 35.5), clip = "off") +
    labs(
      title = "Geom Showcase",
      subtitle = "Per-row Typst labels become a true data layer with mapped style and math",
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
  print(build_geom_showcase())
}
