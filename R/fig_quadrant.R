quad_data <- tribble(
  ~title, ~motives, ~audience,
  "TA", 9, 8,
  "ALA", 1, 3,
  "My Backyard", 8, 4,
  "Act for Birds", 8, 1.5,
  "POMDPs shiny apps", 1, 1,
  "Celebrity conservation", 3, 9.5,
  "Citizen science", 2, 3,
  "Goldberg et al. 2021", 9, 2
)

empty_theme <- theme(
  plot.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  axis.text.y = element_text(angle = 90)
)

ggplot_quadrant <- function(data) {
  plot <- ggplot(data, aes(x = motives, y = audience, label = title)) +
    coord_fixed() +
    # set the scale to one greater than 0-10 in each direction
    # this gives us some breating room and space to add some arrows
    scale_x_continuous(
      expand = c(0, 0), limits = c(0, 10),
      breaks = c(1,9), labels=c("1" = "Apolitical", "9" = "Political")
    ) +
    scale_y_continuous(
      expand = c(0, 0), limits = c(0, 10),
      breaks = c(1,9), labels=c("1" = "Specific", "9" = "Broad")
    ) +
    empty_theme +
    labs(
      x = "Motives",
      y = "Audience"
    ) +
    geom_point(colour = "black", size = 2) +
    geom_label_repel(
      size = 4, fill = "deepskyblue",
      colour = "black"
    ) +
    geom_segment(aes(x = 10, y = 0, xend = 10, yend = 10)) +
    geom_segment(aes(x = 0, y = 0, xend = 0, yend = 10)) +
    geom_segment(aes(x = 0, y = 0, xend = 10, yend = 0)) +
    geom_segment(aes(x = 0, y = 5, xend = 10, yend = 5)) +
    geom_segment(aes(x = 5, y = 0, xend = 5, yend = 10)) +
    geom_segment(aes(x = 0, y = 10, xend = 10, yend = 10)) +
    annotate("text", x = 2.5, y = 2.5, alpha = 0.5, label = "Science-serving") +
    annotate("text", x = 2.5, y = 7.5, alpha = 0.5, label = "Layperson-charismatic") +
    annotate("text", x = 7.5, y = 2.5, alpha = 0.5, label = "Ecologically-targeted") +
    annotate("text", x = 7.5, y = 7.5, alpha = 0.5, label = "Politically-charged")
  plot
}

ggplot_save <- function(filename, plot) {
  ggsave(
    here(paste("output/", filename, ".pdf", sep = "")),
    plot = plot,
    width = 20, height = 20, units = "cm"
  )
}

