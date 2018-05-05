require(tidyverse)
source("global.R")


drawBoard <- function(edges, vertices) {
  g <- ggplot() +

    # Color in edges
    geom_line(data = edges,
              aes(x, y, group = seq, color = player, size = player)) +

    # Color
    geom_point(data = vertices,
               aes(x, y, fill = player),
               size = 8,
               pch = 21,
               color = "black") +

    # Label vertices
    geom_text(data = vertices,
              aes(x * 1.1, y * 1.1, label = names),
              size = 5) +

    # Set player key scales
    scale_fill_manual(breaks = pBreaks, values = pColors) +
    scale_color_manual(breaks = pBreaks, values = pColors) +
    scale_size_manual(breaks = pBreaks, values = lineScale) +

    # Remove theme elements
    theme_void() +
    theme(legend.position = "none")

  return(g)
}
