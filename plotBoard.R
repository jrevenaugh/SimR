require(tidyverse)
source("global.R")
source("helpers.R")

drawBoard <- function(edges, mono) {
  ePlayer(edgesDF) <- edges$v
  vPlayer(vertices) <- edges$v

  g <- ggplot() +
    coord_equal()

  # Show losing triangle (if one exists)
  if (mono$player != gray) {
    g <- g + geom_polygon(data = mono$bold,
                          aes(x, y),
                          fill = mono$player,
                          color = "black",
                          alpha = 0.4)
  }

  # Color in edges and vertices
  g <- g + geom_line(data = edgesDF,
                     aes(x, y, group = seq, color = player, size = player)) +


           geom_point(data = vertices,
                      aes(x, y, fill = player),
                      size = 8,
                      pch = 21,
                      color = "black") +

    # Label vertices
    geom_text(data = vertices,
              aes(x * 1.1, y * 1.1, label = names),
              size = 8) +

    # Set player key scales
    scale_fill_manual(breaks = pColors, values = pColors) +
    scale_color_manual(breaks = pColors, values = pColors) +
    scale_size_manual(breaks = pColors, values = lineScale) +

    # Remove theme elements
    theme_void() +
    theme(legend.position = "none")

  return(g)
}
