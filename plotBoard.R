require(tidyverse)
source("global.R")

source("helpers.R")

drawBoard <- function(edges) {
  ePlayer(edgesDF) <- edges
  vPlayer(vertices) <- edges
  mono <- findMono(edges)

  g <- ggplot() +
    coord_equal() +

    # Color in edges
    geom_line(data = edgesDF,
              aes(x, y, group = seq, color = player, size = player))

  # Show losing triangle (if one exists)
  if (mono$loser != 0) {
    g <- g + geom_polygon(data = mono$bold,
                          aes(x, y, fill = player),
                          color = "black")
    }

    # Color
  g <- g + geom_point(data = vertices,
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
