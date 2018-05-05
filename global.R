# global.R
#
require(tidyverse)

# Geometry
phi <- seq(pi, -pi, length.out = 7)
radius <- 1
Ematrix <- matrix(0, nrow = 40, ncol = 2)
k <- 1
for (i in 1:5) {
  for (j in (i + 1):6) {
    Ematrix[k,] <- radius * c(cos(phi[i]), sin(phi[i]))
    Ematrix[k + 1,] <- radius * c(cos(phi[j]), sin(phi[j]))
    k <- k + 2
  }
}
edges <- as.data.frame(Ematrix)
colnames(edges) <- c("x", "y")
edges$player <- "gray"
edges$seq <- rep( seq(1, 20), each = 2)

vertices <- radius * data.frame(x = cos(phi), y = sin(phi)) %>% slice(1:6)
vertices$names <- toupper(letters[1:6])


# Graphics ---------------------------------------------------------------------

pColors <- c("gray70", "dodgerblue", "orangered")
pBreaks <- c("gray", "blue", "red")
lineScale <- c(1, 2, 2)

