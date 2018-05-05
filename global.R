# global.R
#
require(tidyverse)

# Geometry
phi <- seq(pi, -pi, length.out = 7)
radius <- 1
Ematrix <- matrix(0, nrow = 30, ncol = 2)
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
edges$seq <- rep( seq(1, 15), each = 2)

vertices <- radius * data.frame(x = cos(phi), y = sin(phi)) %>% slice(1:6)
vertices$names <- toupper(letters[1:6])

# Hash for edges lookup (row,column = vertices, 1 = A)
v2e <- matrix(0, nrow = 6, ncol = 6)
k <- 1
for (i in 1:5) {
  for (j in (i + 1):6) {
    v2e[i,j] <- k
    v2e[j,i] <- k
    k <- k + 1
  }
}

# Hash from vertices to triangle
v2t <- array(0, dim = c(6, 6, 6))
l <- 1
for (i in 1:4) {
  for (j in (i + 1):5) {
    for (k in (j + 1):6) {
      v2t[i,j,k] <- l
      v2t[j,i,k] <- l
      v2t[i,k,j] <- l
      v2t[k,i,j] <- l
      v2t[j,k,i] <- l
      v2t[k,j,i] <- l
      l <- l + 1
    }
  }
}


# Graphics ---------------------------------------------------------------------

pColors <- c("gray70", "dodgerblue", "orangered")
pBreaks <- c("gray", "blue", "red")
lineScale <- c(1, 2, 2)

