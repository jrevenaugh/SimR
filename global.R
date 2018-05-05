# global.R
#
require(tidyverse)

# Geometry and hashes ----------------------------------------------------------
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

# edgeDF for plotting purposes
edgesDF <- as.data.frame(Ematrix)
colnames(edgesDF) <- c("x", "y")
edgesDF$player <- "gray"
edgesDF$seq <- rep( seq(1, 15), each = 2)

vertices <- radius * data.frame(x = cos(phi), y = sin(phi)) %>% slice(1:6)
vertices$names <- toupper(letters[1:6])
vertices$player <- "gray"

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

# Hash for edges to vertices
e2v <- matrix(0, nrow = 15, ncol = 2)
for (i in 1:15) {
  vList <- unique(as.numeric(which( v2e == i, arr.ind = TRUE)))
  e2v[i,] <- vList
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

# Hash from edges to triangles
e2t <- array(0, dim = c(20, 20, 20))
l <- 1
for (i in 1:4) {
  for (j in (i + 1):5) {
    for (k in (j + 1):6) {
      p <- v2e[i,j]
      q <- v2e[j,k]
      r <- v2e[i,k]
      e2t[p,q,r] <- l
      e2t[q,p,r] <- l
      e2t[p,r,q] <- l
      e2t[r,p,q] <- l
      e2t[q,r,p] <- l
      e2t[r,q,p] <- l
      l <- l + 1
    }
  }
}

# Data frame and matrix for triangles.
# Former has vertex and edge indices and player variables
# Latter is just a hash from triangle to vertices and edges
triMat <- matrix(0, nrow = 20, ncol = 6)
for (i in 1:20) {
  vList <- unique(sort(which(v2t == i, arr.ind = TRUE)))
  eList <- unique(sort(which(e2t == i, arr.ind = TRUE)))
  triMat[i,1:3] <- vList
  triMat[i,4:6] <- eList
}

triangles <- as.data.frame(triMat)
colnames(triangles) <- c("V1", "V2", "V3", "E1", "E2", "E3")
triangles$player <- rep("gray", 20)



# Graphics ---------------------------------------------------------------------

pColors <- c("gray70", "dodgerblue", "orangered")
pBreaks <- c("gray70", "dodgerblue", "orangered")
lineScale <- c(1, 1.5, 1.5)

