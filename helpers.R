# Take edges vector (edge "color") and map to edgesD
ePlayer <- function(edges, edgesDF) {
  i <- rep(1:15, each = 2)
  edgesDF$player <- factor(edges[i], levels = c(0, -1, 1), labels = pBreaks)
  return(edgesDF)
}

`ePlayer<-` <- function(x, value) {
  i <- rep(1:15, each = 2)
  x$player <- factor(value[i], levels = c(0, -1, 1), labels = pBreaks)
  return(x)
}

# Take edges vector, map to vertices and assign colors
# This assumes that edges = -1 for "blue", 1 for "red", and 0 for "gray."
vPlayer <- function(edges, vertices) {
  vSum <- rep(0, 6)
  vertices$player <- pColors[1]
  for (i in 1:15) {
    vs <- e2v[i,]
    vSum[vs[1]] <- vSum[vs[1]] + edges[i]
    vSum[vs[2]] <- vSum[vs[2]] + edges[i]
  }
  for (i in 1:6) {
    if (vSum[i] > 0) vertices$player[i] <- pColors[3]
    else if (vSum[i] < 0) vertices$player[i] <- pColors[2]
  }
  vertices$player <- factor(vertices$player, levels = pBreaks)
  return(vertices)
}

`vPlayer<-` <- function(x, value) {
  vSum <- rep(0, 6)
  x$player <- pColors[1]
  for (i in 1:15) {
    vs <- e2v[i,]
    vSum[vs[1]] <- vSum[vs[1]] + value[i]
    vSum[vs[2]] <- vSum[vs[2]] + value[i]
  }
  for (i in 1:6) {
    if (vSum[i] > 0) x$player[i] <- pColors[3]
    else if (vSum[i] < 0) x$player[i] <- pColors[2]
  }
  x$player <- factor(x$player, levels = pBreaks)
  return(x)
}

# Distance from point a to line connecting b and c
dist2Line <- function(a, b, c) {
  v1 <- b - c
  v2 <- a - b
  m <- cbind(v1, v2)
  d <- abs(det(m)) / sqrt(sum(v1 * v1))
  return(d)
}
