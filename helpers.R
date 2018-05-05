
# Take edges vector (edge "color") and map to edgesDF
`ePlayer<-` <- function(x, value) {
  i <- rep(1:15, each = 2)
  x$player <- factor(value[i], levels = pColors, labels = pColors)
  return(x)
}

# Take edges vector, map to vertices and assign colors
# This assumes that edges = -1 for "blue", 1 for "red", and 0 for "gray."
`vPlayer<-` <- function(x, value) {
  x$player <- gray
  vSum <- rep(0, 6)
  value <- factor(value, levels = pColors[c(2,1,3)])
  for (i in 1:15) {
    vs <- e2v[i,]
    vSum[vs[1]] <- vSum[vs[1]] + as.integer(value[i])
    vSum[vs[2]] <- vSum[vs[2]] + as.integer(value[i])
  }
  for (i in 1:6) {
    if (vSum[i] > 10) x$player[i] <- red
    else if (vSum[i] < 10) x$player[i] <- blue
  }
  x$player <- factor(x$player, levels = pColors )
  return(x)
}

# Distance from point a to line ls defined by vertices in vMat
dist2Line <- function(x, a, vMat) {
  v1 <- vMat[x[1],] - vMat[x[2],]
  v2 <- a - vMat[x[1],]
  m <- cbind(v1, v2)
  d <- abs(det(m)) / sqrt(t(v1) %*% v1)
  return(d)
}


# Check for monochrome triangle(s) (would always be same color).  If found,
# return vertices and player.  Include data frame to "bold" the triangle on the
# gameboard.
findMono <- function(edges) {
  loser = gray
  for (i in 1:20) {
    eList <- triMat[i,4:6]
    if (all(edges[eList] == red)) {
      loser <- red
      iTri <- i
      break
    } else if (all(edges[eList] == blue)) {
      loser <- blue
      iTri <- i
      break
    }
  }
  if (loser != gray) {
    bold <- data.frame(x = vertices$x[triMat[iTri,c(1:3,1)]],
                       y = vertices$y[triMat[iTri,c(1:3,1)]],
                       player = loser,
                       stringsAsFactors = FALSE)
    return(list(player = loser, bold = bold))
  } else{
    return(list(player = gray, bold = NA))
  }
}
