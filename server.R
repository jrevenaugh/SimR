source("plotBoard.R")
source("helpers.R")


server <- function(input, output, session) {
  edges <- reactiveValues(v = rep(0, 15))
  player <- reactiveValues()

  observeEvent(input$click, {

  })

  observeEvent(input$dblclick, {
    a <- c(input$dblclick$x, input$dblclick$y)
    dist <- rep(0, 15)
    for (i in 1:15) dist[i] <- dist2Line(a, vMat[e2v[i,1],], vMat[e2v[i,2],])
    l <- order(dist, decreasing = FALSE)
    if (dist[l[1]] / dist[l[2]] > pickTolerance) return()
    edges$v[l[1]] <- 1
  })

  observeEvent(input$reset, {
    edges$v <- rep(0, 15)
  })


  output$board <- renderPlot({
    g <- drawBoard(edges$v)
    g
  })
}
