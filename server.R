source("plotBoard.R")
source("helpers.R")

server <- function(input, output, session) {

  # Reactive holding edge colors
  edges <- reactiveValues(v = rep(gray, 15))

  # Reactive for game over condition
  mono <- reactiveValues(player = gray,
                         bold = NA)

  # Game play tracker
  tracker <- reactiveValues(gameOver = FALSE,
                            player = blue)

  # Set up selection using a pair of clicks (two vertices)
  observeEvent(input$click, {

  })

  # If game is active, attempt to map double click to a free edge.
  observeEvent(input$dblclick, {
    if (!tracker$gameOver) {

      # Find closest edge, advance only if distinctly closer (pickTolerance)
      a <- c(input$dblclick$x, input$dblclick$y)
      distance <- apply(e2v, 1, dist2Line, a, vMat)
      l <- order(distance, decreasing = FALSE)
      if (distance[l[1]] / distance[l[2]] > pickTolerance) return()

      # Have edge.  If available, assign it and check for monochrome triangles
      if (edges$v[l[1]] == gray) {
        edges$v[l[1]] <- tracker$player
        mono <- findMono(edges$v)
        if (mono$player != gray) {   # Force a refresh when game ends.
          tracker$gameOver <- TRUE
          output$board <- renderPlot({
            g <- drawBoard(edges, mono)
            g
          })
        }
        tracker$player <- ifelse(tracker$player == red, blue, red)
      }
    }
  })

  observeEvent(input$reset, {
    edges$v <- rep(gray, 15)
    tracker$gameOver = FALSE
    tracker$player = blue
    mono$player = gray
    output$board <- renderPlot({
      g <- drawBoard(edges, mono)
      g
    })
  })

  output$board <- renderPlot({
    g <- drawBoard(edges, mono)
    g
  })
}
