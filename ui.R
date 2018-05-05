# UI
#
# Establish a bootstrap page with new game widget and instructions in a draggable
# absolutePanel (initially upper left).  Designed to fill window.

require(shiny)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),

  plotOutput(outputId = "board",
             height = "100%",
             width = "100%",
             click = "click",
             dblclick = "dblclick"),

  absolutePanel(top = 10, left = 10, width = "230px", draggable = TRUE,
                wellPanel(h4("SimR V1.0"),
                "Players take turns selecting an uncolored line.",
                "The first player forced to complete a triangle in their",
                "own color loses the game. The game cannot end in a draw.",
                br(), br(),
                "Game originated by Gustavus Simmons in 1969.",
                hr(),
                actionButton(inputId = "reset",
                             label = "New Game"),
                style = "opacity: 0.7; background:#FAFAFA;")
  )
)
