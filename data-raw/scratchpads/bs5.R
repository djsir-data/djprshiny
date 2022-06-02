library(shiny)
library(shinyWidgets)
library(bslib)

ui <- fluidPage(
  theme = bs_theme(
    version = "5",
    bootswatch = "minty"
  ),
  dropdownButton(
    inputId = "mydropdown",
    label = "Controls",
    icon = icon("sliders"),
    status = "primary",
    circle = FALSE,
    sliderInput(
      inputId = "n",
      label = "Number of observations",
      min = 10, max = 100, value = 30
    ),
    prettyToggle(
      inputId = "na",
      label_on = "NAs keeped",
      label_off = "NAs removed",
      icon_on = icon("check"),
      icon_off = icon("remove")
    )
  ),
  tags$div(style = "height: 140px;"), # spacing
  verbatimTextOutput(outputId = "out"),
  verbatimTextOutput(outputId = "state")
)

server <- function(input, output, session) {
  output$out <- renderPrint({
    cat(
      " # n\n", input$n, "\n",
      "# na\n", input$na
    )
  })

  output$state <- renderPrint({
    cat("Open:", input$mydropdown_state)
  })
}

shinyApp(ui, server)
