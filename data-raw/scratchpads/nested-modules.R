library(shiny)
library(tidyverse)

plot_fn <- function(data = ggplot2::economics) {
  ggplot(data, aes(x = date, y = unemploy)) +
    geom_line() +
    djprtheme::theme_djpr() +
    labs(subtitle = "Plot 1 subtitle")
}

inner_ui <- function(id) {
  tagList(
    plotOutput(NS(id, "plot")),
    sliderInput(NS(id, "date"),
      label = "dates",
      min = min(ggplot2::economics$date),
      max = max(ggplot2::economics$date),
      value = min(ggplot2::economics$date)
    )
  )
}

outer_ui <- function(id) {
  tagList(
    inner_ui(NS(id, "first")),
    inner_ui(NS(id, "second"))
  )
}

inner_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      plot_data <- reactive({
        dplyr::filter(
          ggplot2::economics,
          date >= input$date
        )
      })

      output$plot <- renderPlot({
        plot_fn(plot_data())
      })
    }
  )
}

outer_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      inner_server("first")
      inner_server("second")
    }
  )
}

ui <- fluidPage(
  inner_ui("first_plot"),
  outer_ui("outer_plots")
)

server <- function(input, output, session) {
  inner_server("first_plot")
  outer_server("outer_plots")
}

shinyApp(ui, server)
