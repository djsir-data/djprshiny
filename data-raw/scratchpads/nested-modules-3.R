library(shiny)
library(tidyverse)

plot_fn <- function(data = ggplot2::economics) {
  ggplot(data, aes(x = date, y = unemploy)) +
    geom_line() +
    djprtheme::theme_djpr() +
    labs(
      title = "Plot title",
      subtitle = "Plot subtitle",
      caption = "Plot caption"
    )
}



outer_ui <- function(id) {
  tagList(
    uiOutput(NS(id, "main_slider")),
    djpr_plot_ui(NS(id, "first")),
    djpr_plot_ui(NS(id, "second"))
  )
}


outer_server <- function(id, data, plt_change) {
  moduleServer(
    id,
    function(input, output, session) {
      output$main_slider <- renderUI({
        sliderInput(session$ns("dates"),
          label = "dates",
          min = min(data$date),
          max = max(data$date),
          value = as.Date("1990-01-01")
        )
      })

      observeEvent(input$dates, {
        print(input$dates)
      })

      observeEvent(plot_data(), {
        glimpse(plot_data())
      })


      plot_data <- reactive({
        req(input$dates)
        dplyr::filter(data, date >= input$dates)
      })

      djpr_plot_server("first",
        plot_fn,
        # data = plot_data(),
        data = plot_data(),
        plt_change = plt_change,
        download_button = FALSE
      )

      djpr_plot_server("second",
        plot_fn,
        data = data,
        plt_change = plt_change,
        download_button = FALSE
      )
    }
  )
}

ui <- djpr_page(
  title = "some title",
  djpr_tab_panel(
    title = "some tab",
    outer_ui("outer_plots")
  )
)

server <- function(input, output, session) {
  outer_server("outer_plots",
    data = ggplot2::economics,
    plt_change = reactive(input$plt_change)
  )
}

shinyApp(ui, server)
