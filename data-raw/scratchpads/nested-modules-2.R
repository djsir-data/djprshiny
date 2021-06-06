library(tidyverse)
library(shiny)


plot_fn <- function(data = ggplot2::economics) {
  print(data)
  ggplot(data, aes(x = date, y = unemploy)) +
    geom_line() +
    djprtheme::theme_djpr() +
    labs(
      title = "Plot title",
      subtitle = "Plot subtitle",
      caption = "Plot caption"
    )
}

library(shiny)
my_ui <- function(id) {
  tagList(
    ggiraph:girafeOutput(NS(id, "plot"))
  )
}

ui <- djpr_page(
  title = "title",
  djpr_tab_panel(
    title = "tab title",
    uiOutput("date_slider"),
    # djpr_plot_ui("plot1") #,
    # djpr_plot_ui("plot2")
    my_ui("plot1")
  )
)



server <- function(input, output, session) {
  output$date_slider <- renderUI({
    sliderInput("min_date",
      "",
      min = min(ggplot2::economics$date),
      max = max(ggplot2::economics$date),
      value = as.Date("1990-01-01")
    )
  })

  plot_data <- reactive({
    req(input$min_date)
    ggplot2::economics %>%
      dplyr::filter(date >= input$min_date)
  })

  djpr_plot_server("plot1",
    plot_fn,
    data = plot_data(),
    plt_change = reactive(input$plt_change),
    date_slider = F,
    download_button = F
  )
}

shinyApp(ui, server)
