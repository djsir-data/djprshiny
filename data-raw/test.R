library(shiny)
library(ggplot2)

ui <- function() {
  fluidPage(
  djpr_plot_ui("plot"))

plot_function <- function(data = economics,
                          title = "This is a title",
                          subtitle = "This is a subtitle",
                          caption = "This data comes from the ggplot2 package") {
  data %>%
    ggplot(aes(x = date, y = unemploy)) +
    geom_line() +
    labs(title  = title,
         subtitle = subtitle,
         caption = caption) +
    theme_minimal(base_size = 16)
}
}

server <- function(input, output, session) {
  function(input, output, session) {

  djpr_plot_server("plot",
                   plot_function,
                   date_slider = TRUE
  )
  }
}

shinyApp(ui, server)

test_app <- function(...) {
  shinyApp(ui(), server())
}
