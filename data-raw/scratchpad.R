# This script is an environment in which to manually play around with Shiny
# functionality

library(shiny)
library(ggplot2)

econ_plot <- function(title = "A title",
                      subtitle = "A very long subtitle lorem ipsum lorem ipsum lorem ipsum lorem ipsum lorem ipsum lorem ipsum lorem ipsum lorem ipsum",
                      caption = "A caption") {
  ggplot2::economics %>%
    ggplot(aes(x = date, y = unemploy)) +
    geom_line() +
    labs(title  = title,
         subtitle = subtitle,
         caption = caption) +
    theme_minimal(base_size = 16)
}

ui <- fluidPage(
  theme = djpr_shiny_theme(),
  title = "Some title",
  centred_row(titlePanel(title = "Example")),
  centred_row("Lorem ipsum dolor sit amet, no ullum melius laoreet quo, quo iuvaret recteque torquatos id. Vix cu habeo reque nonumy, mel ne deleniti percipit efficiantur. An pro definiebas scripserit. Et errem dicam explicari cum, veritus mediocrem reprehendunt mei an. Duo ad dolor soluta referrentur."),
  centred_row(djpr_plot_ui("plot2"))
)

server <- function(input, output, session) {

  output$plot <- renderPlot({
    ggplot2::ggplot(mtcars,
                    aes(x = wt, y = mpg)) +
      geom_point()
  })

  djpr_plot_server(id = "plot2", econ_plot)

}

shinyApp(ui, server)
