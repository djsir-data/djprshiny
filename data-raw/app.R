# This script is an environment in which to manually play around with Shiny
# functionality

library(shiny)
library(ggplot2)
library(dplyr)
library(djprtheme)

econ_plot <- function(data,
                      title = "A title",
                      subtitle = "A very long subtitle lorem ipsum lorem ipsum lorem ipsum lorem ipsum lorem ipsum lorem ipsum lorem ipsum lorem ipsum",
                      caption = "A caption") {
  data %>%
    ggplot(aes(x = date, y = value, col = series)) +
    geom_line() +
    labs(
      title = title,
      subtitle = subtitle,
      caption = caption
    ) +
    djpr_colour_manual(5) +
    theme_djpr() +
    facet_wrap(~series, scales = "free")
}

ui <- fluidPage(
  theme = djpr_shiny_theme(),
  title = "Some title",
  navbarPage(
    title = "Overview",
    position = "fixed-top",
    br(),
    centred_row("Lorem ipsum dolor sit amet, no ullum melius laoreet quo, quo iuvaret recteque torquatos id. Vix cu habeo reque nonumy, mel ne deleniti percipit efficiantur. An pro definiebas scripserit. Et errem dicam explicari cum, veritus mediocrem reprehendunt mei an. Duo ad dolor soluta referrentur."),
    djpr_plot_ui("plot1"),
    ggiraph::girafeOutput("plot2"),
    centred_row("Lorem ipsum dolor sit amet, no ullum melius laoreet quo, quo iuvaret recteque torquatos id. Vix cu habeo reque nonumy, mel ne deleniti percipit efficiantur. An pro definiebas scripserit. Et errem dicam explicari cum, veritus mediocrem reprehendunt mei an. Duo ad dolor soluta referrentur."),
    br()
  )
)

server <- function(input, output, session) {

  mydata <- ggplot2::economics_long %>%
    mutate(series = variable)

  djpr_plot_server("plot1",
    plot_function = econ_plot,
    date_slider = TRUE,
    check_box_options = c(
      "pce",
      "pop",
      "psavert",
      "uempmed",
      "unemploy"
    ),
    check_box_var = variable,
    data = mydata %>%
      filter(date >= as.Date("1990-01-01"))
  )

}

shinyApp(ui, server)
