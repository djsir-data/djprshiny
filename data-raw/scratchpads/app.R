# This script is an environment in which to manually play around with Shiny
# functionality

library(shiny)
library(ggplot2)
library(dplyr)
library(djprtheme)
# library(showtext)

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

ui <- djpr_page(
  title = "Some title",
  djpr_tab_panel(
    h1("This is an h1"),
    title = "Overview",
    br(),
    "Lorem ipsum dolor sit amet, no ullum melius laoreet quo, quo iuvaret recteque torquatos id. Vix cu habeo reque nonumy, mel ne deleniti percipit efficiantur. An pro definiebas scripserit. Et errem dicam explicari cum, veritus mediocrem reprehendunt mei an. Duo ad dolor soluta referrentur.",
    br(),
    djpr_plot_ui("plot1"),
    "Lorem ipsum dolor sit amet, no ullum melius laoreet quo, quo iuvaret recteque torquatos id. Vix cu habeo reque nonumy, mel ne deleniti percipit efficiantur. An pro definiebas scripserit. Et errem dicam explicari cum, veritus mediocrem reprehendunt mei an. Duo ad dolor soluta referrentur.",
    br(),
    br(),
    djpr_plot_ui("plot2")
  ),
  djpr_tab_panel(
    title = "Nothing to see here",
    "Blank tab",
    djpr_plot_ui("plot3"),
    br()
  )
)

server <- function(input, output, session) {

  # sysfonts::font_add_google("Roboto", "Roboto")
  # showtext_auto()

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
    data = ggplot2::economics_long %>%
      mutate(series = variable) %>%
      filter(date >= as.Date("1990-01-01")),
    plt_change = reactive(input$plt_change)
  )

  djpr_plot_server("plot2",
    plot_function = djpr_ts_linechart,
    data = ggplot2::economics %>%
      rename(value = unemploy) %>%
      mutate(series = "Unemployment"),
    plt_change = reactive(input$plt_change)
  )

  djpr_plot_server("plot3",
                   plot_function = djpr_ts_linechart,
                   data = ggplot2::economics %>%
                     rename(value = unemploy) %>%
                     mutate(series = "Unemployment"),
                   plt_change = reactive(input$plt_change)
  )
}

shinyApp(ui, server)
