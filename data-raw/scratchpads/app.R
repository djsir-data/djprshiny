# This script is an environment in which to manually play around with Shiny
# functionality

library(shiny)
library(ggplot2)
library(dplyr)
library(djprtheme)
library(patchwork)

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

dual_plots <- function(data = ggplot2::economics,
                       second_var = uempmed,
                       title = "something") {
  plot1 <- ggplot(data, aes(x = date, y = unemploy)) +
    geom_line() +
    labs(subtitle = "Plot 1 subtitle")

  plot2 <- ggplot(data, aes(x = date, y = {{second_var}})) +
    geom_line() +
    labs(subtitle = "Plot 2 subtitle")

  comb_plots <- patchwork::wrap_plots(plot1, plot2, ncol = 2) +
    plot_annotation(title = title,
                    subtitle = "Combined plot subtitle",
                    caption = "Data source")

  comb_plots

}

title_ui <- function(id) {
  tagList(
    textInput(NS(id, "manual_title"),
              label = "Enter a title"),
    djpr_plot_ui(id)
  )
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
    djpr_plot_ui("plot2"),
    br(),
    title_ui("title_test")
  ),
  djpr_tab_panel(
    title = "Nothing to see here",
    "Blank tab",
    djpr_plot_ui("plot3"),
    br()
  )
)

server <- function(input, output, session) {

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
    width_percent = 50,
    plt_change = reactive(input$plt_change)
  )

  djpr_plot_server("plot3",
                   plot_function = djpr_ts_linechart,
                   data = ggplot2::economics %>%
                     rename(value = unemploy) %>%
                     mutate(series = "Unemployment"),
                   plt_change = reactive(input$plt_change)
  )

  title_server <- function(id) {
    moduleServer(id, function(input, output, session) {
      output$title_test <- djpr_plot_server(id,
                       plot_function = dual_plots,
                       data = ggplot2::economics,
                       plt_change = reactive(input$plt_change),
                       second_var = pop)
    })
  }

  title_server("title_test")
  # djpr_plot_server("title_test",
  #                  plot_function = dual_plots,
  #                  data = ggplot2::economics,
  #                  plt_change = reactive(input$plt_change),
  #                  second_var = pop,
  #                  title = reactive({input$manual_title})
  # )
}

shinyApp(ui, server)
