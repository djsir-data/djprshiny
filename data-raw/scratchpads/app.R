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
                      caption = "A caption foo bar lorem ipsum blah blah sdfhsdkufh isduhf iushdi us dfiushd iufhsiu dhfiuh sdiufh siudh fiushdf ihs idufhisud hfiusd fhisdfiuhsd iufh") {
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
                       second_var = "uempmed",
                       title = "something") {
  plot1 <- ggplot(data, aes(x = date, y = unemploy)) +
    geom_line() +
    theme_djpr() +
    labs(subtitle = "Plot 1 subtitle")

  plot2 <- ggplot(data, aes(x = date, y = .data[[second_var]])) +
    geom_line() +
    labs(subtitle = "Plot 2 subtitle")

  comb_plots <- patchwork::wrap_plots(plot1, plot2, ncol = 2) +
    plot_annotation(
      title = title,
      subtitle = "Combined plot subtitle",
      caption = "Data source"
    )

  comb_plots
}

title_ui <- function(id) {
  tagList(
    textInput(NS(id, "manual_title"),
      label = "Enter a title"
    ),
    djpr_plot_ui(id)
  )
}

ui <- djpr_page(
  title = shiny::HTML("DJPR Jobs<br>Dashboard"),
  logo = "spp_data_logo.png",
  djpr_tab_panel(
    h1("This is an h1"),
    title = "Overview",
    h2("h2 number 1"),
    focus_box(
      "Lorem ipsum dolor sit amet, no ullum melius laoreet quo, quo iuvaret recteque torquatos id. Vix cu habeo reque nonumy, mel ne deleniti percipit efficiantur. An pro definiebas scripserit. Et errem dicam explicari cum, veritus mediocrem reprehendunt mei an. Duo ad dolor soluta referrentur.",
      br(),
      textInput("user_title", "", "Title goes here"),
      fluidRow(
        column(
          6,
          djpr_plot_ui("plot1") %>% djpr_with_spinner()
        ),
        column(
          6,
          djpr_plot_ui("dual_plots") %>% djpr_with_spinner()
        )
      )
    ),
    h2("Unemployment & underemployment"),
    "Lorem ipsum dolor sit amet, no ullum melius laoreet quo, quo iuvaret recteque torquatos id. Vix cu habeo reque nonumy, mel ne deleniti percipit efficiantur. An pro definiebas scripserit. Et errem dicam explicari cum, veritus mediocrem reprehendunt mei an. Duo ad dolor soluta referrentur.",
    br(),
    br(),
    djpr_plot_ui("plot2"),
    br(),
    h2("h2 number 3"),
    djpr_plot_ui("plot3"),
    br()
    # djpr_plot_ui("dual_plots")
  ),
  djpr_tab_panel(
    title = "This is the second tab",
    # logo = "spp_data_logo.png",
    "Blank tab",
    paste0(rep("lorem ipsum ", 10000), collapse = "|"),
    br()
  )
)

server <- function(input, output, session) {

  djpr_plot_server("plot1",
    plot_function = econ_plot,
    width_percent = 45,
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
    width_percent = 100,
    plt_change = reactive(input$plt_change),
    height_percent = 100
  )

  djpr_plot_server("plot3",
    plot_function = djpr_ts_linechart,
    data = ggplot2::economics %>%
      rename(value = unemploy) %>%
      mutate(series = "Unemployment"),
    plt_change = reactive(input$plt_change)
  )

  djpr_plot_server("dual_plots",
    dual_plots,
    width_percent = 45,
    data = ggplot2::economics,
    second_var = "uempmed",
    title = reactive(input$user_title),
    plt_change = reactive(input$plt_change)
  )
}

shinyApp(ui, server)

# profvis::profvis(runApp(shinyApp(ui, server)))
