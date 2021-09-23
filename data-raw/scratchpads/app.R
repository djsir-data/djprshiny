# This script is an environment in which to manually play around with Shiny
# functionality

library(shiny)
library(ggplot2)
library(dplyr)
library(djprtheme)
library(patchwork)

linechart_plot <- function(data = ggplot2::economics_long %>%
                             dplyr::rename(series = variable)) {
  djpr_ts_linechart(data) #+
  # facet_wrap(~series, scales = "free_y")
}

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
                       title = "Non-interactive plot") {
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
  title = shiny::HTML("<span style = 'line-height: 0.1; font-family: Roboto Slab'>DJPR Jobs<br>Dashboard</span>"),
  logo = "spp_data_logo.png",
  djpr_tab_panel(
    h1("This is an h1"),
    title = "Overview",
    h2("h2 number 1"),
    focus_box("Using RStudio Connect, you can have multiple R processes per app. This means that many concurrent users can be distributed between separate processes and are served more efficiently. As there is no limitation on the number of processes, you can make use of all your machine resources. Using RStudio Connect, you can have multiple R processes per app. This means that many concurrent users can be distributed between separate processes and are served more efficiently. As there is no limitation on the number of processes, you can make use of all your machine resources. Using RStudio Connect, you can have multiple R processes per app. This means that many concurrent users can be distributed between separate processes and are served more efficiently. As there is no limitation on the number of processes, you can make use of all your machine resources."),
    focus_box(
      "Lorem ipsum dolor sit amet, no ullum melius laoreet quo, quo iuvaret recteque torquatos id. Vix cu habeo reque nonumy, mel ne deleniti percipit efficiantur. An pro definiebas scripserit. Et errem dicam explicari cum, veritus mediocrem reprehendunt mei an. Duo ad dolor soluta referrentur.",
      br(),
      textInput("user_title", "", "Title goes here"),
      fluidRow(
        column(
          6,
          djpr_plot_ui("plot1")
        ),
        column(
          6,
          djpr_plot_ui("dual_plots", interactive = F)
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
    djpr_plot_ui("plot3", interactive = F),
    br(),
    djpr_plot_ui("ts_linechart"),
    br()
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
  djpr_plot_server("ts_linechart",
    linechart_plot,
    plt_change = reactive(input$plt_change),
    data = ggplot2::economics_long %>%
      dplyr::rename(series = variable)
  )

  djpr_plot_server("plot1",
    plot_function = econ_plot,
    width_percent = 45,
    date_slider = T,
    download_button = T,
    check_box_options = c(
      "pce",
      "pop",
      "psavert",
      "uempmed",
      "unemploy"
    ),
    check_box_selected = c(
      "pce",
      "pop"
    ),
    check_box_var = variable,
    data = ggplot2::economics_long %>%
      mutate(series = variable) %>%
      filter(date >= as.Date("1990-01-01")),
    plt_change = reactive(input$plt_change),
    height_percent = 150
  )

  djpr_plot_server("plot2",
    plot_function = djpr_ts_linechart,
    data = ggplot2::economics %>%
      rename(value = unemploy) %>%
      mutate(series = "Unemployment"),
    width_percent = 100,
    date_slider = F,
    plt_change = reactive(input$plt_change),
    height_percent = 100
  )

  djpr_plot_server("plot3",
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
    plt_change = reactive(input$plt_change),
    interactive = F
  )

  djpr_plot_server("dual_plots",
    dual_plots,
    width_percent = 45,
    data = ggplot2::economics,
    second_var = "uempmed",
    date_slider_value_min = as.Date("2010-01-01"),
    title = reactive(input$user_title),
    plt_change = reactive(input$plt_change),
    interactive = FALSE,
  )
}

shinyApp(ui, server)

# profvis::profvis(runApp(shinyApp(ui, server)))
