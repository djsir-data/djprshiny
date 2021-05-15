library(shiny)
# https://afeld.github.io/bootstrap-toc/#examples

ui <- djpr_page(
  title = "App title",
  tabPanel(
    title = "First tab",
    id = "tab1",
    br(),
    br(),
    br(),
    toc_row(
      page_id = "page-1-content",
      h1("Title"),
      paste0(rep("Text goes here", 100), collapse = ""),
      h2("Some subtitle"),
      paste0(rep("Text goes here", 100), collapse = ""),
      h2("Some other subtitle"),
      h1("Title"),
      djpr_plot_ui("plot"),
      paste0(rep("Text goes here", 100), collapse = ""),
      h1("Another title"),
      paste0(rep("Text goes here", 100), collapse = ""),
      h2("Subsection A"),
      paste0(rep("Text goes here", 100), collapse = ""),
      h2("Subsection B"),
      paste0(rep("Text goes here", 100), collapse = ""),
      h1("A final title"),
      paste0(rep("Text goes here", 100), collapse = "")
    )
  ),
  tabPanel(
    title = "Page 2",
    toc_row(
      page_id = "page-2-content",
      h1("Unemployment"),
      djpr_plot_ui("plot2"),
      paste0(rep("Text goes here", 1e3), collapse = ""),
      h1("Underemploykent"),
      paste0(rep("Text goes here", 1e3), collapse = ""),
      h1("Foo bar"),
      paste0(rep("Text goes here", 1e3), collapse = "")
    )
  )
)



server <- function(input, output, session) {
  djpr_plot_server("plot",
    function(data) {
      ggplot(data, aes(x = date, y = unemploy)) +
        geom_line()
    },
    data = ggplot2::economics,
    date_slider_value_min = as.Date("2000-01-01"),
    plt_change = reactive(input$plt_change)
  )

  djpr_plot_server("plot2",
    function(data) {
      ggplot(data, aes(x = date, y = unemploy)) +
        geom_line()
    },
    data = ggplot2::economics,
    date_slider_value_min = as.Date("2000-01-01"),
    plt_change = reactive(input$plt_change)
  )
}

shinyApp(ui, server)
