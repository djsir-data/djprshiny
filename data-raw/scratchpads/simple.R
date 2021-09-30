library(tidyverse)
library(shiny)
devtools::load_all()

plot_fn <- function(data = ggplot2::economics_long) {
  data %>%
    rename(series = variable) %>%
    djpr_ts_linechart() +
    facet_wrap(~series, scales = "free_y") +
    labs(title = paste0("Random number in title is ", floor(rnorm(1) * 100)),
         subtitle = "Testing testing",
         caption = "1, 2, 3")
}

ui <- djpr_page(
  title = "Simple scratchpad",
  djpr_tab_panel(
    title = "First tab",
    djpr_plot_ui("interactive_plot"),
    djpr_plot_ui("static_plot",
                 interactive = FALSE)
  )
)


server <- function(input, output, session) {
  djpr_plot_server(id = "interactive_plot",
                   plot_function = plot_fn,
                   data = ggplot2::economics_long ,
                   plt_change = reactive(input$plt_change),
                   interactive = T
                   )

  djpr_plot_server(id = "static_plot",
                   plot_function = plot_fn,
                   data = ggplot2::economics_long,
                   plt_change = reactive(input$plt_change),
                   interactive = F)
}



# profvis::profvis(
runApp(shinyApp(ui, server))
# )

#######

no_djpr_ui <- fluidPage(
  ggiraph::girafeOutput("plot") %>%
    djpr_with_spinner()
)

no_djpr_server <- function(input, output, session) {
  output$plot <- ggiraph::renderGirafe({
    static <- plot_fn()
    ggiraph::girafe(ggobj = static)
  })
}

# runApp(shinyApp(no_djpr_ui, no_djpr_server))
