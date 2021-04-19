
#' Function creates a plot environment containing (optional)
#' title and subtitle, chart, notes, and download buttons
#' Takes as input a function to create a ggplot2 or ggirafe object



djpr_plot_title <- function(...) {
  div(..., style = "font-size: 163%; font-weight: bold; line-height: 1.2")
}

djpr_plot_subtitle <- function(...) {
  div(..., style = "font-size: 145%; font-weight: normal; line-height: 1.1")
}

djpr_plot_caption <- function(...) {
  div(..., style = "font-size: 73%; font-weight: normal; line-height: 0.9")
}

djpr_plot_ui <- function(id) {
  tagList(
    br(),
    textOutput(NS(id, "title"), container = djpr_plot_title),
    textOutput(NS(id, "subtitle"), container = djpr_plot_subtitle),
    plotOutput(NS(id, "plot")),
    fluidRow(
      column(8,
             textOutput(NS(id, "caption"), container = djpr_plot_caption)),
      column(4,
             downloadButton(NS(id, "download"),
                            "Download",
                            style = "font-weight: normal;",
                            icon = shiny::icon("arrow-circle-down")),
             align = 'right')
    )
  )
}

djpr_plot_server <- function(id, plot_function) {
  moduleServer(
    id,
    function(input, output, session) {

      plot <- reactive(plot_function())

      output$title <- renderText({
          plot()$labels$title
        })

      output$subtitle <- renderText({
        plot()$labels$subtitle
      })

      output$caption <- renderText({
        plot()$labels$caption
      })

      output$plot <- renderPlot({
        x <- plot()
        x$labels$title <- NULL
        x$labels$subtitle <- NULL
        x$labels$caption <- NULL
        x
      })

      output$download <- downloadHandler(
        filename = function() {
          paste0(id, ".png")
        },
        content = function(file) {
          obj <- plot()

          ggsave(
              filename = file,
              plot = obj
            )
          }
      )


    }
  )
}



