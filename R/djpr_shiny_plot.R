
#' Function creates a plot environment containing (optional)
#' title and subtitle, chart, notes, and download buttons
#' Takes as input a function to create a ggplot2 or ggirafe object
#' @param id a Shiny `outputId` specific to the individual plot.
#' @return A `shiny.tag` object creating a plot environment, with
#' labels (title, subtitle, caption) as HTML text, a download button,
#' and optional input controls.
#' @details To be used in conjunction with `djpr_plot_server()` Shiny module,
#' which provides the server-side outputs that `djpr_plot_ui()` expects. See
#' example.
#' @export
#' @examples
#' \dontrun{
#'
#' library(shiny)
#' library(ggplot2)
#'
#' ui <- fluidPage(
#'   djpr_plot_ui("plot"))
#'
#' plot_function <- function(data = economics,
#'                           title = "This is a title",
#'                           subtitle = "This is a subtitle",
#'                           caption = "This data comes from the ggplot2 package") {
#'   data %>%
#'     ggplot(aes(x = date, y = unemploy)) +
#'     geom_line() +
#'     labs(title  = title,
#'          subtitle = subtitle,
#'          caption = caption) +
#'     theme_minimal(base_size = 16)
#' }
#'
#' server <- function(input, output, session) {
#'
#'   djpr_plot_server("plot",
#'                    plot_function,
#'                    date_slider = TRUE
#'   )
#' }
#'
#' shinyApp(ui, server)
#' }
#'

djpr_plot_ui <- function(id) {
  centred_row(
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
    ),
    uiOutput(NS(id, "user_input")),
    br()
  )
  )
}

#' Shiny module to create DJPR plot environment.
#'
#' Server-side companion to `djpr_plot_ui()`.
#'
#' @param id a Shiny `outputId` specific to the individual plot.
#' @param plot_function A function (without `()`) that creates a ggplot2
#' object. Function must contain a `data` argument that takes a data.frame.
#' @param date_slider Logical; `TRUE` if you want a date slider to be shown.
#' If `TRUE`, your data must contain a `date` column.
#' @import shiny
#' @importFrom rlang .data .env
#' @export
#' @examples
#' \dontrun{
#'
#' library(shiny)
#' library(ggplot2)
#'
#' ui <- fluidPage(
#'   djpr_plot_ui("plot"))
#'
#' plot_function <- function(data = economics,
#'                           title = "This is a title",
#'                           subtitle = "This is a subtitle",
#'                           caption = "This data comes from the ggplot2 package") {
#'   data %>%
#'     ggplot(aes(x = date, y = unemploy)) +
#'     geom_line() +
#'     labs(title  = title,
#'          subtitle = subtitle,
#'          caption = caption) +
#'     theme_minimal(base_size = 16)
#' }
#'
#' server <- function(input, output, session) {
#'
#'   djpr_plot_server("plot",
#'                    plot_function,
#'                    date_slider = TRUE
#'   )
#' }
#'
#' shinyApp(ui, server)
#' }
#'

djpr_plot_server <- function(id, plot_function, date_slider = TRUE) {
  moduleServer(
    id,
    function(input, output, session) {

      base_plot <- plot_function()

      base_plot_data <- base_plot$data

      plot_data <- reactive({
        df <- base_plot$data

        if (date_slider == TRUE) {
          req(input$dates)
          df <- df %>%
            dplyr::filter(.data$date >= input$dates[1])
        }

        df

        })

      plot <- reactive({
        req(plot_data())
        plot_function(data = plot_data())
        })


      output$user_input <- renderUI({
        if (date_slider == TRUE) {
          req(base_plot_data)
          sliderInput(NS(id, "dates"),
                      label = "",
                      min = min(base_plot_data$date),
                      max = max(base_plot_data$date),
                      value = min(base_plot_data$date))
        }

      })

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

          ggplot2::ggsave(
              filename = file,
              plot = obj
            )
          }
      )


    }
  )
}



