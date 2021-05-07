
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
#'   djpr_plot_ui("plot")
#' )
#'
#' plot_function <- function(data = economics,
#'                           title = "This is a title",
#'                           subtitle = "This is a subtitle",
#'                           caption = "This data comes from the ggplot2 package") {
#'   data %>%
#'     ggplot(aes(x = date, y = unemploy)) +
#'     geom_line() +
#'     labs(
#'       title = title,
#'       subtitle = subtitle,
#'       caption = caption
#'     ) +
#'     theme_minimal(base_size = 14)
#' }
#'
#' server <- function(input, output, session) {
#'   djpr_plot_server("plot",
#'     plot_function,
#'     date_slider = TRUE,
#'     data = ggplot2::economics
#'   )
#' }
#'
#' shinyApp(ui, server)
#' }
#'
djpr_plot_ui <- function(id) {
  centred_row(
    tagList(
      shinyWidgets::chooseSliderSkin("Flat",
        color = "#2A6FA2"
      ),
      br(),
      textOutput(NS(id, "title"), container = djpr_plot_title),
      textOutput(NS(id, "subtitle"), container = djpr_plot_subtitle),
      div(id = "girafe_container",
        ggiraph::girafeOutput(NS(id, "plot"),
            width = "100%",
            height = "400px")
      ),
      fluidRow(
        column(
          8,
          textOutput(NS(id, "caption"), container = djpr_plot_caption)
        ),
        column(4,
          downloadButton(NS(id, "download"),
            "Download",
            style = "font-weight: normal",
            class = "bg-white",
            icon = shiny::icon("arrow-circle-down")
          ),
          align = "right"
        )
      ),
      fluidRow(
        column(
          6,
          uiOutput(NS(id, "date_slider"))
        ),
        column(
          6,
          uiOutput(NS(id, "check_box"))
        )
      ),
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
#' @param check_box_options A character vector containing values to include
#' in a check box. `NULL` by default, which suppresses the check box.
#' @param check_box_var name of column in `data` that contains the levels
#' included in `check_box_options`. `series` by default.
#' @param data data frame containing data to visualise
#' @param plt_change
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
#'   djpr_plot_ui("plot")
#' )
#'
#' plot_function <- function(data = economics) {
#'   data %>%
#'     ggplot(aes(x = date, y = unemploy)) +
#'     geom_line() +
#'     labs(
#'       title = "This is a title",
#'       subtitle = "This is a subtitle",
#'       caption = "This data comes from the ggplot2 package"
#'     ) +
#'     theme_minimal(base_size = 16)
#' }
#'
#' server <- function(input, output, session) {
#'   djpr_plot_server("plot",
#'     plot_function,
#'     date_slider = TRUE,
#'     data = economics,
#'     plt_change = reactive(input$plt_change)
#'   )
#' }
#'
#' shinyApp(ui, server)
#' }
#'
djpr_plot_server <- function(id,
                             plot_function,
                             date_slider = TRUE,
                             check_box_options = NULL,
                             check_box_var = series,
                             data,
                             plt_change) {

  moduleServer(
    id,
    function(input, output, session) {
      base_plot_data <- data

      plot_data <- reactive({
        df <- base_plot_data

        if (date_slider == TRUE) {
          req(input$dates)
          df <- df %>%
            dplyr::filter(.data$date >= input$dates[1] &
              .data$date <= input$dates[2])
        }

        if (!is.null(check_box_options)) {
          req(input$checkboxes)

          checked_opts <- paste0(input$checkboxes,
            collapse = "|"
          )

          df <- df %>%
            dplyr::filter(
              stringr::str_detect(
                {{ check_box_var }},
                checked_opts
              )
            )
        }

        df
      })

      plot <- reactive({
        req(plot_data())
        plot_function(data = plot_data())
      })


      output$date_slider <- renderUI({
        if (date_slider == TRUE) {
          req(base_plot_data)
          sliderInput(NS(id, "dates"),
            label = "",
            min = min(base_plot_data$date),
            max = max(base_plot_data$date),
            value = c(
              min(base_plot_data$date),
              max(base_plot_data$date)
            ),
            timeFormat = "%b %Y",
            ticks = FALSE
          )
        }
      })

      output$check_box <- renderUI({
        if (!is.null(check_box_options)) {
          req(base_plot_data)
          shinyWidgets::awesomeCheckboxGroup(
            NS(id, "checkboxes"),
            label = "",
            choices = check_box_options,
            selected = check_box_options,
            # status = "blue",
            inline = TRUE
          )
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

      output$plot <- ggiraph::renderGirafe({
        req(plt_change())
        static_plot <- plot()
        static_plot$labels$title <- NULL
        static_plot$labels$subtitle <- NULL
        static_plot$labels$caption <- NULL

        browser_params <- plt_change()
        girafe_width <- min(c(1140,
                              browser_params$width))
        girafe_height <- max(c(browser_params$height / browser_params$dpi * 0.4,
                               200 / browser_params$dpi))


        ggiraph::girafe(

          ggobj = static_plot,
          width_svg = (1 * girafe_width / browser_params$dpi),
          height_svg = girafe_height,
          # height_svg = (0.6 * browser_params$height / browser_params$dpi),
          options = list(
            ggiraph::opts_toolbar(saveaspng = FALSE),
            ggiraph::opts_sizing(rescale = FALSE),
            ggiraph::opts_zoom(min = 1, max = 1),
            ggiraph::opts_tooltip(
              delay_mouseover = 100,
              opacity = 0.9,
              css = "background-color: white; color: black; font-family: Roboto, Arial, Helvetica, sans-serif;"
            )
          )
        )
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
