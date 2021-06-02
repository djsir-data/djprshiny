
#' Function creates a plot environment containing (optional)
#' title and subtitle, chart, notes, and download buttons
#' Takes as input a function to create a ggplot2 or ggirafe object
#' @param id a Shiny `outputId` specific to the individual plot.
#' @param height Height of container
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
#'   ggiraph_js(),
#'   centred_row(
#'     djpr_plot_ui("plot")
#'   )
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
#'     data = ggplot2::economics,
#'     plt_change = reactive(input$plt_change)
#'   )
#' }
#'
#' shinyApp(ui, server)
#' }
#'

djpr_plot_ui <- function(id,
                         height = "400px") {
  tagList(
    br(),
    textOutput(NS(id, "title"), container = djpr_plot_title),
    textOutput(NS(id, "subtitle"), container = djpr_plot_subtitle),
    div(
      ggiraph::girafeOutput(NS(id, "plot"),
        width = "100%",
        height = height
      ) %>%
        djpr_with_spinner()
    ),
    fluidRow(
      column(
        7,
        br(),
        textOutput(NS(id, "caption"), container = djpr_plot_caption)
      ),
      column(5,
        br(),
        uiOutput(NS(id, "download_dropdown")),
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
#' @param date_slider_value_min `NULL` by default. Specify a date to modify the
#' first (minimum) selected date in the date slider. A date slider (if present)
#' will have two selected values, corresponding to the start and end of the date
#' range to be visualised. By default, these values are the minimum and
#' maximum of the `date` columns of `data`. Supplying a non-`NULL` value to this
#' argument overrides the minimum value.
#' @param check_box_options A character vector containing values to include
#' in a check box. `NULL` by default, which suppresses the check box.
#' @param check_box_var name of column in `data` that contains the levels
#' included in `check_box_options`. `series` by default.
#' @param download_button logical; `TRUE` by default. When `TRUE`, a download
#' button is displayed.
#' @param width_percent Width of plot object, as a percentage of the standard
#' @param height_percent Height of plot object, as a percentage of the standard
#' @param data data frame containing data to visualise
#' @param plt_change This should be: `reactive(input$plt_change)`. Note that
#' this reactive is created by `ggiraph_js()` which is called by
#' `djpr_tab_panel()`. `djpr_plot_server()` should only be called in apps that
#' feature a `djpr_tab_panel()`.
#' @param ... arguments passed to `plot_function`
#' @import shiny
#' @importFrom rlang .data .env
#' @export
#' @examples
#' \dontrun{
#'
#' library(shiny)
#' library(ggplot2)
#'
#' ui <- djpr_page(
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
                             data,
                             plt_change,
                             date_slider = TRUE,
                             date_slider_value_min = NULL,
                             check_box_options = NULL,
                             check_box_var = series,
                             download_button = TRUE,
                             width_percent = 100,
                             height_percent = 100,
                             ...) {
  moduleServer(
    id,
    function(input, output, session) {

      plot_data <- reactive({
        if (date_slider == TRUE) {
          req(input$dates)
          data <- data %>%
            dplyr::filter(.data$date >= input$dates[1] &
              .data$date <= input$dates[2])
        }

        if (!is.null(check_box_options)) {
          req(input$checkboxes)

          checked_opts <- paste0(input$checkboxes,
            collapse = "|"
          )

          data <- data %>%
            dplyr::filter(
              stringr::str_detect(
                {{ check_box_var }},
                checked_opts
              )
            )
        }

        data
      })

      static_plot <- reactive({
        req(plot_data())
        plot_function(
          data = plot_data(),
          ...
        ) +
          theme(text = element_text(family = "Roboto"))
      }) %>%
        shiny::bindCache(plot_data())


      output$date_slider <- renderUI({
        if (date_slider == TRUE) {
          req(data)

          if (is.null(date_slider_value_min)) {
            date_values <- c(
              min(data$date),
              max(data$date)
            )
          } else {
            date_values <- c(
              date_slider_value_min,
              max(data$date)
            )
          }

          sliderInput(session$ns("dates"),
            label = "",
            min = min(data$date),
            max = max(data$date),
            value = date_values,
            timeFormat = "%b %Y",
            ticks = FALSE
          )
        }
      })

      output$check_box <- renderUI({
        if (!is.null(check_box_options)) {
          req(data)
          shinyWidgets::awesomeCheckboxGroup(
            session$ns("checkboxes"),
            label = "",
            choices = check_box_options,
            selected = check_box_options,
            inline = TRUE
          )
        }
      })

      output$title <- renderText({
        extract_labs(static_plot(), "title")
      })

      output$subtitle <- renderText({
        extract_labs(static_plot(), "subtitle")
      })

      output$caption <- renderText({
        extract_labs(static_plot(), "caption")
      })

      window_size <- reactiveValues(
        width = 1140,
        dpi = 72,
        height = 400
      )

      observeEvent(plt_change()$width, {
        window_size$width <- plt_change()$width
      })

      observeEvent(plt_change()$height, {
        window_size$height <- plt_change()$height
      })

      observeEvent(plt_change()$dpi, {
        window_size$dpi <- plt_change()$dpi
      })

      output$plot <- ggiraph::renderGirafe({
        req(static_plot())

        girafe_width <- min(c(
          1140,
          window_size$width
        )) *
          (width_percent / 100) /
          window_size$dpi

        girafe_height <- max(c(
          window_size$height  * 0.4,
          200
        )) * (height_percent / 100) /
          window_size$dpi

        djpr_girafe(ggobj = static_plot(),
                    width = girafe_width,
                    height = girafe_height)

      }) %>%
        shiny::bindCache(
          static_plot(),
          plt_change(),
          window_size$width,
          window_size$height,
          window_size$width
        )

      if (download_button) {
        output$download_dropdown <- renderUI({
          req(static_plot())
          download_dropdown(session$ns)
        })
      }

      output$download_data <- downloadHandler(
        filename = function() {
          paste0(id, "_data.csv")
        },
        content = function(file) {
          req(static_plot())
          data <- djprtheme::get_plot_data(static_plot())

          utils::write.csv(
            x = data,
            file = file
          )
        }
      )

      output$download_plot <- downloadHandler(
        filename = function() {
          paste0(id, "_plot.pptx")
        },
        content = function(file) {
          req(static_plot())
          plot <- static_plot()

          djprtheme::djpr_save_pptx(
            destination = file,
            plot = plot
          )
        }
      )
    }
  )
}
