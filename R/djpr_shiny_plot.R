
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
    # shinyWidgets::chooseSliderSkin("Flat",
    #   color = "#2A6FA2"
    # ),
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
        8,
        br(),
        textOutput(NS(id, "caption"), container = djpr_plot_caption)
      ),
      column(4,
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
#' @param width_percent Width of plot object, as a percentage of the standard
#' @param data data frame containing data to visualise
#' @param plt_change reactive(input$plt_change)
#' @param height_scale scaling factor for height; 1 = default
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
                             date_slider = TRUE,
                             date_slider_value_min = NULL,
                             check_box_options = NULL,
                             check_box_var = series,
                             width_percent = 100,
                             data,
                             plt_change,
                             height_scale = 1,
                             ...) {
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

      static_plot <- reactive({
        req(plot_data())
        plot_function(data = plot_data(),
                      ...)
      }) %>%
        shiny::bindCache(plot_data())


      output$date_slider <- renderUI({
        if (date_slider == TRUE) {
          req(base_plot_data)

          if (is.null(date_slider_value_min)) {
            date_values <- c(
              min(base_plot_data$date),
              max(base_plot_data$date)
            )
          } else {
            date_values <- c(
              date_slider_value_min,
              max(base_plot_data$date)
            )
          }

          sliderInput(NS(id, "dates"),
            label = "",
            min = min(base_plot_data$date),
            max = max(base_plot_data$date),
            value = date_values,
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
        window_size$width <- plt_change()$width *
          (width_percent / 100)
      })

      observeEvent(plt_change()$height, {
        window_size$height <- plt_change()$height
      })

      observeEvent(plt_change()$dpi, {
        window_size$dpi <- plt_change()$dpi
      })


      output$plot <- ggiraph::renderGirafe({
        req(static_plot())

        static_plot <- static_plot()
        static_plot$labels$title <- NULL
        static_plot$labels$subtitle <- NULL
        static_plot$labels$caption <- NULL
        static_plot$patches$annotation$title <- NULL
        static_plot$patches$annotation$subtitle <- NULL
        static_plot$patches$annotation$caption <- NULL

        girafe_width <- min(c(
          1140,
          window_size$width
        ))
        girafe_height <- max(c(
          window_size$height / window_size$dpi * 0.4,
          200 / window_size$dpi
        ))

        ggiraph::girafe(
          ggobj = static_plot,
          width_svg = (1 * girafe_width / window_size$dpi) ,
          height_svg = girafe_height * height_scale,
          options = list(
            ggiraph::opts_toolbar(saveaspng = FALSE),
            ggiraph::opts_sizing(rescale = FALSE),
            ggiraph::opts_zoom(min = 1, max = 1),
            ggiraph::opts_tooltip(
              delay_mouseover = 100,
              opacity = 0.9,
              css = "background-color: white; color: black; font-family: Roboto, Arial, Helvetica, sans-serif; line-height: 100%;"
            )
          ),
          fonts = list(sans = "Roboto")
        )
      }) %>%
        shiny::bindCache(
          plot_data(),
          plt_change()
        )

      output$download_dropdown <- renderUI({
        req(static_plot())
        download_dropdown(id)
      })

      output$download_data <- downloadHandler(
        filename = function() {
          paste0(id, "_data.csv")
        },
        content = function(file) {
          req(static_plot())
          plot <- static_plot()
          data <- djprtheme::get_plot_data(plot)

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
