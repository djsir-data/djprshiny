
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
#' ui <- djpr_page(
#'   title = "My dashboard",
#'   djpr_tab_panel(
#'     title = "First tab",
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
#'   title = "My dashboard",
#'   djpr_tab_panel(
#'     title = "First tab",
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
  djpr_girafe_mem <- memoise::memoise(djpr_girafe,
    cache = getShinyOption("cache")
  )

  moduleServer(
    id,
    function(input, output, session) {

      # Filter data based on user input (slider + checkbox) ----
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

      # Create a subset of plot data to use for caching ----
      first_col <- reactive({
        plot_data()[[1]]
      })

      # Evaluate arguments to plot function ----
      # Need to pass reactive arguments in ...
      # (eg `selected_input = input$focus_sa4`) as reactives
      plot_args <- reactive({
        lapply(list(
          ...
        ), function(x) {
          if (is.reactive(x)) {
            x()
          } else {
            x
          }
        })
      })

      # Construct static plot -----
      # Static plot is a ggplot2 object created by the plot_function()
      # It is not re-rendered on resizing the browser
      static_plot <- reactive({
        req(plot_args(), plot_data())

        args_with_data <- c(
          list(data = plot_data()),
          plot_args()
        )

        do.call(plot_function,
          args = args_with_data
        ) +
          theme(text = element_text(family = "Roboto"))
      }) %>%
        shiny::bindCache(
          id,
          first_col(),
          plot_args()
        )

      # Create date slider UI ------
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

      # Create check box UI -----
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

      # Extract title, subtitle, and caption as HTML ----
      output$title <- renderText({
        extract_labs(static_plot(), "title")
      }) %>%
        shiny::bindCache(static_plot())

      output$subtitle <- renderText({
        extract_labs(static_plot(), "subtitle")
      }) %>%
        shiny::bindCache(static_plot())

      output$caption <- renderText({
        extract_labs(static_plot(), "caption")
      }) %>%
        shiny::bindCache(static_plot())

      window_size <- reactiveValues(
        width = 1140,
        height = 400
      )

      # Capture changes in browser size -----

      observeEvent(plt_change()$width, {
        # Round down to nearest 50 pixels; prevent small resizing
        window_size$width <- floor(plt_change()$width / 50) * 50
      })

      observeEvent(plt_change()$height, {
        # Round down to nearest 50 pixels; prevent small resizing
        window_size$height <- floor(plt_change()$height / 50) * 50
      })

      girafe_width <- reactive({
        req(window_size, plt_change())

        # When the window is narrow, the column width ( plt_change()$width )
        # will equal the full browser width ( plt_change()$browser_width). In
        # that case, we want the plot to fill the whole column.
        if (plt_change()$width == plt_change()$browser_width) {
          width_percent <- min(95, width_percent * 1.95)
        }

        calc_girafe_width(
          width_percent = width_percent,
          window_width = window_size$width,
          dpi = plt_change()$dpi
        )
      }) %>%
        shiny::bindCache(
          plt_change()$width,
          plt_change()$dpi,
          width_percent
        )

      girafe_height <- reactive({
        req(window_size, plt_change())
        calc_girafe_height(
          height_percent = height_percent,
          window_height = window_size$height,
          dpi = plt_change()$dpi
        )
      }) %>%
        shiny::bindCache(
          plt_change()$height,
          plt_change()$dpi,
          height_percent
        )

      # Render plot as ggiraph::girafe object (interactive htmlwidget) -----
      output$plot <- ggiraph::renderGirafe({
        req(
          static_plot(),
          girafe_width(),
          girafe_height()
        )

        djpr_girafe_mem(
          ggobj = static_plot(),
          width = girafe_width(),
          height = girafe_height()
        )
      }) %>%
        shiny::bindCache(
          first_col(),
          plot_args(),
          plt_change()$width,
          plt_change()$height,
          id
        )

      # Create download button UI -----
      if (download_button) {
        output$download_dropdown <- renderUI({
          req(static_plot())
          download_dropdown(session$ns)
        })
      }

      # Create server-side logic for download button -----
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
