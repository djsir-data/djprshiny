
#' Function creates a plot environment containing (optional)
#' title and subtitle, chart, notes, and download buttons
#' Takes as input a function to create a ggplot2 or ggirafe object
#' @param id a Shiny `outputId` specific to the individual plot.
#' @param height Height of container
#' @param width Width of container; default is "100%"
#' @param interactive Logical; `TRUE` by default.
#' @return A `shiny.tag` object creating a plot environment, with
#' labels (title, subtitle, caption) as HTML text, a download button,
#' and optional input controls.
#' @details To be used in conjunction with `djpr_plot_server()` Shiny module,
#' which provides the server-side outputs that `djpr_plot_ui()` expects. See
#' example.
#' @importFrom purrr map imap discard
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
                         height = "400px",
                         width = "100%",
                         interactive = TRUE) {
  if (interactive) {
    plot_ui <- ggiraph::girafeOutput(NS(id, "plot"),
      width = width,
      height = height
    )
  } else {
    plot_ui <- plotOutput(NS(id, "plot"),
      height = height,
      width = width
    )
  }

  tagList(
    textOutput(NS(id, "title"), container = djpr_plot_title),
    textOutput(NS(id, "subtitle"), container = djpr_plot_subtitle),
    plot_ui %>%
      djpr_with_spinner(
        proxy.height = height,
        hide.ui = FALSE
      ),
    fluidRow(
      column(
        7,
        br(),
        textOutput(NS(id, "caption"), container = djpr_plot_caption)
      ),
      column(5,
        id = NS(id, "download_col"),
        br(),
        download_ui(NS(id, "download_dropdown")),
        align = "right"
      )
    ),
    fluidRow(
      column(
        6,
        id = NS(id, "date_slider_col"),
        sliderInput(NS(id, "dates"),
          label = "",
          min = as.Date("1978-01-01"),
          max = Sys.Date(),
          value = c(
            as.Date("1978-01-01"),
            as.Date("2017-10-18")
          ),
          dragRange = TRUE,
          timeFormat = "%b %Y",
          ticks = FALSE
        )
      ),
      column(
        5,
        id = NS(id, "check_box_col"),
        shinyWidgets::awesomeCheckboxGroup(
          NS(id, "checkboxes"),
          label = "",
          choices = NULL,
          selected = NULL,
          inline = TRUE
        )
      ),
      column(1)
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
#' @param check_box_selected A character vector containing values of the check
#' box that should be selected by default.
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
#' @param interactive logical; `TRUE` by default. When `TRUE`, plot will be
#' rendered as an interactive `ggiraph` object; when `FALSE` a static ggplot
#' will be rendered.
#' @param convert_lazy logical; `TRUE` by default. When `TRUE`, lazy data will be
#' converted so that a data.frame is passed to plot functions.
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
                             check_box_selected = check_box_options,
                             check_box_var = series,
                             download_button = TRUE,
                             width_percent = 100,
                             height_percent = 100,
                             interactive = TRUE,
                             convert_lazy = TRUE,
                             ...) {
  moduleServer(
    id,
    function(input, output, session) {


      # Date slider and checkbox UI are both created on the UI side and then
      # updated on the server side; this is quicker than creating server-side
      # and displaying with `uiOutput()`

      # Update date slider UI ------
      dates <- data %>%
        dplyr::ungroup() %>% # In case we're accidentally passed a grouped tbl
        dplyr::summarise(min = min(as.Date(date), na.rm = TRUE),
                         max = max(as.Date(date), na.rm = TRUE))

      if ('tbl_lazy' %in% class(data)) {
        dates <- dates %>% dplyr::collect()
      }

      if (date_slider) {
        min_slider_date <- ifelse(is.null(date_slider_value_min),
          dates$min,
          date_slider_value_min
        ) %>%
          as.Date(origin = as.Date("1970-01-01"))

        shiny::updateSliderInput(
          session = session,
          "dates",
          value = c(
            min_slider_date,
            dates$max
          ),
          min = dates$min,
          max = dates$max,
          timeFormat = "%b %Y"
        )

      } else {
        removeUI(selector = paste0("#", NS(id, "date_slider_col")))
      }

      # Update check box UI ------
      if (!is.null(check_box_options)) {
        shinyWidgets::updateAwesomeCheckboxGroup(
          session = session,
          "checkboxes",
          label = "",
          choices = check_box_options,
          selected = check_box_selected,
          inline = TRUE
        )
      } else {
        removeUI(selector = paste0("#", NS(id, "check_box_col")))
      }

      data_ready <- reactive({
        # This reactive indicates whether the inputs (date slider + checkboxes)
        # have been updated from their default values
        out <- FALSE

        if (date_slider == TRUE &&
            # Need to make sure that input$dates has updated from the default
            input$dates[2] != as.Date("2017-10-18")) {
          out <- TRUE
        }

        if (date_slider != TRUE) {
          out <- TRUE
        }

        if (!is.null(check_box_options) &&
            !is.null(input$checkboxes)) {
          out <- TRUE
        }

        out
      })

      # Filter data based on user input (slider + checkbox) ----
      plot_data <- reactive({
        req(data_ready())

        if (date_slider == TRUE) {
          req(input$dates)

          selected_dates <- c(
            date_floor(input$dates[1]),
            date_ceiling(input$dates[2])
          )

          # data <- data[
          #   data$date >= selected_dates[1] &
          #     data$date <= selected_dates[2],
          # ]
          data <- data %>%
            dplyr::filter(date >= !!selected_dates[1] & date <= !!selected_dates[2])
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

        debug_log(class(data))

        if ('tbl_lazy' %in% class(data) & convert_lazy) {
          data %>% dplyr::collect() %>%
            dplyr::mutate(date = lubridate::ymd(date))
        } else {
          data
        }


      })

      # Create a subset of plot data to use for caching ----
      unique_data <- reactive({
        if ('tbl_lazy' %in% class(plot_data())) {
          out <- data |>
            utils::head() |>
            as.data.frame() |>
            purrr::map(dplyr::type_sum) |>
            purrr::discard(~ .x %in% c('dbl','int')) |>
            purrr::imap( ~ data %>%
                      dplyr::summarize(values = distinct(!!dplyr::sql(.y))) %>%
                      dplyr::collect() %>%
                      dplyr::pull()
            )

        } else {
          out <- subset(plot_data(),
                        select = sapply(plot_data(),
                                        function(x) !inherits(x, c("integer","numeric"))))

          out <- lapply(out, unique)
        }

        if ('date' %in% names(out)) {
          out$date <- lubridate::ymd(out$date)
        }

        out
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
        )
      }) %>%
        shiny::bindCache(
          id,
          unique_data(),
          plot_args() #,
          # body(plot_function)
        )

      static_plot_nolabs <- reactive({
        req(static_plot()) %>%
          djprtheme::remove_labs()
      }) %>%
        shiny::bindCache(
          id,
          unique_data(),
          plot_args() #,
          # body(plot_function)
        )

      # Extract title, subtitle, and caption as HTML ----
      output$title <- renderText({
        extract_labs(static_plot(), "title")
      }) %>%
        shiny::bindCache(
          id,
          unique_data(),
          plot_args() #,
          # body(plot_function)
        )

      output$subtitle <- renderText({
        extract_labs(static_plot(), "subtitle")
      }) %>%
        shiny::bindCache(
          id,
          unique_data(),
          plot_args() #,
          # body(plot_function)
        )

      output$caption <- renderText({
        extract_labs(static_plot(), "caption")
      }) %>%
        shiny::bindCache(
          id,
          unique_data(),
          plot_args() #,
          # body(plot_function)
        )

      # Render plot ------


      # Render non-interactive plot -----
      if (!interactive) {
        output$plot <- renderPlot({
          req(static_plot_nolabs())
          p <- static_plot_nolabs() %>%
            djprtheme::gg_font_change("VIC-font")


          theme_mod <- theme(text = element_text(
            family = "VIC-font",
            size = 14
          ))

          if (inherits(p, "patchwork")) {
            p <- p &
              theme_mod
          } else {
            p <- p +
              theme_mod
          }

          p
        }) %>%
          shiny::bindCache(
            unique_data(),
            plot_args(),
            id #,
            # body(plot_function)
          )
      }

      # Render girafe object -------
      if (interactive) {

        # Capture changes in browser size -----
        window_width <- reactive({
          req(plt_change())
          # Round down to nearest 50 pixels; prevent small resizing
          floor(plt_change()$width / 50) * 50
        })

        width_perc <- reactive({
          # When the window is narrow, the column width ( plt_change()$width )
          # will equal the full browser width ( plt_change()$browser_width). In
          # that case, we want the plot to fill the whole column. We max out
          # at <100 width, to prevent ggiraph objects overflowing their
          # containers and forcing a re-render
          req(plt_change())
          if (plt_change()$width == plt_change()$browser_width) {
            min(92, width_percent * 1.9)
          } else {
            min(92, width_percent)
          }
        })

        girafe_width <- reactive({
          req(window_width(), width_perc())

          calc_girafe_width(
            width_percent = width_perc(),
            window_width = window_width(),
            dpi = 72
          )
        })

        girafe_height <- calc_girafe_height(
          height_percent = height_percent,
          base_height = 400,
          dpi = 72
        )

        output$plot <- ggiraph::renderGirafe({
          req(
            static_plot_nolabs(),
            girafe_width()
          )

          djpr_girafe(
            ggobj = static_plot_nolabs(),
            width = girafe_width(),
            height = girafe_height
          )
        }) %>%
          shiny::bindCache(
            unique_data(),
            plot_args(),
            girafe_width(),
            id,
            interactive
          )

      }

      if (download_button) {
        download_server(
          id = "download_dropdown",
          plot = static_plot(),
          plot_name = id
        )
      } else {
        removeUI(selector = paste0("#", NS(id, "download_col")))
      }
    }
  )
}
