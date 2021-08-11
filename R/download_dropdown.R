#' Create a button that drops down into two sub-options
#'
#' Used to download chart or chart data. Intended as the UI side of a
#' a Shiny module.
#'
#' @export
#' @param id Shiny module id
#' @param ... arguments passed to `shinyWidgets::dropdownButton()`
#'

download_ui <- function(id, ...) {
  shinyWidgets::dropdownButton(
    circle = FALSE,
    tooltip = FALSE,
    size = "sm",
    status = "default bg-white",
    inline = TRUE,
    icon = shiny::icon("arrow-circle-down"),
    shiny::downloadButton(NS(id, "download_data"),
      "Download data",
      style = "font-weight: normal; font-family: 'Roboto', 'Helvetica Neue', 'Arial', 'sans-serif', 'sans'",
      class = "bg-white",
      icon = shiny::icon("arrow-circle-down")
    ),
    shiny::downloadButton(NS(id, "download_plot"),
      "Download plot",
      style = "font-weight: normal; font-family: 'Roboto', 'Helvetica Neue', 'Arial', 'sans-serif', 'sans'",
      class = "bg-white",
      icon = shiny::icon("arrow-circle-down")
    ),
    label = "Download",
    ...
  )
}

#' Server side of download_ui Shiny module
#' @param id Module id
#' @param plot A ggplot2 object
download_server <- function(id, plot) {
  moduleServer(id, function(input, output, session) {
    output$download_data <- downloadHandler(

      filename = paste0(id, "_data.csv"),

      content = function(file) {
        req(plot)

        data <- djprtheme::get_plot_data(plot)

        if ("tooltip" %in% names(data)) {
          data <- dplyr::select(data, -.data$tooltip)
        }

        utils::write.csv(
          x = data,
          file = file,
          row.names = FALSE
        )
      },

      contentType = "text/csv"
    )

    output$download_plot <- downloadHandler(
      filename = function() {
        paste0(id, "_plot.pptx")
      },
      content = function(file) {
        plot <- djprtheme::gg_font_change(plot, font = "Arial")
        plot <- plot + theme(text = element_text(family = "Arial"))

        djprtheme::djpr_save_pptx(
          destination = file,
          plot = plot
        )
      }
    )

  })
}

