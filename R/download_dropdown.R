#' Create a button that drops down into two sub-options
#'
#' Used to download chart or chart data. Intended as the UI side of a
#' a Shiny module.
#'
#' @export
#' @param id Unique identifier used by Shiny module
#' @param ... arguments passed to `shinyWidgets::dropdownButton()`
#'

download_dropdown <- function(id, ...) {
  shinyWidgets::dropdownButton(
    circle = FALSE,
    # tags$style("font-weight: normal"),
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
    label="Download",
    ...
  )
}
