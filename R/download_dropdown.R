#' Create a button that drops down into two sub-options
#'
#' Used to download chart or chart data. Intended as the UI side of a
#' a Shiny module.
#'
#' @export
#' @param session_ns Value should be `session$ns`. This is supplied rather than
#' an `id` directly, as it enables nesting Shiny modules that generate UI.
#' @param ... arguments passed to `shinyWidgets::dropdownButton()`
#'

download_dropdown <- function(session_ns, ...) {
  shinyWidgets::dropdownButton(
    circle = FALSE,
    tooltip = FALSE,
    size = "sm",
    status = "default bg-white",
    inline = TRUE,
    icon = shiny::icon("arrow-circle-down"),
    shiny::downloadButton(session_ns("download_data"),
      "Download data",
      style = "font-weight: normal; font-family: 'Roboto', 'Helvetica Neue', 'Arial', 'sans-serif', 'sans'",
      class = "bg-white",
      icon = shiny::icon("arrow-circle-down")
    ),
    shiny::downloadButton(session_ns("download_plot"),
      "Download plot",
      style = "font-weight: normal; font-family: 'Roboto', 'Helvetica Neue', 'Arial', 'sans-serif', 'sans'",
      class = "bg-white",
      icon = shiny::icon("arrow-circle-down")
    ),
    label = "Download",
    ...
  )
}
