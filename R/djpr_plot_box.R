#' Output plots as shinydashboard boxes
#'
#' @param id a Shiny `outputId` specific to the individual plot.
#' @param interactive Boolean; does this plot use ggiraph
#' @param plot_height height in css spesification
#' @param ... additional items passed to `shinydashboard::box`
#'
#' @return HTML UI component
#' @export
djpr_plot_box <- function(
  id,
  interactive = FALSE,
  plot_height = "400px",
  ...
  ){

  if (interactive) {
    plot_ui <- ggiraph::girafeOutput(NS(id, "plot"), height = plot_height)
  } else {
    plot_ui <- plotOutput(NS(id, "plot"), height = plot_height)
  }

  info_id <- paste(id, "caption", "icon", sep = "-")

  shinydashboard::box(
    title = tagList(
      textOutput(NS(id, "title"), container = djpr_plot_title),
      textOutput(NS(id, "subtitle"), container = djpr_plot_subtitle)
      ),
    djpr_with_spinner(plot_ui, proxy.height = plot_height, hide.ui = FALSE),
    ...,
    footer = fluidRow(
      column(
        8,
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
      column(8, ),
      column(2, tags$div(icon("info"), id = info_id), align = "right"),
      column(2,download_icon(NS(id, "download_dropdown")), align = "right")
    ),
    shinyBS::bsTooltip(info_id, NS(id, "caption"))
  )

}
