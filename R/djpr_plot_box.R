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
  interactive = TRUE,
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
      textOutput(NS(id, "title"), container = h3),
      textOutput(NS(id, "subtitle"), container = h4)
    ),
    djpr_with_spinner(plot_ui, proxy.height = plot_height, hide.ui = FALSE),
    ...,
    footer =
      fluidRow(
        shinyBS::bsTooltip(info_id, NS(id, "caption"))),
    column(
      6,
      div(
        id = NS(id, "date_slider_col"),
        sliderInput(
          NS(id, "dates"),
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
      )
    ),
    column(
      5,
      div(
        id = NS(id, "check_box_col"),
        shinyWidgets::awesomeCheckboxGroup(
          NS(id, "checkboxes"),
          label = "",
          choices = NULL,
          selected = NULL,
          inline = TRUE
        )
      )
    ),
    column(
      1,
      div(
        style="width: 100%; display: table; padding-top: 50%;",
        div(
          style="display: table-row",
          div(
            style = "width: 10px; display: table-cell;",
            icon("info"),
            id = info_id
          ),
          div(
            id = NS(id, "download_col"),
            style="display: table-cell;",
            download_icon(NS(id, "download_dropdown")),
          )
        )
      )
    )
  )

}
