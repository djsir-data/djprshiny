#' Create a `shiny::fluidPage()` with the DJPR theme
#'
#' Structured as a `navbarPage()`. Provides option to include a table of
#' contents, which must be further defined within individual `tabPanel`(s) -
#' see `?toc_row`.
#'
#' @param title Dashboard title, such as "DJPR Labour Dashboard"
#' @param ... content to include within the `navbarPage()`. The content
#' should typically be one or more `tabPanel`s.
#' @param col_widths Numeric vector of length 3; must sum to 12. Second
#' element of vector defines width of main content.
#'
#' @export

djpr_page <- function(title,
                      ...,
                      col_widths = c(2, 8, 2)) {
  shiny::fluidPage(
    theme = djpr_shiny_theme(),
    ggiraph_js(col_widths = col_widths),
    # shinyWidgets::chooseSliderSkin("Flat",
    #                                color = "#2A6FA2"
    # ),
    htmltools::tags$link(
      href = "https://fonts.googleapis.com/css?family=Roboto:300,300italic,400,500,600,700,700italic",
      rel = "stylesheet",
      type = "text/css"
    ),
    htmltools::tags$head(
      htmltools::tags$style(
        ".leaflet .legend {
                 line-height: 16px;
                 font-size: 12px;
        }
        "
      )
    ),
    toc_header(),
    navbarPage(
      title = title,
      id = "navbarpage",
      lang = "en",
      position = "fixed-top",
      collapsible = TRUE,
      ...
    )
  )
}
