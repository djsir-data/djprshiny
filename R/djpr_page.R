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
    toc_header(),
    navbarPage(
      title = title,
      lang = "en",
      position = "fixed-top",
      collapsible = TRUE,
      ...
    )
  )
}
