#' Create a `shiny::fluidPage()` with the DJPR theme
#'
#' Structured as a `navbarPage()`. Provides option to include a table of
#' contents, which must be further defined within individual `tabPanel`(s) -
#' see `?toc_row`.
#'
#' @param title Dashboard title, such as "DJPR Labour Dashboard"
#' @param ... content to include within the `navbarPage()`. The content
#' should typically be one or more `tabPanel`s.
#'
#' @export

djpr_page <- function(title,
                      ...) {

  shiny::fluidPage(

    theme = djpr_shiny_theme(),
    ggiraph_js(),
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
