#' Set up a `shiny::fluidPage()` with DJPR characteristics
#'
#' This function should be used as a drop-in replacement for `shiny::fluidPage`.
#' It sets up a fluidPage with the DJPR Shiny theme, and some Javascript to enable
#' correct resizing of ggiraph objects.
#'
#' @param ... content for the page
#' @param title Title for the page
#' @param theme By default, `djpr_shiny_theme()`
#' @param lang "en" for English
#' @return Fluid page; object of class `shiny.tag.list`
#' @export

djpr_page <- function(...,
                      title = NULL,
                      theme = djpr_shiny_theme(),
                      lang = "en") {

  fluidPage(
    ggiraph_js(),
    ...,
    title = title,
    theme = theme,
    lang = lang
  )
}
