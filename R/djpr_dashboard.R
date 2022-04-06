#' Title
#'
#' @param header temp
#' @param sidebar temp
#' @param body temp
#' @param title temp
#'
#' @export

djpr_dashboard <- function(header, sidebar, body, title = NULL){
  shinydashboard::dashboardPage(
    header = header,
    sidebar = sidebar,
    body = shiny::tagAppendChild(
      body,
      shiny::includeCSS("inst/www/dashboard.css")
      ),
    title = title
    )
}
