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
    body = shiny::tagAppendChildren(
      body,
      shiny::tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "djprshiny/dashboard.css"
      ),
      shiny::tags$script(
        '$(".logo").prependTo(".main-header .navbar");
        $(".sidebar-toggle").prependTo(".main-header .navbar");'
        )
      ),
    title = title
    )
}
