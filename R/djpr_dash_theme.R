#' DJPR themed dashboard
#'
#' @param header shinydashboard dashboardHead
#' @param sidebar shinydashboard dashboardSidebar
#' @param body shinydashboard dashboardBody
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

#' DJPR dashboard theme
#'
#' @param ... unused
#' @details add anywhere in a shinydashboard body to add DJPR theme and ggiraph
#' javascript
#' @export

djpr_dash_theme <- function(...){
  htmltools::tagList(
    shiny::tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "djprshiny/dashboard.css"
    ),
    shiny::tags$script(
      '$(".logo").prependTo(".main-header .navbar");
        $(".sidebar-toggle").prependTo(".main-header .navbar");'
    ),
    ggiraph_js()
  )
}
