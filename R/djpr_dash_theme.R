#' DJPR dashboard theme
#'
#' @param ... unused
#' @details add anywhere in a shinydashboard body to add DJPR theme
#' javascript
#' @export

djpr_dash_theme <- function(...) {
  htmltools::tagList(
    shiny::tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "djprshiny/dashboard.css"
    ),
    shiny::tags$link(
      rel = "stylesheet",
      type = "text/css",
      href = "djprshiny/bs5-card2.css"
    ),
    shiny::tags$script(
      '$(".logo").prependTo(".main-header .navbar");
        $(".sidebar-toggle").prependTo(".main-header .navbar");'
    )
  )
}
