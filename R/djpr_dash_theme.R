#' DJPR dashboard theme
#'
#' @param ... unused
#' @details add anywhere in a shinydashboard body to add DJPR theme and
#' ggiraph_js()
#' javascript
#' @export

djpr_dash_theme <- function(...){
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
    ),
    column(
      12,
      tags$body(
        shiny::div(id = "ppitest", style = "width:0.75in;visible:hidden;padding:0px"),
        shiny::fluidRow(
          style = "visible: hidden",
          column(12, id = "girafe_container"),
        )
      )
      ),
    tags$script(
    '$(document).on("shiny:connected", function(e) {
                                    var w = document.getElementById("girafe_container").offsetWidth;
                                    var b = window.innerWidth;
                                    var obj = {width: w, browser_width: b};
                                    Shiny.onInputChange("plt_change", obj);
                                });
                                $(window).resize(function(e) {
                                    var w = document.getElementById("girafe_container").offsetWidth;
                                    var b = window.innerWidth;
                                    var obj = {width: w, browser_width: b};
                                    Shiny.onInputChange("plt_change", obj);
                                });
                            ')
    )

}
