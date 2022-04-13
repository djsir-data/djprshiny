

#' DJPR font css
#'
#' @param ... unused
#' @details Replaces default fonts with DJPR fonts
#' @return HTML link to djpr font css
#' @export

djpr_fonts <- function(...){
  shiny::tags$link(
    rel = "stylesheet",
    type = "text/css",
    href = "djprshiny/djpr_fonts.css"
  )
}
