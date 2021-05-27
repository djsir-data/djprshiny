#' Create a focus box
#'
#' A focus box is a fluidRow() with a small amount of drop shadow and spacing
#' @param ... content for the box
#' @export

focus_box <- function(...) {
  fluidRow(
    style = "padding-left: 15px;border: 1px solid #53565A; box-shadow: 3px 3px 2px #AEAEAE;",
    fluidRow(),
    ...,
    br(),
    fluidRow()
  )
}
