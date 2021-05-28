#' Create a focus box
#'
#' A focus box is a fluidRow() with a small amount of drop shadow and spacing
#' @param ... content for the box
#' @export

focus_box <- function(...) {
  div(
    style = "padding-left: 15px; box-shadow: 0px 3px 1px -1px rgba(0, 0, 0, .2), 0px 0px 2px 0px rgba(0, 0, 0, 0.3), 0px 1px 3px 0px rgba(0, 0, 0, 0.12); border-radius: 6px; margin-top: 5px; margin-bottom: 5px; padding: 14px 20px 20px 20px;",
    ...
  )
}
