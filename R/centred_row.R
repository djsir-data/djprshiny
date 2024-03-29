#' Create a Shiny fluidRow with centred content
#' This is a convenience function intended for use in Shiny apps where you
#' want content centred, surrounded to left and white by whitespace, using
#' `shiny::fluidRow()` and `shiny::column()`.
#'
#' @param content The content you wish to place in the main (middle) column
#' of the row, such as `plotOutput("plot")`. Use `shiny::tagList()` to supply
#' multiple items.
#' @param left_content The content to be placed in the left hand column. `NULL`
#' by default, which means the column will be whitespace.
#' @param right_content The content to be placed in the right hand column. `NULL`
#' by default, which means the column will be whitespace.
#' @param left_space Numeric. Default is `2`. Must be between 1 and 10
#' (inclusive). See `Details`.
#' @param right_space Numeric. Default is equal to `left_space`.
#' Must be between 1 and 10 (inclusive). See `Details`.
#' @param max_width_px Maximum width of content, in pixels.
#' @param ... Additional elements passed to main column. Can be used for
#' attributes such as `id`.
#' @details This function creates a `shiny::fluidRow()` then divides that
#' row into three columns. The left and right columns are whitespace. The
#' content goes in the centre column. Any space not allocated to the left or
#' right space is left for the content.
#'
#' Units of measurement for `left_space` and `right_space` are based on the
#' Bootstrap 12-wide grid system. A value of 2 therefore means that a column
#' will take up 1/6th of the horizontal space on the page.
#'
#' The total width of each column must be at least 1. The total width of
#' the three columns must be equal to 12. Each column therefore cannot be
#' greater than 10 width.
#'
#' @return a `shiny.tag` object that creates a three-column fluidRow.
#' @examples
#'
#' centred_row(shiny::plotOutput("plot1"))
#' @export

centred_row <- function(content,
                        left_content = NULL,
                        right_content = NULL,
                        left_space = 2,
                        right_space = left_space,
                        max_width_px = 1140,
                        ...) {
  if (left_space < 1 || right_space < 1) {
    stop("`left_space` and `right_space` must be 1 or greater.")
  }

  if (left_space + right_space > 11) {
    stop("`left_space` and `right_space` combined must not exceed 11.")
  }

  if (missing(content)) {
    stop("`content` must be included.")
  }

  shiny::fluidRow(
    shiny::column(
      width = left_space,
      left_content
    ),
    shiny::column(12 - left_space - right_space,
      content,
      style = paste0("max-width: ", max_width_px, "px;"),
      ...
    ),
    shiny::column(
      right_space,
      right_content
    )
  )
}
