#' Wrapper around shinycssloaders::withSpinner() with DJPR defaults
#'
#' This function displays a spinner in place of Shiny UI elements while
#' they are loading / refreshing
#' @param ui_element An element such as `plotOutput()`
#' @param type See \url{https://daattali.com/shiny/shinycssloaders-demo}
#' @param colour Hex code for spinner
#' @param size Numeric; Size relative to default (1 = default)
#' @param hide.ui Logical. When `TRUE`, spinner is shown over a blank space;
#' when `FALSE`, spinner is shown over previously output UI.
#' @param ... arguments passed to `shinycssloaders::withSpinner()`
#' @details Use within UI script
#' @examples
#' \dontrun{
#' plotOutput("plot") %>% djpr_with_spinner()
#' }
#' @export

djpr_with_spinner <- function(ui_element,
                              type = 8,
                              colour = "#2A6FA2",
                              size = 0.8,
                              hide.ui = FALSE,
                              ...) {
  shinycssloaders::withSpinner(
    ui_element = ui_element,
    type = type,
    color = colour,
    size = size,
    hide.ui = hide.ui,
    ...
  )
}
