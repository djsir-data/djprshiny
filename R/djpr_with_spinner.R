#' Wrapper around shinycssloaders::withSpinner() with DJPR defaults
#'
#' This function displays a spinner in place of Shiny UI elements while
#' they are loading / refreshing
#' @param ui_element An element such as `plotOutput()`
#' @param type See \url{https://daattali.com/shiny/shinycssloaders-demo}
#' @param colour Hex code for spinner
#' @details Use within UI script
#' @examples
#' \dontrun{
#' plotOutput("plot") %>% djpr_with_spinner()
#' }
#' @export

djpr_with_spinner <- function(ui_element,
                              type = 8,
                              colour = "#2A6FA2") {
  shinycssloaders::withSpinner(
    ui_element = ui_element,
    type = type,
    color = colour,
    size = 0.8,
    hide.ui = FALSE
  )
}
