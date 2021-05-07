#' Modify appearance of Shiny apps consistent with DJPR style
#'
#' This function uses the `bslib` package to create a Shiny theme.
#'
#' @param ... arguments passed to `bslib::bs_theme()`
#'
#' @examples
#' ui <- shiny::navbarPage(
#'   title = "Example app!",
#'   theme = djpr_shiny_theme()
#' )
#' @export

djpr_shiny_theme <- function(...) {
  bslib::bs_theme(
    bootswatch = "litera",
    version = "4",
    primary = "#2A6FA2",
    secondary = "#d9d9d6",
    success = "#62BB46",
    warning = "#f3e500",
    info = "#1D9EC3",
    # Browser default is typically 16pt font
    # We want 14 = 0.875 x 16
    "font-size-base" = "0.875rem",
    base_font = list(
      bslib::font_google("Roboto"),
      "Helvetica Neue",
      "Arial",
      "sans-serif",
      "sans"
    ),
    heading_font = list(
      bslib::font_google("Roboto"),
      "sans"
    ),
    `enable-shadows` = TRUE,
  ) %>%
    bslib::bs_add_variables("navbar-padding-y" = "spacer / 2")
}
