#' Modify appearance of Shiny apps consistent with DJPR style
#'
#' This function uses the `bslib` package to create a Shiny theme.
#'
#' @param ... arguments passed to `bslib::bs_theme()`
#'
#' @examples
#' ui <- shiny::navbarPage(title = "Example app!",
#'                 theme = djpr_shiny_theme()
#' )
#'
#' @export

djpr_shiny_theme <- function(...) {
  bslib::bs_theme(
    bootswatch = "lumen",
    version = "4",
    primary = "#1F1547",
    success = "#62BB46",
    info = "#1D9EC3",
    base_font = list(
      bslib::font_google("Roboto Slab"),
      "Helvetica Neue",
      "Arial",
      "sans-serif",
      "Apple Color Emoji",
      "Segoe UI Emoji",
      "Segoe UI Symbol"
    ),
    heading_font = list(bslib::font_google("Roboto"),
                        "sans"),
    `enable-shadows` = TRUE,
  ) %>%
    bslib::bs_add_variables("navbar-padding-y" = "spacer / 2")
}
