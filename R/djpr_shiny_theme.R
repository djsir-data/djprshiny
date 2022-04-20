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
    version = "5",
    primary = "#2A6FA2",
    secondary = "#d9d9d6",
    success = "#62BB46",
    warning = "#f3e500",
    info = "#1D9EC3",
    # Browser default is typically 16pt font
    # We want 14 = 0.875 x 16
    "font-size-base" = "0.875rem",
    base_font = list(
      "VIC-font",
      "Helvetica Neue",
      "Arial",
      "sans-serif",
      "sans"
    ),
    heading_font = list(
      "VIC-font",
      "Helvetica Neue",
      "Arial",
      "sans-serif",
      "sans"
    ),
    `enable-shadows` = TRUE,
  ) %>%
    bslib::bs_add_variables(
      "navbar-padding-y" = "spacer / 2",
      "font-family-sans-serif" = '"VIC-font", "Helvetica Neue", "Arial", "sans-serif", "sans"'
    ) %>%
    # Hacky way to add spacing around brand:
    # https://stackoverflow.com/questions/60980409/separate-the-title-from-the-tabpanels-in-navbarpage
    bslib::bs_add_rules(c(
      "
      .shiny-input-checkboxgroup .checkbox-inline {
        font-size: 0.75rem;
      }

      h1, h2, h3 {
        color: #1F1547;
      }

      .irs--shiny .irs-bar {
        border-top: 0px solid #2A6FA2 !important;
        border-bottom: 0px solid #2A6FA2 !important;
        background: #2A6FA2 !important;
      }

      .irs-to {
        background-color: #2A6FA2 !important;
      }

      .irs-from {
        background-color: #2A6FA2 !important;
      }

      .ReactTable .rt-table {
        color: #1F1547;
      }

      .navbar-brand {
        font-weight: 400;
        font-size: 0.75rem;
        line-height: 100%;
        font-family: 'VIC-font', 'Helvetica Neue', 'Arial', 'sans-serif', 'sans';
      }

      .navbar-nav {
        font-weight: 400;
        font-size: 0.925rem;
      }

      .navbar.navbar-default {
        border-bottom-color: rgba(0, 0, 0, 0.1);
        box-shadow: 0 0 transparent,0 0 transparent,1px 2px 4px #00000010;
      }

      .navbar.navbar-default .navbar-brand {
        color: #1F1547
      }

      .navbar.navbar-default ul.nav.navbar-nav>li>a {
        color: #1F1547;
      }

      .navbar.navbar-default ul.nav.navbar-nav>li.active>a {
        color: #1F1547;
        text-decoration: underline;
        text-underline-offset: 0.5em;

      }

      .navbar-default .navbar-nav>li>a:focus, .navbar-default .navbar-nav>li>a:hover {
        text-decoration: underline !important;
        text-underline-offset: 0.5em;
        }

      .nav {
        font-family: 'VIC-font', 'Helvetica Neue', 'Arial', 'sans-serif', 'sans';
      }

      "
    ))
}
