#' Create a `shiny::fluidPage()` with the DJPR theme
#'
#' Use in place of `shiny::fluidPage()`.
#'
#' Structured as a `navbarPage()`. Provides option to include a table of
#' contents, which must be further defined within individual `tabPanel`(s) -
#' see `?toc_row`.
#'
#' @param title Dashboard title, such as "DJPR Labour Dashboard"
#' @param ... content to include within the `navbarPage()`. The content
#' should typically be one or more `tabPanel`s.
#' @param col_widths Numeric vector of length 3; must sum to 12. Second
#' element of vector defines width of main content.
#' @param logo Filename for logo, which will be placed at RHS of navbar.
#' File should be included in the `www` folder of the Shiny app. If `NULL`,
#' no logo is displayed. Default is included with this package.
#' @param logo_style CSS to style the logo.
#'
#' @export

djpr_page <- function(title,
                      ...,
                      col_widths = c(2, 8, 2),
                      logo = file.path("djprshiny", "spp_data_logo.png"),
                      logo_style = "float:right;width:83px;height:20px;padding-top:0px;") {

  # Add logo to navbar: https://stackoverflow.com/a/50991648/10677884
  logo_panel <- if (!is.null(logo)) {
    tabPanel(
      title = "",
      htmltools::tags$script(HTML("var header = $('.navbar > .container-fluid');
header.append('<div style=\"float:right\"><img src=", logo, " alt=\"alt\" style=", logo_style, "></div>');
    console.log(header)"))
    )
  } else {
    tabPanel(title = "")
  }

  shiny::fluidPage(
    theme = djpr_shiny_theme(),
    ggiraph_js(col_widths = col_widths),
    htmltools::tags$head(
      htmltools::tags$link(
        rel = "stylesheet",
        type = "text/css",
        href = "djprshiny/fonts/vic-font.css"
      ),
      htmltools::tags$style(
        ".leaflet .legend {
                 line-height: 16px;
                 font-size: 12px;
        }

        .leaflet-container {
                 background: #ffffff;
        }
        /* Reset font override on shiny sliders */
        .irs {
          font-family: inherit;
        }
        "
      )
    ),
    toc_header(),
    navbarPage(
      title = title,
      id = "navbarpage",
      lang = "en",
      position = "fixed-top",
      collapsible = TRUE,
      windowTitle = gsub("<br>", " ", title),
      ...,
      header = list(
        # Add logo
        logo_panel,
        # Ensure user is returned to top of page on clicking navbar
        # https://stackoverflow.com/a/44713073/10677884
        tags$script(" $(document).ready(function () {
           $('#navbarpage a[data-toggle=\"tab\"]').on('click', function (e) {
            window.scrollTo(0, 0)
                 });

                 });")
      )
    )
  )
}
