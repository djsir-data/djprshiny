#' Create a Shiny `fluidRow()` with three columns: table of contents,
#' content, and right space
#'
#' Use in place of `shiny::fluidRow()`. Each page should have a maximum of one
#' `toc_row()` - to create rows within this overall row, use `shiny::fluidRow()`.
#'
#' @export
#' @param ... Elements to include within the main column.
#' @param page_id An ID that uniquely identifies the page on which the TOC row
#' will be placed. Can be any string without spaces, such as "indicators" or
#' "regional-unemployment.
#' @param page_title Title of the page. Can include spaces. Used for the
#' title of the table of contents.
#' @param toc_space Numeric. Minimum is 1, maximum is 10. Proportion of the
#' width of the page that should be given to the table of contents, where
#' the total width of the page = 12. Default is `3`.
#' @param right_space Numeric. Minimum is 1, maximum is 10. Proportion of the
#' width of the page that should be given to the whitespace at the right hand
#' side, where the total width of the page = 12. By default, same as `toc_space`.
#' @param max_width_px Maximum width, in pixels, of the main content column.
#' @details Note that widths of columns are currently hard-coded at page level;
#' changes to row widths in `toc_row` will not affect objects created with
#' `djpr_plot_server()`.

toc_row <- function(...,
                    page_id,
                    page_title = "",
                    toc_space = 2,
                    right_space = toc_space,
                    max_width_px = 1140) {
  if (toc_space < 1 || right_space < 1) {
    stop("`toc_space` and `right_space` must be 1 or greater.")
  }

  if (toc_space + right_space > 11) {
    stop("`toc_space` and `right_space` combined must not exceed 11.")
  }

  main_space <- 12 - right_space - toc_space

  toc_id <- paste0("#toc-", page_id)

  shiny::fluidRow(
    # Left column containing TOC
    shiny::column(
      width = toc_space,
      htmltools::br(),
      htmltools::br(),
      span(page_title,
           class = "sticky-top",
           style = "top: 75px; text-decoration: underline"),
      br(),
      htmltools::tags$nav(
        id = gsub("#", "", toc_id),
        class = "sticky-top"
      ),
      htmltools::tags$script(
        paste0('$(function() {
                        var navSelector = "', toc_id, '";
                        var $myNav = $(navSelector);
                        Toc.init({
                        $nav: $("', toc_id, '"),
                        $scope: $(document.getElementById("', page_id, '"))
                        });
                        $("body").scrollspy({
                          target: navSelector
                        });
                      });')
      )
    ),
    # Main column containing content
    shiny::column(
      width = main_space,
      ...,
      style = paste0("max-width: ", max_width_px, "px;"),
      class = "main-content",
      id = page_id
    ),
    # Right column, blank space
    shiny::column(width = right_space)
  )
}
