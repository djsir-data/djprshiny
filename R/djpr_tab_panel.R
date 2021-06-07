#' Creates a `shiny::tabPanel()` that includes a table of contents,
#' using `toc_row()`
#'
#' @param title The title of the panel
#' @param ... The content of the panel, passed to `toc_row()`
#' @param toc_space Numeric; width (out of a total of 12) to be given to the
#' left area that incldues the table of contents
#' @param right_space Numeric; width for the right whitespace; default is
#' the same as `toc_space`
#' @return A `shiny.tag` object; a `tabPanel`.
#' @details Intended for use inside `djpr_page()`.
#' @export
#' @examples
#' \dontrun{
#' library(shiny)
#'
#' example_panel <- djpr_tab_panel(
#'   title = "Example",
#'   h1("A heading"),
#'   "Lorem ipsum",
#'   h2("A subheading"),
#'   "Lorem ipsum",
#'   h1("Another heading")
#' )
#'
#' ui <- djpr_page(
#'     title = "Example",
#'     example_panel
#'   )
#'
#' server <- function(input, output, session) {
#'
#' }
#'
#' shinyApp(ui, server)
#' }
#'
djpr_tab_panel <- function(title,
                           ...,
                           toc_space = 2,
                           right_space = toc_space) {
  page_id <- tolower(title)
  page_id <- gsub(" ", "-", page_id)

  tabPanel(
    title = title,
    ggiraph_js(),
    HTML(""),
    value = paste0("tab-", page_id),
    br(),
    br(),
    br(),
    toc_row(...,
      page_id = page_id,
      toc_space = toc_space,
      right_space = right_space
    ),
    br()
  )
}
