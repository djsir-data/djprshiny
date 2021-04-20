#' Functions to create title, subtitle, and caption of plots
#' Creates container
#' Currently implemented via inline CSS.
#' @param rel_font_size Numeric. Relative font size. In percent, so `100` would
#' set the font size equal to the base font size.
#' @param ... arguments passed to container (eg. `div`)
#' @param A `shiny.tag` object.
#' @examples
#'
#' djpr_plot_title("A title goes here")
#'
#' djpr_plot_subtitle("A subtitle goes here", rel_font_size = 100)
#'
#' # Use as container in shiny::textOutput()
#'
#' \dontrun{
#' textOutput("title", container = djpr_plot_title)
#' }
#' @rdname djpr_plot_text
#' @export

djpr_plot_title <- function(...,
                            rel_font_size = 163) {
  h3(...,
     style = paste0("font-size: ",
                    rel_font_size,
                    "%; font-weight: bold; line-height: 1.2; margin-top: 0px; margin-bottom: 1px;"))
}

#' @rdname djpr_plot_text
#' @export
djpr_plot_subtitle <- function(...,
                               rel_font_size = 145) {
  h4(...,
     style = paste0("font-size: ",
                    rel_font_size,
                    "%; font-weight: normal; line-height: 1.1; margin-top: 0px; margin-bottom: 1px;")
  )
}

#' @rdname djpr_plot_text
#' @export
djpr_plot_caption <- function(...,
                              rel_font_size = 73) {
  div(..., style = paste0("font-size: ",
                          rel_font_size,
                          "%; font-weight: normal; line-height: 0.9")
  )
}
