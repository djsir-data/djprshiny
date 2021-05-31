#' Ensure ggiraph object resizes with browser window
#'
#' As a Shiny window  (html container) is resized,
#' ggplot2 objects (rendered using plotOutput())
#' change their widths while retaining their height. By default,
#' ggiraph::girafeOutput() objects don't resize in the same way. This code,
#' when added to a `fluidPage()` or similar, makes the browser width and
#' height available so that the ggiraph object can scale correctly.
#'
#'
#' @source https://stackoverflow.com/questions/65267602/can-a-ggiraph-interactive-plot-be-the-size-of-the-window-in-r-shiny
#' and https://stackoverflow.com/questions/45191999/ggiraph-plot-doesnt-resize-to-fit-the-page and
#' https://stackoverflow.com/questions/36995142/get-the-size-of-the-window-in-shiny
#' @examples
#' \dontrun{
#' library(shiny)
#' library(ggiraph)
#' library(ggplot2)
#'
#' ui <- fluidPage(
#'   ggiraph_js(),
#'   fluidRow(
#'     "This is a ggplot2 object",
#'     plotOutput("plot1",
#'       width = "100%"
#'     ),
#'     "This is a ggiraph object",
#'     girafeOutput("plot2",
#'       width = "100%",
#'       height = "400px"
#'     ),
#'     "Blergh"
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'   output$plot1 <- renderPlot({
#'     ggplot(mtcars, aes(x = wt, y = mpg)) +
#'       geom_point()
#'   })
#'
#'   output$plot2 <- renderGirafe({
#'     p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'       geom_point_interactive(aes(tooltip = cyl))
#'
#'
#'     djpr_girafe(p, input = input)
#'   })
#' }
#'
#' shinyApp(ui, server)
#' }
#' @export
#' @keywords internal
#' @rdname djpr_girafe
#' @param ggobj A ggplot2 object
#' @param height height in inches
#' @param width width in inches
#' @return A `ggiraph::girafe()` object
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point()
#'
#' djpr_girafe(p, 5, 6)
#'
#' @details
#' `djpr_girafe()` should be used within `renderGirafe({})`, in place of
#' `ggiraph::girafe()`, within the `server` component of a Shiny app. **NOTE**
#' that `djpr_girafe()` is memoised on package load - see `zzz.R`.
#'
#' `ggiraph_js()` should be called within the Shiny UI, as in
#' `fluidPage(ggiraph_js())`.
#'
djpr_girafe <- function(ggobj,
                        height,
                        width) {

  p <- ggobj
  p$labels$title <- NULL
  p$labels$subtitle <- NULL
  p$labels$caption <- NULL
  p$patches$annotation$title <- NULL
  p$patches$annotation$subtitle <- NULL
  p$patches$annotation$caption <- NULL

  ggiraph::girafe(
    ggobj = p,
    width_svg =  width,
    height_svg = height,
    options = list(
      ggiraph::opts_toolbar(saveaspng = FALSE),
      ggiraph::opts_sizing(rescale = FALSE),
      ggiraph::opts_zoom(min = 1, max = 1),
      ggiraph::opts_tooltip(
        delay_mouseover = 100,
        opacity = 0.9,
        css = "background-color: white; color: black; font-family: Roboto, Arial, Helvetica, sans-serif; line-height: 100%;"
      )
    ),
    fonts = list(sans = c("Roboto"))
  )
}

#' Javascript to capture browser resizing
#' @export
#' @param col_widths numeric vector of length 3; elements correspond to
#' Bootstrap widths of 3 columns. Must sum to 12.
#' @name ggiraph_js
#' @rdname djpr_girafe
#' @keywords internal
ggiraph_js <- function(col_widths = c(2, 8, 2)) {
  tagList(
    tags$body(
      shiny::div(id = "ppitest", style = "width:0.75in;visible:hidden;padding:0px"),
      # This is not ideal - the column widths are hard-coded into each
      # sub-page, so that we can have 1 container to use to measure
      # the width of the content column in pixels for resizing ggiraphs
      shiny::fluidRow(
        style = "visible: hidden",
        column(col_widths[1]),
        column(col_widths[2],
          id = "girafe_container"
        ),
        column(col_widths[3])
      )
    ),
    tags$script('$(document).on("shiny:connected", function(e) {
                                    var w = document.getElementById("girafe_container").offsetWidth;
                                    var h = window.innerHeight;
                                    var d =  document.getElementById("ppitest").offsetWidth;
                                    var obj = {width: w, height: h, dpi: d};
                                    Shiny.onInputChange("plt_change", obj);
                                });
                                $(window).resize(function(e) {
                                    var w = document.getElementById("girafe_container").offsetWidth;
                                    var h = $(this).height();
                                    var d =  document.getElementById("ppitest").offsetWidth;
                                    var obj = {width: w, height: h, dpi: d};
                                    Shiny.onInputChange("plt_change", obj);
                                });
                            ')
  )
}
