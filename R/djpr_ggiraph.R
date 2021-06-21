#' Render ggiraph::girafe() object in a Shiny app and resize appropriately
#' with the browser window
#'
#' As a Shiny window  (html container) is resized,
#' ggplot2 objects (rendered using plotOutput())
#' change their widths while retaining their height. By default,
#' ggiraph::girafeOutput() objects don't resize in the same way - they maintain
#' their aspect ratio. These functions render these objects in a way that
#' resizes with the browser window.
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
#' ui <- djpr_page(
#'   title = "Dashboard title",
#'   djpr_tab_panel(
#'     title = "Panel title",
#'     "Some text",
#'     "This is a ggplot2 object",
#'     plotOutput("plot1",
#'       width = "100%"
#'     ),
#'     "This is a standard ggiraph object",
#'     girafeOutput("plot2"),
#'     "This is a ggiraph object that resizes with the window",
#'     girafeOutput("plot3")
#'   )
#' )
#'
#' server <- function(input, output, session) {
#'
#'   # First, we create a plot we can re-use:
#'   static_plot <- reactive({
#'     ggplot(mtcars, aes(x = wt, y = mpg)) +
#'       geom_point()
#'   })
#'
#'   # This code generates a standard static ggplot
#'   output$plot1 <- renderPlot({
#'     static_plot()
#'   })
#'
#'   # This code generates a standard ggiraph that doesn't re-size as we want
#'
#'   output$plot2 <- renderGirafe({
#'     ggiraph::girafe(ggobj = static_plot())
#'   })
#'
#'   # This code generates a ggiraph that resizes with the browser window
#'   output$plot3 <- renderGirafe({
#'     req(input$plt_change)
#'     p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'       ggiraph::geom_point_interactive(aes(tooltip = cyl))
#'
#'
#'     plt_change <- reactive(input$plt_change)
#'     plot_width <- calc_girafe_width(
#'       width_percent = 100,
#'       window_width = plt_change()$width,
#'       dpi = plt_change()$dpi
#'     )
#'
#'     plot_height <- calc_girafe_height(
#'       height_percent = 100,
#'       window_height = plt_change()$height,
#'       dpi = plt_change()$dpi
#'     )
#'
#'     djpr_girafe(p,
#'       width = plot_width,
#'       height = plot_height
#'     )
#'   })
#' }
#' shinyApp(ui, server)
#' }
#' @export
#' @keywords internal
#' @rdname djpr_girafe
#' @param ggobj A ggplot2 object
#' @param height height in inches
#' @param width width in inches
#' @param ... Additional options;
#' passed to `options` argument of `ggiraph::girafe`.
#' @return A `ggiraph::girafe()` object
#' @examples
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point()
#'
#' djpr_girafe(p, 5, 6)
#' @details
#' `djpr_girafe()` should be used within `renderGirafe({})`, in place of
#' `ggiraph::girafe()`, within the `server` component of a Shiny app.
#'
#' `ggiraph_js()` should be called within the Shiny UI, as in
#' `fluidPage(ggiraph_js())`.
#'
djpr_girafe <- function(ggobj,
                        height = 5,
                        width = 6,
                        ...) {
  p <- ggobj
  p$labels$title <- NULL
  p$labels$subtitle <- NULL
  p$labels$caption <- NULL
  p$patches$annotation$title <- NULL
  p$patches$annotation$subtitle <- NULL
  p$patches$annotation$caption <- NULL

  ggiraph::girafe(
    ggobj = p,
    width_svg = width,
    height_svg = height,
    options = list(
      ggiraph::opts_toolbar(saveaspng = FALSE),
      ggiraph::opts_sizing(rescale = FALSE),
      ggiraph::opts_zoom(min = 1, max = 1),
      ggiraph::opts_tooltip(
        delay_mouseover = 100,
        opacity = 0.9,
        css = "background-color: white; color: black; font-family: Roboto, Arial, Helvetica, sans-serif; line-height: 100%;"
      ),
      ...
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
                                    var d = document.getElementById("ppitest").offsetWidth;
                                    var b = window.innerWidth;
                                    var obj = {width: w, height: h, dpi: d, browser_width: b};
                                    Shiny.onInputChange("plt_change", obj);
                                });
                                $(window).resize(function(e) {
                                    var w = document.getElementById("girafe_container").offsetWidth;
                                    var h = $(this).height();
                                    var d =  document.getElementById("ppitest").offsetWidth;
                                    var b = window.innerWidth;
                                    var obj = {width: w, height: h, dpi: d, browser_width: b};
                                    Shiny.onInputChange("plt_change", obj);
                                });
                            ')
  )
}

#' Return width / height in inches for ggiraph::girafe() objects
#' @rdname calc_girafe_size
#' @param width_percent Percentage of the standard plotting area that
#' should be taken up by the object
#' @param height_percent Height of the object as percentage of standard
#' @param window_width Width of the row in which the object will be
#' placed. Given by `plt_change()$width`
#' @param dpi Dots per inch of browser. Given by `plt_change()$dpi`
#' @param max_width Maximum width of object in pixels
#' @param window_height Height of the  browser window. Given by
#' `plt_change()$height`
#' @param perc_of_height The height of the object as a percentage
#' of the browser window height
#' @param min_px The minimum height of the object in pixels
#' @export

calc_girafe_width <- function(width_percent,
                              window_width,
                              dpi,
                              max_width = 1140) {
  min(c(
    max_width,
    window_width
  )) *
    (width_percent / 100) /
    dpi
}

#' @export
#' @rdname calc_girafe_size
calc_girafe_height <- function(height_percent,
                               window_height,
                               dpi,
                               perc_of_height = 40,
                               min_px = 200) {
  max(c(
    window_height * (perc_of_height / 100),
    min_px
  )) * (height_percent / 100) /
    dpi
}
