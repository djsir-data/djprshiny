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
#' @export
#' @keywords internal
#' @rdname djpr_girafe
#' @param ggobj A ggplot2 object
#' @param height height in inches
#' @param width width in inches
#' @param ... Additional options;
#' passed to `options` argument of `ggiraph::girafe`.
#' @return A `ggiraph::girafe()` object.
#' @examples
#' \dontrun{
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   labs(title = "Title will be removed in girafe plot")
#'
#' djpr_girafe(p, 5, 6)
#' }
#' @details
#' Note that the title, subtitle, and caption are removed from `ggobj` using
#' `djprtheme::remove_labs()`.
#'
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
  ggiraph::girafe(
    ggobj = ggobj,
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
                                    var b = window.innerWidth;
                                    var obj = {width: w, browser_width: b};
                                    Shiny.onInputChange("plt_change", obj);
                                });
                                $(window).resize(function(e) {
                                    var w = document.getElementById("girafe_container").offsetWidth;
                                    var b = window.innerWidth;
                                    var obj = {width: w, browser_width: b};
                                    Shiny.onInputChange("plt_change", obj);
                                });
                            ')
  )
}

#' Return width in inches for ggiraph::girafe() objects
#' @param width_percent Percentage of the standard plotting area that
#' should be taken up by the object
#' @param window_width Width of the row in which the object will be
#' placed. Given by `plt_change()$width`
#' @param dpi Dots per inch
#' @param max_width Maximum width of object in pixels
#' @export
calc_girafe_width <- function(width_percent,
                              window_width,
                              dpi = 72,
                              max_width = 1140) {
  min(c(
    max_width,
    window_width
  )) *
    (width_percent / 100) /
    dpi
}

#' Calculate the height of a ggiraph object
#' @param height_percent Height of object, as percentage of base_height
#' @param base_height Pixels; default 400
#' @param dpi DPI; default 72
#' @export
calc_girafe_height <- function(height_percent,
                               base_height = 400,
                               dpi = 72) {
  (height_percent / 100) * base_height / dpi
}
