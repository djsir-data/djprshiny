#' Function returns tagList() of items to add
#' to a page to implement the Bootstrap Table of Contents
#' plugin.
#'
#' @source https://afeld.github.io/bootstrap-toc/#options
#' @return A Shiny tagList intended to be added to an HTML page
#'

toc_header <- function() {
  tagList(
    tags$head(

      # CSS for TOC
      includeCSS(system.file("www", "bootstrap-toc.min.css", package = "djprshiny")),
      # tags$link(
      #   rel = "stylesheet",
      #   href = "https://afeld.github.io/bootstrap-toc/dist/bootstrap-toc.min.css"
      # ),

      # JS for TOC
      includeScript(system.file("www", "bootstrap-toc.min.js", package = "djprshiny")),
      # tags$script(
      #   src = "https://afeld.github.io/bootstrap-toc/dist/bootstrap-toc.min.js"
      #   ),

      tags$style(HTML(".sticky-top {
    top: 100px; /* height of header */
      }
"))
    ),
    tags$body(
      `data-spy` = "scroll",
      `data-target` = "#toc"
    )
  )
}
