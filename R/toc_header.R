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
      tags$link(
        rel = "stylesheet",
        href = "https://cdn.rawgit.com/afeld/bootstrap-toc/v1.0.1/dist/bootstrap-toc.min.css"
      ),
      tags$script(src = "https://cdn.rawgit.com/afel
d/bootstrap-toc/v1.0.1/dist/bootstrap-toc.min.js"
      ),
      tags$style(HTML(".sticky-top {
    top: 100px; /* height of header */
}"))
    ),
    tags$body(
      `data-spy` = "scroll",
      `data-target` = "#toc"
    )
  )
}
