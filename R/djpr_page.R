djpr_page <- function(title,
                      ...) {
  shiny::fluidPage(

    theme = djpr_shiny_theme(),
    ggiraph_js(),
    toc_header(),

    navbarPage(
      title = title,
      lang = "en",
      position = "fixed-top",
      collapsible = TRUE,
      ...
    )

  )


}
