library(shiny)
# https://afeld.github.io/bootstrap-toc/#examples

ui <- djpr_page(
  title = "App title",
    tabPanel(
      title = "First tab",
      id = "tab1",
      br(),
      br(),
      br(),
      fluidRow(
        column(
          3,
          br(),
          br(),
          tags$nav(
            id = "toc1",
            class="sticky-top"
          ),
          tags$script(
            '$(function() {
  var navSelector = "#toc1";
  var $myNav = $(navSelector);
  Toc.init({
  $nav: $("#toc1"),
  $scope: $(document.getElementById("page-1-content"))
  });
  $("body").scrollspy({
    target: navSelector
  });
});'
          ),
        ),
        column(
          9,
          id = "page-1-content",
          h1("Title"),
          paste0(rep("Text goes here", 100), collapse = ""),
          h2("Some subtitle"),
          paste0(rep("Text goes here", 100), collapse = ""),
          h2("Some other subtitle"),
          h1("Title"),
          paste0(rep("Text goes here", 100), collapse = ""),

          h1("Another title"),
          paste0(rep("Text goes here", 100), collapse = ""),

          h2("Subsection A"),
          paste0(rep("Text goes here", 100), collapse = ""),

          h2("Subsection B"),
          paste0(rep("Text goes here", 100), collapse = ""),

          h1("A final title"),
          paste0(rep("Text goes here", 100), collapse = ""),

        )
      )
    ),
    tabPanel(
      title = "Second tab",
      id = "tab2",
      br(),
      br(),
      fluidRow(
        column(
          3,
          tags$nav(
            id = "toc2",
            class = "sticky-top"
          ),
          tags$script(
            '$(function() {
  var navSelector = "#toc2";
  var $toc2 = $(navSelector);
    Toc.init({
  $nav: $("#toc2"),
  $scope: $(document.getElementById("page-2-content"))
  });
  $("body").scrollspy({
    target: navSelector
  });
});'
          )
        ),
        column(
          9,
          id = "page-2-content",
          h1("Unemployment"),
          paste0(rep("Text goes here", 1e3), collapse = ""),
          h1("Underemploykent"),
          paste0(rep("Text goes here", 1e3), collapse = ""),
          h1("Foo bar"),
          paste0(rep("Text goes here", 1e3), collapse = "")
        )
      )
    )
  )


server <- function(input, output, session) {

}

shinyApp(ui, server)
