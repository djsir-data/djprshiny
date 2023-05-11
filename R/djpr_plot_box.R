#' DJPR box plot server
#'
#' @param id output namespace id to be used in conjunction with `djpr_plot_ui`
#' or `djpr_plot_box`
#' @param plot_fun a function which generates a ggplot
#' @param ... arguments passed to plot_fun
#' @param input_from_server (experimental) named list of function inputs to be
#' evalued outside a module. currently all input calls are evaluated in a
#' module - that is to say that inputs defined outside the plot namespace are
#' not available. input_from_server evaluates before the module. All inputs
#' used here should be in a reactive wrapper.
#' meaning
#' @examples
#' \dontrun{
#' # Experimental input_from_server use
#' djpr_box_server(
#'   id = "myplot",
#'   plot_fun = viz_make_my_plot,
#'   dates = input$dates,
#'   input_from_server = list(region = reactive(input$region))
#' )
#' }
#' @export
djpr_box_server <- function(id,
                              plot_fun,
                              ...,
                              input_from_server = NULL) {

  # Check inputs
  if (!is.function(plot_fun)) stop("plot_fun must be a function")
  if (is.na(as.character(id))) stop("id must be character")
  if (length(as.character(id)) != 1) stop("id must be length 1")

  arg_list_call <- as.list(match.call(expand.dots = F))[["..."]]
  arg_list_names <- names(arg_list_call)


  # Generate outputs in namespace
  shiny::callModule(
    id = id,
    input_from_server = input_from_server,
    module = function(input, output, session, input_from_server) {

      # evaluate plotfun arguments in module
      arg_list <- shiny::reactive({
        req(input)
        args <- lapply(arg_list_call, eval.parent)
        names(args) <- arg_list_names
        args <- c(args, input_from_server)
        lapply(args, function(x) if (shiny::is.reactive(x)) x() else x)
      })

      # Decrease how often resizing triggers the plot render
      sizing <- shiny::reactive({
        lapply(input$sizing, function(x) shiny::req(as.logical(x)))
        input$sizing
      }) %>%
        shiny::debounce(1000)

      # Evaluate plot function
      plot <- shiny::reactive({
        args <- req(arg_list())
        do.call(plot_fun, args)
      }) %>%
        shiny::bindCache(plot_fun, arg_list())

      # Generate all output
      output$title <- shiny::reactive({
        plot() %>%
          shiny::req() %>%
          djprtheme::extract_labs("title")
      })
      output$subtitle <- shiny::reactive({
        plot() %>%
          shiny::req() %>%
          djprtheme::extract_labs("subtitle")
      })
      output$caption <- shiny::reactive({
        plot() %>%
          shiny::req() %>%
          djprtheme::extract_labs("caption")
      })
      output$plot <- ggiraph::renderGirafe({
        height <- sizing()$height
        width <- sizing()$width
        dpi <- sizing()$dpi
        chart <- plot()

        shiny::req(chart, height, width, dpi)

        chart %>%
          djprtheme::remove_labs() %>%
          ggiraph::giraph(
            ggobj = .,
            width_svg = width / dpi,
            height_svg = height / dpi,
            options = list(
              ggiraph::opts_toolbar(saveaspng = FALSE),
              ggiraph::opts_sizing(rescale = FALSE),
              ggiraph::opts_zoom(min = 1, max = 1),
              ggiraph::opts_tooltip(
                delay_mouseover = 100,
                opacity = 0.9,
                css = "color: black; font-family: VIC-Regular, Arial, Helvetica, sans-serif; line-height: 100%;"
              )
            )
          )
      })

      # Downloads
      output$download_pptx <- shiny::downloadHandler(
        filename = paste0(id, ".pptx"),
        content = function(file){
          plot() %>%
            djprtheme::gg_font_change(font = "Arial") %>%
            {. + theme(text = element_text(family = "Arial"))} %>%
            djprtheme::djpr_save_pptx(file, plot = .) %>%
            djprtheme::without_showtext()
        },
        contentType = "application/vnd.openxmlformats-officedocument.presentationml.presentation"
      )

      output$download_csv <- shiny::downloadHandler(
        filename = paste0(id, ".csv"),
        content = function(file) {
          plot() %>%
            djprtheme::get_plot_data() %>%
            dplyr::select(-dplyr::contains("tooltip")) %>%
            utils::write.csv(file = file, row.names = FALSE)
        },
        contentType = "text/csv"
      )
    }
  )
}








#' DJPR box plot ui
#'
#' @param id a Shiny `outputId` specific to the individual plot.
#' @param ... additional HTML elements to be placed inside the box, such as
#' inputs
#' @param width Numeric value between 1 and 12. Follows `shiny::column` and
#' bootstrap with sizing conventions.
#' @param height CSS height
#'
#' @return HTML element
#' @export
djpr_box_ui <- function(id, ..., width = 6L, height = "500px") {

  # divs IDs and input names used for ggiraph resizing
  ruler_container <- shiny::NS(id, "container")
  ruler_ppi <- shiny::NS(id, "ruler-ppi")
  ruler_input <- shiny::NS(id, "sizing")

  # JS code to create plot-specific resizing
  width_helper <- shiny::tags$script(
    sprintf(
      '$(document).on("shiny:connected", function(e) {
  var w = document.getElementById("%s").offsetWidth;
  var h = document.getElementById("%s").offsetHeight;
  var d =  document.getElementById("%s").offsetWidth;
  var obj = {width: w, height: h, dpi: d};
  Shiny.setInputValue("%s", obj, {priority: "event"});
});
$(window).resize(function(e) {
  var w = document.getElementById("%s").offsetWidth;
  var h = document.getElementById("%s").offsetHeight;
  var d =  document.getElementById("%s").offsetWidth;
  var obj = {width: w, height: h, dpi: d};
  Shiny.setInputValue("%s", obj, {priority: "event"});
});
$(document).on("shiny:inputchanged", function(e) {
  if(e.name === "tabs"){
    var w = document.getElementById("%s").offsetWidth;
    var h = document.getElementById("%s").offsetHeight;
    var d =  document.getElementById("%s").offsetWidth;
    var obj = {width: w, height: h, dpi: d};
    Shiny.setInputValue("%s", obj, {priority: "event"});
  }
});',
      ruler_container,
      ruler_container,
      ruler_ppi,
      ruler_input,
      ruler_container,
      ruler_container,
      ruler_ppi,
      ruler_input,
      ruler_container,
      ruler_container,
      ruler_ppi,
      ruler_input
    )
  )

  shiny::column(
    width = width,
    shiny::div(
      class = "box",
      shiny::div(
        class = "box-header",
        shiny::textOutput(
          outputId = shiny::NS(id, "title"),
          container = shiny::h3
        ),
        shiny::textOutput(
          outputId = shiny::NS(id, "subtitle"),
          container = shiny::h4
        )
      ),
      shiny::div(
        id = ruler_container,
        class = "box-body",
        shiny::div(
          id = ruler_ppi,
          style = "width:0.75in;visible:hidden;padding:0px"
        ),
        djpr_with_spinner(
          ggiraph::girafeOutput(shiny::NS(id, "plot"), height = height),
          proxy.height = height
        )
      ),
      shiny::div(
        class = "box-footer",
        shiny::fluidRow(
          shiny::column(
            9,
            shiny::textOutput(outputId = shiny::NS(id, "caption")) %>%
              shiny::tagAppendAttributes(class = "djpr-caption")
          ),
          shiny::column(
            3,
            shiny::downloadButton(shiny::NS(id, "download_csv"), "Download data"),
            shiny::downloadButton(shiny::NS(id, "download_pptx"), "Download chart")
          )
        ),
        ...
      ),
      width_helper
    )
  )
}
