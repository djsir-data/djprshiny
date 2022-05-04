#' Asynchronous DJPR plot server
#'
#' @param id output namespace id to be used in conjunction with `djpr_plot_ui`
#' or `djpr_plot_box`
#' @param plot_fun a function which generates a ggplot
#' @param ... arguments passed to plot_fun
#'
#' @return
#' @export
djpr_async_server <- function(
  id,
  plot_fun,
  ...
){

  # Check inputs
  if(!is.function(plot_fun)) stop("plot_fun must be a function")
  if(is.na(as.character(id))) stop("id must be character")
  if(length(as.character(id)) != 1) stop("id must be length 1")

  # Generate outputs in namespace
  shiny::moduleServer(
    id = id,
    function(input, output, session){

      # Ensure reactive inputs are handled correctly
      arg_list <- shiny::reactive(
        lapply(
          list(...),
          function(x) if (shiny::is.reactive(x)) {x()} else {x}
        )
      )

      # Evaluate plot function
      plot <- shiny::reactive({
        req(arg_list())
        promises::future_promise(do.call(plot_fun, arg_list()))
      }) %>%
        shiny::bindCache(plot_fun, arg_list())


      # Establish container width helper div and js
      width_plot_name  <- shiny::NS(id, "plot")
      width_ppi_name   <- shiny::NS(id, "ppi-test")
      width_input_name <- shiny::NS(id, "sizing")

      width_helper <- shiny::tagList(
        shiny::div(id = width_ppi_name, style = "width:0.75in;visible:hidden;padding:0px"),
        shiny::tags$script(
          sprintf(
              '
              $(document).on("shiny:connected", function(e) {
                var w = document.getElementById("%s").width();
                var h = document.getElementById("%s").height();
                var d =  document.getElementById("%s").offsetWidth;
                var obj = {width: w, height: h, dpi: d};
                Shiny.onInputChange("%s", obj);
              });
              $(window).resize(function(e) {
                var w = document.getElementById("%s").width();
                var h = document.getElementById("%s").height();
                var d =  document.getElementById("%s").offsetWidth;
                var obj = {width: w, height: h, dpi: d};
                Shiny.onInputChange("%s", obj);
              });
              ',
              width_plot_name,
              width_plot_name,
              width_ppi_name,
              width_input_name,
              width_plot_name,
              width_plot_name,
              width_ppi_name,
              width_input_name
          )
        )
      )

      shiny::observeEvent(
        input,
        {message("UI insertion")
          shiny::insertUI(width_plot_name, "beforeBegin", width_helper) }#,
        # once = TRUE
      )


      # Generate all output
      output$title <- shiny::reactive(
        {
          message("PLot class: ", class(plot()))
          plot() %...>%
            shiny::req() %...>%
            djprtheme::extract_labs("title")
        }
        )
      output$subtitle <- shiny::reactive(
        plot() %...>%
          shiny::req() %...>%
          djprtheme::extract_labs("subtitle")
      )
      output$caption <- shiny::reactive(
        plot() %...>%
          shiny::req() %...>%
          djprtheme::extract_labs("caption")
      )
      output$plot <- ggiraph::renderggiraph({
        update_width <- if(is.null(input[[width_input_name]])) {
          list(width = 500, height = 500, ppi = 72)
        } else {
          input[[width_input_name]]
        }

        promises::future_promise({
          plot() %...>%
            req() %...>%
            djprtheme::remove_labs() %...>%
            ggiraph::ggiraph(
              ggobj = .,
              width_svg = update_width$width / update_width$ppi,
              height_svg = update_width$height / update_width$ppi
            )
        })
      })

      download_server(
        id = "download_dropdown",
        plot = plot(),
        plot_name = id
      )

    }
  )

}





djpr_async_ui <- function(id, ..., plot_height = "400px", width = 6){

  column(
    width = width,
    div(
      class = "box",

      div(
        class = "box-header",
        textOutput(
          NS(id, "title"),
          container = function(x) h3(x, style = "display: inline-block;")
          ),
        download_icon(NS(id, "download_dropdown")),
        textOutput(NS(id, "subtitle"), container = h4),
      ),
      div(
        class = "box-body",
        djpr_with_spinner(
          NS(id, "plot"),
          proxy.height = plot_height,
          hide.ui = FALSE
          ),
        textOutput(
          NS(id, "subtitle"),
          container = function(x) div(x, class = "djpr_caption")
          )
      ),
      div(
        class = "box-footer",
        ...,
      )
    )
  )


}

