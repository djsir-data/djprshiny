#' Create a button that drops down into two sub-options
#'
#' Used to download chart or chart data. Intended as the UI side of a
#' a Shiny module.
#'
#' @export
#' @param id Shiny module id
#' @param ... arguments passed to `shinyWidgets::dropdownButton()`
#'

download_ui <- function(id, ...) {
  shinyWidgets::dropdownButton(
    circle = FALSE,
    tooltip = FALSE,
    size = "sm",
    status = "default bg-white",
    inline = TRUE,
    djpr_dl_button(
      NS(id, "download_data"),
      "Download data"
    ),
    djpr_dl_button(
      NS(id, "download_plot"),
      "Download plot"
    ),
    label = "Download",
    ...
  )
}


djpr_dl_button <- function(id, label) {
  shiny::downloadButton(
    outputId = id,
    label = label,
    style = "font-size: 0.75rem; font-weight: normal; font-family: 'VIC-font', 'Helvetica Neue', 'Arial', 'sans-serif', 'sans'",
    class = "bg-white",
    icon = shiny::icon("arrow-circle-down")
  )
}

#' Server side of download_ui Shiny module
#' @param id Module id
#' @param plot A ggplot2 object
#' @param plot_name Character vector, to be used in the filename of the
#' saved data/plot
#' @export
download_server <- function(id, plot, plot_name = "plot") {
  moduleServer(id, function(input, output, session) {
    output$download_data <- downloadHandler(
      filename = paste0(plot_name, "_data.csv"),
      content = function(file) {
        data <- req(plot) %>%
          djprtheme::get_plot_data()

        if ("tooltip" %in% names(data)) {
          data <- data[names(data) != "tooltip"]
        }

        utils::write.csv(
          x = data,
          file = file,
          row.names = FALSE
        )
      },
      contentType = "text/csv"
    )

    output$download_plot <- downloadHandler(
      filename = function() {
        paste0(plot_name, "_plot.pptx")
      },
      content = function(file) {
        plot <- djprtheme::gg_font_change(plot, font = "Arial")
        plot <- plot + theme(text = element_text(family = "Arial"))

        djprtheme::without_showtext({
          djprtheme::djpr_save_pptx(
            destination = file,
            plot = plot
          )
        })
      }
    )
  })
}



#' Create an icon that drops down into two sub-options
#'
#' Used to download chart or chart data. Intended as the UI side of a
#' a Shiny module.
#'
#' @export
#' @param id Shiny module id
#' @param ... arguments passed to `shinyWidgets::dropdownButton()`
#'

download_icon <- function(id, ...) {
  shinyWidgets::dropdownButton(
    djpr_dl_button(
      NS(id, "download_data"),
      "Download data"
    ),
    djpr_dl_button(
      NS(id, "download_plot"),
      "Download plot"
    ),
    ...,
    circle = FALSE,
    right = TRUE,
    up = TRUE,
    inline = TRUE,
    icon = icon("download")
  )
}
