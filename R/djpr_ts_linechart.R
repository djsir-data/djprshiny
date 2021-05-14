
#' Create a standard time series linechart
#'
#' @param data A dataframe containing time series data.
#' Your dataframe is presumed to include, at a minimum:
#' \itemize{
#'    \item{"date"}{"A date column, of class `"Date"`}
#'    \item{"value"}{"A value column, of class `numeric`, containing data to be shown on the y-axis}
#'    \item{col_var}{"A variable to map to colour, which can be specified with the `col_var` argument."}
#' }
#' @param col_var Variable in `data` to map to the colour aesthetic;
#' default is `series`
#' @param dot Logical; `TRUE` by default. When `TRUE`, a filled dot will be shown on the
#' most recent data point.
#' @param label Logical; `TRUE` by default. When `TRUE`, a text label will be
#' added to the right of the most recent data point.
#' @param label_num Variable name (or expression) defining the label to be placed
#' on the chart. Default is `round(value, 1)`. Ignored if `label` is `FALSE`.
#' @param y_labels Supplied to the `labels` argument of
#' `ggplot2::scale_y_continuous()`
#' @param hline Numeric. If non-`NULL` (the default), a horizontal line will
#' be drawn at the data value given. eg. if `hline` = `0`, a `geom_hline()`
#' will be added at y = 0.
#' @return A ggplot2 object
#' @details If a column called 'tooltip' is present, it will be used as the
#' ggiraph tooltip; if not, one will be created.
#'
#' @examples
#' \dontrun{
#' library(tidyverse)
#' data <- djprdashdata::download_abs_ts("6202.0") %>%
#'   dplyr::filter(table_no == "6202005")
#'
#' data <- data %>%
#'   dplyr::filter(
#'     grepl("Employment to population ratio", series),
#'     series_type == "Seasonally Adjusted"
#'   )
#'
#' djpr_ts_linechart(data = data)
#' }
#'
#' @import ggplot2
#' @export

djpr_ts_linechart <- function(data,
                              col_var = .data$series,
                              dot = TRUE,
                              label = TRUE,
                              label_num = round(.data$value, 1),
                              y_labels = ggplot2::waiver(),
                              hline = NULL) {
  max_date <- data %>%
    dplyr::filter(date == max(.data$date))

  if (is.null(data[["tooltip"]])) {
    data <- data %>%
      dplyr::mutate(tooltip = paste0(
        {{ col_var }},
        "\n",
        format(
          .data$date,
          "%b %Y"
        ),
        "\n",
        round(.data$value, 1)
      ))
  }

  p <- data %>%
    ggplot(aes(
      x = .data$date,
      y = .data$value,
      col = {{ col_var }}
    ))

  if (!is.null(hline)) {
    p <- p +
      geom_hline(yintercept = hline,
                 colour = "black")
  }

  p <- p +
    geom_line() +
    scale_colour_discrete(palette = djprtheme::djpr_pal) +
    djprtheme::theme_djpr() +
    theme(axis.title.x = element_blank()) +
    coord_cartesian(clip = "off")

  if (isTRUE(dot)) {
    p <- p +
      geom_point(
        data = max_date,
        fill = "white",
        stroke = 1.5,
        size = 2.5,
        shape = 21
      )
  }

  if (isTRUE(label)) {
    days_in_data <- as.numeric(max(data$date) - min(data$date))

    # If there's only one series (ie. the max_date DF has one row), then
    # we don't want to show the series name (col_var) in the label

    if (nrow(max_date) > 1) {
      lab_df <- max_date %>%
        dplyr::mutate(label = paste0(
          stringr::str_wrap({{ col_var }}, 10),
          "\n",
          stringr::str_wrap({{ label_num }}, 10)
        ))
    } else {
      lab_df <- max_date %>%
        dplyr::mutate(label = stringr::str_wrap({{ label_num }}, 10))
    }

    p <- p +
      ggrepel::geom_label_repel(
        data = lab_df,
        aes(label = label),
        hjust = 0,
        fontface = "bold",
        nudge_x = days_in_data * 0.033,
        label.padding = 0.01,
        label.size = NA,
        lineheight = 0.9,
        point.padding = unit(0, "lines"),
        direction = "y",
        seed = 123,
        show.legend = FALSE,
        min.segment.length = unit(5, "lines"),
        size = 14 / .pt
      ) +
      scale_x_date(
        expand = expansion( # mult = c(0, 0.08)
          add = c(0, days_in_data * 0.18)
        ),
        date_labels = "%b\n%Y"
      ) +
      scale_y_continuous(
        expand = expansion(mult = 0.1),
        labels = y_labels
      ) +
      # theme(plot.margin = unit(c(0.5, 0.1, 0.1, 0.01), "lines")) +
      NULL
  }

  p <- p +
    ggiraph::geom_point_interactive(aes(tooltip = .data$tooltip),
      size = 3,
      colour = "white",
      alpha = 0.01
    )

  p
}
