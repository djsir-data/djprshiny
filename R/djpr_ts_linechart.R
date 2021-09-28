
#' Create a standard time series linechart with DJPR characteristics
#'
#' @param data A dataframe containing time series data.
#' Your dataframe is presumed to include, at a minimum:
#' \itemize{
#'    \item{"date"}{"A date column, of class `"Date"`}
#'    \item{"value"}{"A value column, of class `numeric`, containing data to be shown on the y-axis}
#'    \item{"col_var"}{"A variable to map to colour, which can be specified with the `col_var` argument."}
#' }
#' @param y_var Variable in `data` to map to the y aesthetic;
#' default is `value`.
#' @param col_var Variable in `data` to map to the colour aesthetic;
#' default is `series`.
#' @param group_var Variable in `data` to map to the group aesthetic;
#' default is whatever is supplied to `col_var`
#' @param dot Logical; `TRUE` by default. When `TRUE`, a filled dot will be shown on the
#' most recent data point.
#' @param label Logical; `TRUE` by default. When `TRUE`, a text label will be
#' added to the right of the most recent data point.
#' @param label_num Variable name (or expression) defining the label to be placed
#' on the chart. Default is `round2(value, 1)`. Ignored if `label` is `FALSE`.
#' @param y_labels Supplied to the `labels` argument of
#' `ggplot2::scale_y_continuous()`
#' @param hline Numeric. If non-`NULL` (the default), a horizontal line will
#' be drawn at the data value given. eg. if `hline` = `0`, a `geom_hline()`
#' will be added at y = 0.
#' @param n_x_breaks Number of 'pretty' breaks on the x (date) axis.
#' Passed to `scales::breaks_pretty()`.
#' @param x_expand_mult Length one or two numeric vector of padding to be added
#' to the horizontal axis; passed to the `expand` argument of `scale_x_date`
#' @return A ggplot2 object
#' @details If a column called 'tooltip' is present, it will be used as the
#' ggiraph tooltip; if not, one will be created.
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#' library(ggplot2)
#'
#' data <- ggplot2::economics_long
#'
#' data <- data %>%
#'   dplyr::rename(series = variable)
#'
#' djpr_ts_linechart(data = data)
#' }
#'
#' @import ggplot2
#' @export

djpr_ts_linechart <- function(data,
                              y_var = .data$value,
                              col_var = .data$series,
                              group_var = NULL,
                              dot = TRUE,
                              label = TRUE,
                              label_num = round2(.data$value, 1),
                              y_labels = ggplot2::waiver(),
                              hline = NULL,
                              n_x_breaks = 5,
                              x_expand_mult = c(0, 0.18)) {
  date_limits <- c(
    min(data$date),
    max(data$date)
  )

  x_breaks <- djprtheme::breaks_right(
    limits = date_limits,
    n_breaks = n_x_breaks
  )

  max_date <- data[data$date == max(data$date), ]

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
        round2(.data$value, 1)
      ))
  }

  if (is.null(group_var)) {
    p <- data %>%
      ggplot(aes(
        x = .data$date,
        y = {{ y_var }},
        col = {{ col_var }}
      ))
  } else {
    p <- data %>%
      ggplot(aes(
        x = .data$date,
        y = {{ y_var }},
        col = {{ col_var }},
        group = {{ group_var }}
      ))
  }

  if (!is.null(hline)) {
    p <- p +
      geom_hline(
        yintercept = hline,
        colour = "black"
      )
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
        expand = expansion(
          mult = x_expand_mult
        ),
        breaks = x_breaks,
        date_labels = "%b\n%Y"
      ) +
      scale_y_continuous(
        expand = expansion(mult = 0.1),
        labels = y_labels
      )
  }

  p <- p +
    ggiraph::geom_point_interactive(aes(tooltip = .data$tooltip),
      size = 3,
      colour = "white",
      alpha = 0.01
    )

  p
}
