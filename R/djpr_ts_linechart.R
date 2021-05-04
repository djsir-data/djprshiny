
#' Create a standard time series linechart
#'
#' @param df A dataframe containing time series data.
#' Your dataframe is presumed to include, at a minimum:
#' \itemize{
#'    \item{"date"}{"A date column, of class `"Date"`}
#'    \item{"value"}{"A value column, of class `numeric`, containing data to be shown on the y-axis}
#'    \item{col_var}{"A variable to map to colour, which can be specified with the `col_var` argument."}
#' }
#' @param col_var Variable in `df` to map to the colour aesthetic;
#' default is `series`
#' @param dot Logical; `TRUE` by default. When `TRUE`, a filled dot will be shown on the
#' most recent data point.
#' @param label Logical; `TRUE` by default. When `TRUE`, a text label will be
#' added to the right of the most recent data point.
#' @param label_var Variable name (or expression) defining the label to be placed
#' on the chart. Default is `round(value, 1)`. Ignored if `label` is `FALSE`.
#' @return A ggplot2 object
#'
#' @examples
#' \dontrun{
#' library(tidyverse)
#' df <- djprdashdata::download_abs_ts("6202.0") %>%
#'   dplyr::filter(table_no == "6202005")
#'
#' df <- df %>%
#'   dplyr::filter(
#'     grepl("Employment to population ratio", series),
#'     series_type == "Seasonally Adjusted"
#'   )
#'
#' djpr_ts_linechart(df = df)
#' }
#'
#' @import ggplot2
#' @export

djpr_ts_linechart <- function(df,
                              col_var = .data$series,
                              dot = TRUE,
                              label = TRUE,
                              label_var = round(.data$value, 1)) {
  max_date <- df %>%
    dplyr::filter(date == max(.data$date))

  p <- df %>%
    ggplot(aes(
      x = .data$date,
      y = .data$value,
      col = {{ col_var }}
    )) +
    geom_line() +
    scale_colour_discrete(palette = djprtheme::djpr_pal) +
    djprtheme::theme_djpr() +
    theme(axis.title.x = element_blank())

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
    p <- p +
      ggrepel::geom_text_repel(
        data = max_date,
        aes(label = stringr::str_wrap({{ label_var }}, 10)),
        hjust = 0,
        nudge_x = 300,
        direction = "y",
        min.segment.length = unit(5, "lines"),
        size = 14 / .pt
      ) +
      scale_x_date(
        expand = expansion(mult = c(0, 0.08)),
        date_labels = "%b\n%Y"
      ) +
      scale_y_continuous(expand = expansion(mult = 0.1)) +
      coord_cartesian(clip = "off")
  }

  p
}
