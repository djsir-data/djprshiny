#' Summarise a data frame containing ABS time series
#' @name ts_summarise
#' @param df Data frame containing ABS time series
#' @return Data frame with the following columns
#' \itemize{
#'   \item {series_id}{Unique ABS time series ID}
#'   \item {series}{Name of the ABS time series}
#'   \item {latest_date}{Date of latest value}
#'   \item {latest_value}{Latest value}
#'   \item {d_year_abs}{Change between latest observation and prior year in absolute terms}
#'   \item {d_year_perc}{Change between latest observation and prior year in percentage terms}
#'   \item {d_period_abs}{Change between previous and latest period in absolute terms}
#'   \item {d_period_perc}{Change between previous and latest period in percentage terms}
#'   \item {latest_month}{Month of latest value}
#'   \item {latest_year}{Year of latest value}
#' }
#' @export
ts_summarise <- function(df) {

  df <- df %>%
    dplyr::select(.data$series_id,
                  .data$series,
                  .data$date,
                  .data$value,
                  .data$frequency) %>%
    dplyr::mutate_if(is.factor, as.character)

  # Some data is quarterly, some is monthly; we split them up and calculate
  # annual change separately. Could also do this with slider::slide_period()?

  split_df <- df %>%
    split(.$frequency)

  change_by_freq <- function(df, freq = c("month", "quarter")) {
    freq <- match.arg(freq)
    freq_num <- dplyr::if_else(freq == "month", 12L, 4L)

    df %>%
      dplyr::group_by(.data$series, .data$series_id) %>%
      dplyr::arrange(.data$date) %>%
      mutate(d_year_abs = .data$value - lag(.data$value, freq_num),
             d_year_perc = 100 * ((.data$value / lag(.data$value, freq_num)) - 1)
             ) %>%
      ungroup()
  }

  split_df <- purrr::map2(.x = split_df,
              .y = tolower(names(split_df)),
              .f = change_by_freq)

  comb_df <- bind_rows(split_df)

  comb_df <- comb_df %>%
    dplyr::group_by(.data$series, .data$series_id) %>%
    dplyr::arrange(.data$date) %>%
    dplyr::mutate(d_period_abs = .data$value - lag(.data$value),
                  d_period_perc = 100 *
                    ((.data$value / dplyr::lag(.data$value)) - 1),
                  prev_value = dplyr::lag(.data$value, 1)) %>%
    dplyr::ungroup()


  comb_df <- comb_df %>%
    dplyr::group_by(.data$series, .data$series_id) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::ungroup()

  comb_df <- comb_df %>%
    dplyr::mutate(latest_month = format(.data$date, "%B %Y"),
                  latest_year = format(.data$date, "%Y"))

  comb_df %>%
    select(-.data$frequency) %>%
    rename(latest_date = .data$date,
           latest_value = .data$value)

}
