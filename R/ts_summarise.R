#' Summarise a data frame containing ABS time series
#' @name ts_summarise
#' @param df Data frame containing ABS time series
#' @param digits Number of digits to round numbers to
#' @return Data frame with the following columns
#' \itemize{
#'   \item {series_id}{ Unique ABS time series ID}
#'   \item {series}{ Name of the ABS time series}
#'   \item {latest_date}{ Date of latest value}
#'   \item {latest_value}{ Latest value}
#'   \item {d_year_abs}{ Change between latest observation and prior year in absolute terms}
#'   \item {d_year_perc}{ Change between latest observation and prior year in percentage terms}
#'   \item {d_period_abs}{ Change between previous and latest period in absolute terms}
#'   \item {d_period_perc}{ Change between previous and latest period in percentage terms}
#'   \item {ptile_latest_value}{ Percentile of latest value within full history}
#'   \item {ptile_d_year_abs}{ Percentile of absolute changee over past year, within full history}
#'   \item {ptile_d_year_perc}{ Percentile of percentage change over past year, within full history}
#'   \item {ptile_d_period_abs}{ Percentile of absolute change over latest period, within full history}
#'   \item {ptile_d_period_perc}{ Percentile of percentage change over latest period, within full history}
#'   \item {latest_period}{ Month of latest value}
#'   \item {latest_year}{ Year of latest value}
#'   \item {prev_date}{ Date of previous period}
#'   \item {prev_value}{ Value in previous period}
#'   \item {prev_period}{ Month of previous period}
#'   \item {min_date}{ Date of earliest observation in data for series}
#'   \item {precovid_value}{ Value on the observation prior to 2020-03-14}
#'   \item {d_sincecovid_perc}{ Percentage change in value since the observation prior to 2020-03-14}
#' }
#'
#'
#' @export
ts_summarise <- function(df, digits = 1) {

  df <- df %>%
    dplyr::select(dplyr::all_of(c("series_id",
                                  "series",
                                  "date",
                                  "value",
                                  "frequency",
                                  "unit"))
                  )

  # We want the actual numbers - so "3000" should be "3000", not "3"
  df <- df %>%
    # Need to deal with units like "000" and "000 hours" - anything that
    # doesn't fit this pattern is left as-is
    dplyr::mutate(value = dplyr::if_else(grepl("000", unit, fixed = TRUE),
                           round(value * 1000, 0),
                           value))

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
      dplyr::mutate(d_year_abs = .data$value - dplyr::lag(.data$value, freq_num),
             d_year_perc = 100 * ((.data$value / dplyr::lag(.data$value, freq_num)) - 1)
             ) %>%
      dplyr::ungroup()
  }

  split_df <- purrr::map2(.x = split_df,
              .y = tolower(names(split_df)),
              .f = change_by_freq)

  comb_df <- dplyr::bind_rows(split_df)

  comb_df <- comb_df %>%
    dplyr::group_by(.data$series, .data$series_id) %>%
    dplyr::arrange(.data$date) %>%
    dplyr::mutate(d_period_abs = .data$value - dplyr::lag(.data$value),
                  d_period_perc = 100 *
                    ((.data$value / dplyr::lag(.data$value)) - 1),
                  prev_value = dplyr::lag(.data$value, 1),
                  prev_date = dplyr::lag(.data$date, 1),
                  min_date = min(.data$date)) %>%
    dplyr::ungroup()

  # Calc change since COVID
  comb_df <- comb_df %>%
    dplyr::filter(.data$date <= as.Date("2020-03-14")) %>%
    dplyr::group_by(.data$series_id) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::select(.data$series_id,
                  precovid_date = .data$date) %>%
    dplyr::right_join(comb_df, by = "series_id") %>%
    dplyr::group_by(.data$series, .data$series_id) %>%
    dplyr::mutate(precovid_value = .data$value[.data$date == .data$precovid_date],
                  d_sincecovid_perc = 100 *
             ((.data$value / .data$precovid_value) - 1)) %>%
    dplyr::select(-precovid_date) %>%
    dplyr::ungroup()

  # Calculate percentiles of value and change variables
  ptile_df <- comb_df %>%
    dplyr::select(c(dplyr::all_of(c("series_id", "value")),
                    dplyr::starts_with("d_"))) %>%
    dplyr::group_by(.data$series_id) %>%
    # Note that because the data is grouped, everything() doesn't include series_id
    dplyr::mutate(dplyr::across(dplyr::everything(),
                                dplyr::cume_dist)) %>%
    dplyr::rename_with(.fn = ~paste0("ptile_", .x),
                       .cols = -one_of(c("series_id", "value"))) %>%
    dplyr::rename(ptile_latest_value = .data$value) %>%
    dplyr::slice_tail(n = 1) %>%
    dplyr::ungroup()

  # Filter to latest date
  comb_df <- comb_df %>%
    dplyr::group_by(.data$series, .data$series_id) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::ungroup()

  # Join in percentile values
  comb_df <- comb_df %>%
    dplyr::left_join(ptile_df, by = "series_id")

  comb_df <- comb_df %>%
    dplyr::mutate(latest_period = format(.data$date, "%B %Y"),
                  latest_year = format(.data$date, "%Y"),
                  prev_period = format(.data$prev_date, "%B %Y"))


  # We want values and changes to 1 digit
  comb_df <- comb_df %>%
    dplyr::mutate(across(c(contains("value"),
                           dplyr::starts_with("d_")),
                         ~round(.x, 2)))

  comb_df %>%
    dplyr::select(-.data$frequency) %>%
    dplyr::rename(latest_date = .data$date,
           latest_value = .data$value)

}
