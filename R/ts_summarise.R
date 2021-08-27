#' Summarise a data frame containing ABS time series
#' @name ts_summarise
#' @param df Data frame containing ABS time series
#' @param digits Number of digits to round numbers to
#' @return Data frame with the following columns
#' \itemize{
#'   \item {series_id}{ Unique ABS time series ID}
#'   \item {series}{ Name of the ABS time series}
#'   \item {indicator}{ Fragment of the series that refers to the thing that is
#'   being measured (eg. 'Unemployment rate'). Only defined for the LFS.}
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
#'   \item {up_is_good}{ Logical. `TRUE` if an increase in value is 'good',
#'   `FALSE` if it is 'bad', NA if it is undefined or neither good nor bad. Only defined for the LFS.}
#' }
#'
#'
#' @export
ts_summarise <- function(df, digits = 1) {
  df <- df %>%
    dplyr::select(dplyr::all_of(c(
      "series_id",
      "series",
      "date",
      "value",
      "frequency",
      "unit"
    )))

  # We want the actual numbers - so "3000" should be "3000", not "3"
  df <- df %>%
    # Need to deal with units like "000" and "000 hours" - anything that
    # doesn't fit this pattern is left as-is
    dplyr::mutate(value = dplyr::if_else(grepl("000", unit, fixed = TRUE),
      round(.data$value * 1000, 0),
      .data$value
    ))

  # Some data is quarterly, some is monthly; we split them up and calculate
  # annual change separately. Could also do this with slider::slide_period()?
  # Or with padr::pad()

  split_df <- df %>%
    split(df$frequency)

  change_by_freq <- function(df, freq = c("month", "quarter")) {
    freq <- match.arg(freq)
    freq_num <- dplyr::if_else(freq == "month", 12L, 4L)

    df %>%
      dplyr::group_by(.data$series, .data$series_id) %>%
      dplyr::arrange(.data$date) %>%
      dplyr::mutate(
        d_year_abs = .data$value - dplyr::lag(.data$value, freq_num),
        d_year_perc = 100 * ((.data$value / dplyr::lag(.data$value, freq_num)) - 1)
      ) %>%
      dplyr::ungroup()
  }

  split_df <- purrr::map2(
    .x = split_df,
    .y = tolower(names(split_df)),
    .f = change_by_freq
  )

  comb_df <- dplyr::bind_rows(split_df)

  comb_df <- comb_df %>%
    dplyr::group_by(.data$series, .data$series_id) %>%
    dplyr::arrange(.data$date) %>%
    dplyr::mutate(
      d_period_abs = .data$value - dplyr::lag(.data$value),
      d_period_perc = 100 *
        ((.data$value / dplyr::lag(.data$value)) - 1),
      prev_value = dplyr::lag(.data$value, 1),
      prev_date = dplyr::lag(.data$date, 1),
      min_date = min(.data$date)
    ) %>%
    dplyr::ungroup()

  # Calc change since COVID
  comb_df <- comb_df %>%
    dplyr::filter(.data$date <= as.Date("2020-03-14")) %>%
    dplyr::group_by(.data$series_id) %>%
    dplyr::filter(.data$date == max(.data$date)) %>%
    dplyr::select(.data$series_id,
      precovid_date = .data$date
    ) %>%
    dplyr::right_join(comb_df, by = "series_id") %>%
    dplyr::group_by(.data$series, .data$series_id) %>%
    dplyr::mutate(
      precovid_value = .data$value[.data$date == .data$precovid_date],
      d_sincecovid_perc = 100 *
        ((.data$value / .data$precovid_value) - 1)
    ) %>%
    dplyr::select(-.data$precovid_date) %>%
    dplyr::ungroup()

  # Calculate percentiles of value and change variables
  ptile_df <- comb_df %>%
    dplyr::select(c(
      dplyr::all_of(c("series_id", "value")),
      dplyr::starts_with("d_")
    )) %>%
    dplyr::group_by(.data$series_id) %>%
    # Note that because the data is grouped, everything() doesn't include series_id
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      dplyr::cume_dist
    )) %>%
    dplyr::rename_with(
      .fn = ~ paste0("ptile_", .x),
      .cols = -dplyr::one_of(c("series_id", "value"))
    ) %>%
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
    dplyr::mutate(
      latest_period = format(.data$date, "%B %Y"),
      latest_year = format(.data$date, "%Y"),
      prev_period = format(.data$prev_date, "%B %Y")
    )


  # We want values and changes to 1 digit
  comb_df <- comb_df %>%
    dplyr::mutate(dplyr::across(
      c(
        dplyr::contains("value"),
        dplyr::starts_with("d_")
      ),
      ~ round(.x, 2)
    ))

  # For certain series, we manually define whether an increase in the value
  # is 'good' (eg. an increase in unemployment is not good, an increase in
  # employment is good)
  comb_df <- djprdashdata::lfs_lookup %>%
    dplyr::select(.data$series_id, .data$indicator) %>%
    dplyr::right_join(comb_df, by = "series_id")

  up_is_good <- dplyr::tribble(
    ~indicator, ~up_is_good,
    "Employed full-time", TRUE,
    "Employed part-time", TRUE,
    "Employed total", TRUE,
    "Unemployed looked for full-time work", FALSE,
    "Unemployed looked for only part-time work", FALSE,
    "Unemployed total", FALSE,
    "Labour force total", TRUE,
    "Unemployment rate", FALSE,
    "Participation rate", TRUE,
    "Unemployment rate looked for full-time work", FALSE,
    "Unemployment rate looked for only part-time work", FALSE,
    "Employment to population ratio", TRUE,
    "Not in the labour force (NILF)", NA,
    "Civilian population aged 15 years and over", NA,
    "Civilian population aged 15-24 years", NA,
    "Civilian population aged 15-19 years", NA,
    "Civilian population aged 15-64 years", NA,
    "Monthly hours worked in all jobs", TRUE,
    "Monthly hours worked in all jobs (employed full-time)", TRUE,
    "Monthly hours worked in all jobs (employed part-time)", TRUE,
    "Quarterly hours worked in all jobs", TRUE,
    "Underemployed total", FALSE,
    "Underemployment ratio (proportion of employed)", FALSE,
    "Underemployment rate (proportion of labour force)", FALSE,
    "Underutilisation rate", FALSE,
    "Underemployed full-time (worked part-time for economic reasons)", FALSE,
    "Underemployed part-time (prefer more hours)", FALSE,
    "Underemployed full-time (expanded analytical series)", FALSE,
    "Underemployed part-time (expanded analytical series)", FALSE,
    "Underemployed full-time (prefer more hours)", FALSE,
    "Underemployed part-time (worked less than usual hours for economic reasons)", FALSE,
    "Underemployed total (expanded analytical series)", FALSE,
    "Unemployment rate looked for only part time work", FALSE,
    "Number of hours actually worked in all jobs", TRUE,
    "Hours actually worked in all jobs per employed person", TRUE,
    "Number of hours usually worked in all jobs", TRUE,
    "Weekly hours usually worked in all jobs per employed person", TRUE,
    "Number of actual hours worked in all jobs", TRUE,
    "Hours actually worked in all jobs per capita", TRUE,
    "Number of weeks searching for job", FALSE,
    "Average duration of job search", FALSE,
    "Median duration of job search", FALSE,
    "Long-term unemployed", FALSE,
    "Long-term unemployment ratio", FALSE,
    "Unemployed", FALSE,
    "Employed to population ratio", TRUE,
    "Unemployed looked for both full-time and part-time work", FALSE,
    "Unemployed looked for only full-time work", FALSE,
    "Number of hours sought by unemployed", FALSE,
    "Number of additional hours preferred or hours not worked by underemployed", FALSE,
    "Number of potential hours in the labour force", NA,
    "Volume measure of unemployment rate", FALSE,
    "Volume measure of underemployment rate", FALSE,
    "Volume measure of underutilisation rate", FALSE,
    "Civilian population aged 20-64 years", NA,
    "Engaged in employment and/or study", TRUE,
    "Fully engaged in employment and/or study", TRUE,
    "Partially engaged in employment and/or study", NA,
    "Not engaged in either employment or study", FALSE,
    "Engagement rate", TRUE,
    "Fully engaged rate", TRUE,
    "Civilian population aged 15-29 years", NA,
    "Retrenched (in previous quarter)", FALSE,
    "Employed total (previous quarter)", NA,
    "Retrenchment rate", FALSE
  )

  comb_df <- comb_df %>%
    dplyr::left_join(up_is_good, by = "indicator")

  comb_df %>%
    dplyr::select(-.data$frequency) %>%
    dplyr::rename(
      latest_date = .data$date,
      latest_value = .data$value
    )
}
