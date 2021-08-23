#' Round dates within the month
#'
#' Given a vector of date(s), return the first or last day of the month in
#' which the date occurs
#'
#' @name round_dates
#' @param dates A vector of date(s)
#' @return A vector of date(s) the same length as the input `dates`.
#' @details For each element in `dates`, `date_floor()` returns a date
#' corresponding to the first day of the month in which the `dates` element
#' falls. `date_ceiling()` returns the final day of the month.
#'
#' @examples
#' date_floor(Sys.Date())
#' date_floor(c(Sys.Date(), as.Date("2017-10-18")))
#' date_ceiling(c(Sys.Date(), as.Date("2017-10-18")))
NULL

#' @rdname round_dates
#' @export
date_floor <- function(dates) {
  dates %>%
    strftime(format="%Y-%m") %>%
    paste0("-01") %>%
    as.Date()
}

#' @export
#' @rdname round_dates
date_ceiling <- function(dates) {

  do_date_ceiling <- function(date) {
    seq.Date(from = date,
             by = "months",
             length.out = 2)[2] %>%
      date_floor() - 1
  }

  dates_num <- sapply(dates, do_date_ceiling)

  as.Date(dates_num, origin = "1970-01-01")
}
