#' Round numbers to specified number of digits
#' This function is a drop-in replacement for `base::round()`
#'
#' Unlike `base::round()`, it does NOT follow the
#'  IEC 60559 standard by rounding numbers ending in .5 to the even
#'  digit. Instead, 0.5 is rounded up.
#' @export
#' @param x a numeric vector of numbers to round
#' @param digits integer indicating the number of decimal places
#' to which `x` should be rounded.
#' @examples
#' # This gives '2'
#' base::round(2.5)
#'
#' # This gives 3
#' round2(2.5)
#' @source https://stackoverflow.com/questions/12688717/round-up-from-5
round2 <- function(x, digits = 0) {
  posneg <- sign(x)
  z <- abs(x) * 10^digits
  z <- z + 0.5 + sqrt(.Machine$double.eps)
  z <- trunc(z)
  z <- z / 10^digits
  z * posneg
}
