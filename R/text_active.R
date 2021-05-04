#' Function to create 'active text'
#'
#' Active text takes a string, inserts numbers and/or words at pre-defined
#' points in the string, and styles the inserted numbers/words with CSS. Active
#' text is intended for use in Shiny apps.
#'
#' @param string A length-one character vector, with XX inserted at points where
#' you want the elements of `numbers` to be inserted
#' @param numbers A vector (numeric or character) of numbers and/or words you
#' want to insert at the points marked XX in `string`.
#' @param colour Background colour for the inserted text (`numbers`)
#' @param alpha Alpha (transparency) for the inserted text (`numbers`), where
#' 100 is no transparency, 0 is full transparency.
#' @param extra_style Additional CSS to style the inserted text, such as
#' `"font-weight: bold;"`.
#'
#' @return Length-one character vector, with inline CSS.
#' @export
#' @examples
#' string <- "The unemployment  rate went up by XX percentage points to XX per cent."
#' numbers <- c(0.2, 6.1)
#' text_active(string, numbers)
#' text_active(string, numbers, colour = "#007b4b", alpha = 0.5)
#' text_active(string, numbers,
#'   colour = "#007b4b", alpha = 0.5,
#'   extra_style = "font-weight: bold;"
#' )
text_active <- function(string,
                        numbers,
                        colour = "#71c5e8",
                        alpha = 25,
                        extra_style = "") {
  stopifnot(length(string) == 1)

  split_string <- strsplit(string, split = " ", fixed = TRUE)[[1]]

  num_insert_points <- sum(grepl("XX", split_string))

  stopifnot(length(numbers) == num_insert_points)

  num_style <- paste0(
    "background-color: ", colour, alpha, "; ",
    extra_style
  )

  styled_string <- gsub("XX",
    htmltools::span("%s", style = num_style),
    string,
    fixed = TRUE
  )

  result <- do.call("sprintf", args = c(styled_string, as.list(numbers)))

  result <- htmltools::HTML(result)

  result
}
