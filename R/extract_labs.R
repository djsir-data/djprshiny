
#' Function to extract labels from ggplot2 / patchwork objects
#' @param plot ggplot2 / patchwork object
#' @param lab_type Label type to extract - one of c("title", "subtitle", "caption)
#' @return A length-one character vector
#'
#' @examples
#' library(ggplot2)
#' library(patchwork)
#'
#' plot1 <- ggplot(economics, aes(x = date, y = unemploy)) +
#'   geom_point() +
#'   labs(subtitle = "Plot 1 subtitle")
#'
#' # Extract the subtitle from plot2
#' extract_labs(plot1, "subtitle")
#'
#' # Create a second plot
#' plot2 <- ggplot(economics, aes(x = date, y = uempmed)) +
#'   geom_point() +
#'   labs(subtitle = "Plot 2 subtitle")
#'
#' # Now combine these plots using patchwork
#' comb_plots <- plot1 + plot2 +
#'   plot_annotation(title = "Combined plot title",
#'                   caption = "Data source")
#'
#' extract_labs(comb_plots, "title")
#' @export

extract_labs <- function(plot,
                         lab_type = c("title",
                                      "subtitle",
                                      "caption")) {

  lab_type <- match.arg(lab_type)

  stopifnot(inherits(plot, "gg"))

  if (inherits(plot, "patchwork")) {
    plot$patches$annotation[[lab_type]]
  } else {
    plot$labels[[lab_type]]
  }
}
