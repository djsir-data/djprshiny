#' @name headings
#' @aliases djpr_h1_box
#' @aliases djpr_h2_box
#' @aliases djpr_h3_box
#' @aliases djpr_h4_box
#'
#' @title DJPR title boxes
#'
#' @param heading text for heading
#' @param ... additional contents for the box
#' @param width width of box on 1 to 12 scale (bootstrap grid)
#' @param colour Colour for the box as css specification
#' @param text_colour colour for text as css specification
#'
#' @return HTML element
#' @export
djpr_h1_box <- function(
  heading,
  ...,
  width = 12,
  colour = "#201547",
  text_colour = "#FFFFFF"
  ){
  column(
    width = width,
    div(
      class = "small-box",
      div(
        class = "inner",
        style = paste0("background-color: ", colour, ";"),
        h1(heading, style = paste0("color: ", text_colour, ";")),
        ...
      )

    )
  )
}


#' @rdname headings
#' @export
djpr_h2_box <- function(
  heading,
  ...,
  width = 12,
  colour = "#201547",
  text_colour = "#FFFFFF"
){
  column(
    width = width,
    div(
      class = "small-box",
      div(
        class = "inner",
        style = paste0("background-color: ", colour, ";"),
        h2(heading, style = paste0("color: ", text_colour, ";")),
        ...
      )

    )
  )
}

#' @rdname headings
#' @export
djpr_h3_box <- function(
  heading,
  ...,
  width = 12,
  colour = "#201547",
  text_colour = "#FFFFFF"
){
  column(
    width = width,
    div(
      class = "small-box",
      div(
        class = "inner",
        style = paste0("background-color: ", colour, ";"),
        h3(heading, style = paste0("color: ", text_colour, ";")),
        ...
      )

    )
  )
}

#' @rdname headings
#' @export
djpr_h4_box <- function(
  heading,
  ...,
  width = 12,
  colour = "#201547",
  text_colour = "#FFFFFF"
){
  column(
    width = width,
    div(
      class = "small-box",
      div(
        class = "inner",
        style = paste0("background-color: ", colour, ";"),
        h4(heading, style = paste0("color: ", text_colour, ";")),
        ...
      )

    )
  )
}

