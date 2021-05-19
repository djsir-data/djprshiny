% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/djpr_with_spinner.R
\name{djpr_with_spinner}
\alias{djpr_with_spinner}
\title{Wrapper around shinycssloaders::withSpinner() with DJPR defaults}
\usage{
djpr_with_spinner(ui_element, type = 8, color = "#2A6FA2")
}
\arguments{
\item{ui_element}{An element such as \code{plotOutput()}}

\item{type}{See \link{https://daattali.com/shiny/shinycssloaders-demo}}

\item{colour}{Hex code for spinner}
}
\description{
This function displays a spinner in place of Shiny UI elements while
they are loading / refreshing
}
\details{
Use within UI script
}
\examples{
\dontrun{
plotOutput("plot") \%>\% djpr_with_spinner()
}
}