% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/djpr_plot_async.R
\name{djpr_async_ui}
\alias{djpr_async_ui}
\title{Asynchronous DJPR plot server}
\usage{
djpr_async_ui(id, ..., width = 6L, height = "500px")
}
\arguments{
\item{id}{a Shiny \code{outputId} specific to the individual plot.}

\item{...}{additional HTML elements to be placed inside the box, such as
inputs}

\item{width}{Numeric value between 1 and 12. Follows \code{shiny::column} and
bootstrap with sizing conventions.}

\item{height}{CSS height}
}
\value{
HTML element
}
\description{
Asynchronous DJPR plot server
}
