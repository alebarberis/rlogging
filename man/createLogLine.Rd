% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/1-log-functions.R
\name{createLogLine}
\alias{createLogLine}
\title{Create a Log Line}
\usage{
createLogLine(start = "#", middle = "-", times = 10, end = start)
}
\arguments{
\item{start}{character vector, the starting string of the line break}

\item{middle}{character vector, the middle part of the line break}

\item{times}{integer vector, the number of times to repeat the \code{middle} string}

\item{end}{character vector, the string ending the line break}
}
\value{
A character string obtained by pasting together \code{start}, \code{middle}, and
\code{end}. If \code{times} is provided, the \code{middle} string is replicated.
}
\description{
Returns a character vector representing a line break.
}
\examples{
createLogLine()

}
\author{
Alessandro Barberis
}
