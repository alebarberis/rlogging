% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/1-log-functions.R
\name{logLines}
\alias{logLines}
\alias{createLogLine1}
\alias{createLogLine2}
\alias{createLogLine3}
\title{Log Lines}
\usage{
createLogLine1(len = 10)

createLogLine2(len = 10)

createLogLine3(len = 10)
}
\arguments{
\item{len}{integer, length of the output line}

\item{object}{a \code{\link{Logger}}}
}
\value{
A character string with form '----------'
}
\description{
Creates a pre-defined log line.

Creates a pre-defined character vector.
}
\section{Functions}{
\itemize{
\item \code{createLogLine2()}: A character string with form '#--------#'

\item \code{createLogLine3()}: A character string with form '##########'

}}
\author{
Alessandro Barberis
}
\keyword{internal}
