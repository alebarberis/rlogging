% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/1-log-functions.R
\name{logTrace}
\alias{logTrace}
\title{Log Method}
\usage{
logTrace(object, message, sep = "", add.level = FALSE, add.time = FALSE)
}
\arguments{
\item{object}{an object of class \code{\link{Logger}}}

\item{message}{a character string, the message to print}

\item{sep}{a character vector of strings to append after \code{message}}

\item{add.level}{logical, whether to add the log level to \code{message}}

\item{add.time}{logical, whether to add the time to \code{message}}
}
\value{
None (invisible \code{NULL}).
}
\description{
This function prints the log information of level \code{TRACE}.
}
\details{
A log information of level \code{TRACE} is printed if the provided logger
has a >= log level.
}
\examples{
#create logger
logger = createLogger(level = "TRACE")

#log
logTrace(object = logger, message = 'Hello')

#log with level
logTrace(object = logger, message = 'Hello', add.level = TRUE)

#log with time
logTrace(object = logger, message = 'Hello', add.level = TRUE, add.time = TRUE)

}
\seealso{
\code{\link{logAll}},
\code{\link{logDebug}},
\code{\link{logInfo}},
\code{\link{logWarn}},
\code{\link{logError}},
\code{\link{logFatal}}
}
\author{
Alessandro Barberis
}
