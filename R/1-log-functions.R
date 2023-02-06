#'@include 0-utility-functions.R
NULL

# Class Constructor -------------------------------------------------------

#'Logger Structure
#'
#'@description The Logger class serves as the core element for the
#'logging system.
#'
#' @param path character vector, a path to the log file to use
#' @param con (optional) connection to a file
#' @param verbose logical, whether R should report extra information on progress
#' @param level character vector, the log level of the logger. Levels are a
#' mechanism to categorise the severity of an event.
#' Eight levels are supported:
#' \describe{
#' \item{\code{"OFF"}}{indicates the logger is inactive}
#' \item{\code{"FATAL"}}{indicates a fatal event in the application that prevents it from running}
#' \item{\code{"ERROR"}}{indicates an error in the application, possibly recoverable}
#' \item{\code{"WARN"}}{indicates an event that might lead to an error}
#' \item{\code{"INFO"}}{indicates informational messages}
#' \item{\code{"DEBUG"}}{indicates general debugging messages}
#' \item{\code{"TRACE"}}{indicates fine-grained debug events}
#' \item{\code{"ALL"}}{all levels are considered}
#' }
#' Levels are considered ordered: \code{OFF < FATAL < ERROR < WARN < INFO < DEBUG < TRACE < ALL}
#'
#' For example, if we set the level of a logger to `DEBUG`, then all `INFO`, `WARN`,
#' `ERROR`, and `FATAL` events will be logged.
#'
#' @return A Logger object.
#'
#' @author Alessandro Barberis
#'
#' @keywords internal
Logger <- function(path, con, verbose, level){

  if(isFALSE(is.character(path))){stop("Error: 'path' must be a character string. Please, check your input.\n")}
  if(isFALSE(is.null(con) || is.connection(con))){stop("Error: 'con' must be 'NULL' or of class 'connection'. Please, check your input.\n")}
  if(isFALSE(is.logical(verbose))){stop("Error: 'verbose' must be a boolean. Please, check your input.\n")}
  if(isFALSE(is.character(level) && (level %in% listAvailableLevels()) )){stop("Error: 'level' must be a valid character string. Please, check your input.\n")}


  return(structure(
    .Data = list(
      path    = path,
      con     = con,
      verbose = verbose,
      level   = level),
    class = "Logger")
  )
}



#' Logger Class Constructor
#'
#' @description
#' Constructor for the object.
#'
#' @details A log request in a Logger of \code{level} **l**
#' is enabled if the level of the request is **<= l**.
#'
#'
#'
#' @inheritParams Logger
#'
#' @return A \code{\link{Logger}} object.
#'
#'@seealso
#'\code{\link{logAll}},
#'\code{\link{logTrace}},
#'\code{\link{logDebug}},
#'\code{\link{logInfo}},
#'\code{\link{logWarn}},
#'\code{\link{logError}},
#'\code{\link{logFatal}}
#'
#' @author Alessandro Barberis
#'
#'@examples
#'#create a logger
#'logger = createLogger(
#'  level = "DEBUG"
#')
#'
#' @export
createLogger <- function(
    path    = character(),
    con     = NULL,
    verbose = TRUE,
    level   = c("FATAL", "ERROR", "WARN", "INFO", "DEBUG", "TRACE", "ALL", "OFF")
){

  #----------------------------------------------------------------------#
  level = match.arg(level)

  #----------------------------------------------------------------------#
  use.path = FALSE

  #----------------------------------------------------------------------#
  if(isTRUE(!is.null(con))){
    #check con
    bool = isOpenConnection(con)
    if(isFALSE(bool)){
      use.path = TRUE
    }
  }

  #----------------------------------------------------------------------#
  if(isTRUE(use.path && !is.null(path) && length(path) > 0)){
    con = checkFilePathAndOpenConnection(path = path)
  }

  #----------------------------------------------------------------------#
  return(
    Logger(path = path, con = con, verbose = verbose, level = level)
  )
}

# Getters and Setters -----------------------------------------------------
#methods

#'Logger Getters and Setters
#'
#'@name loggerGettersAndSetters
#'
#'@param object a \code{\link{Logger}}
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
NULL

#'@describeIn loggerGettersAndSetters Returns the object element
getCon     <- function(object){return(object$con)}
#'@describeIn loggerGettersAndSetters Returns the object element
getVerbose <- function(object){return(object$verbose)}
#'@describeIn loggerGettersAndSetters Returns the object element
getPath    <- function(object){return(object$path)}
#'@describeIn loggerGettersAndSetters Returns the object element
getLevel   <- function(object){return(object$level)}
#'@describeIn loggerGettersAndSetters Returns the updated object
setCon     <- function(object, value){object$con = value; return(object)}
#'@describeIn loggerGettersAndSetters Returns the updated object
setVerbose <- function(object, value){object$con = value; return(object)}
#'@describeIn loggerGettersAndSetters Returns the updated object
setPath    <- function(object, value){object$con = value; return(object)}
#'@describeIn loggerGettersAndSetters Returns the updated object
setLevel   <- function(object, value){object$con = value; return(object)}

# Connection Utility Functions  ------------------------------------------------

#'Connection Utility Functions
#'
#'@description Functions to open and close connections.
#'
#'@name loggerConnectionUtilityFunctions
#'
#'@author Alessandro Barberis
#'
#'@examples
#'logger = createLogger()
#'
#'#open a connection
#'logger = openCon(logger)
#'
#'#close a connection
#'closeCon(logger)
#'
#'
NULL


#'@describeIn loggerConnectionUtilityFunctions Checks if `object` has a
#'connection or a path to a file. If so, returns a \code{\link{Logger}} with an
#'opened connection to a file.
#'
#'@param object an object of class \code{\link{Logger}}
#'
#'@export
openCon <- function(object){
  fpath  = getPath(object = object)
  con    = checkFilePathAndOpenConnection(path = fpath)
  object = setCon(object = object, value = con)
  return(object)
}

#'@describeIn loggerConnectionUtilityFunctions Closes an opened connection.
#'
#'@param object an object of class \code{\link{Logger}}
#'
#'@export
closeCon <- function(object){
  con = getCon(object = object)
  con = checkConAndClose(con = con)
}

# Log Functions  ----------------------------------------------------------

#'Get log levels
#'@description Returns all the log levels included in the selected level
#'@param level character string, a log level
#'@return A vector containing all the levels corresponding to \code{level}
#'@author Alessandro Barberis
#'@keywords internal
getLogLevels = function(
    level = c("OFF", "FATAL", "ERROR", "WARN", "INFO", "DEBUG", "TRACE", "ALL")
){

  level = match.arg(level)

  out = switch(
    level,
    ALL   = c("FATAL", "ERROR", "WARN", "INFO", "DEBUG", "TRACE") ,
    TRACE = c("FATAL", "ERROR", "WARN", "INFO", "DEBUG", "TRACE"),
    DEBUG = c("FATAL", "ERROR", "WARN", "INFO", "DEBUG"),
    INFO  = c("FATAL", "ERROR", "WARN", "INFO"),
    WARN  = c("FATAL", "ERROR", "WARN"),
    ERROR = c("FATAL", "ERROR"),
    FATAL = c("FATAL"),
    OFF   = ""
  )

  return(out)

}

#'Get log levels
#'@description Returns the log \code{level} as a string with fixed length.
#'@param level character string, a log level
#'@return A vector containing all the levels corresponding to \code{level}
#'@author Alessandro Barberis
#'@keywords internal
logLevelNameFixedLength <- function(level){
  out = switch(
    level,
    ALL   = "[ALL]  ",
    TRACE = "[TRACE]",
    DEBUG = "[DEBUG]",
    INFO  = "[INFO] ",
    WARN  = "[WARN] ",
    ERROR = "[ERROR]",
    FATAL = "[FATAL]",
    OFF   = ""
  )
  return(out)
}


#'Log Method
#'
#'@description Generic function to print the log information.
#'
#'@param object an object of class \code{\link{Logger}}
#'@param log.level a character string, the log level of the information
#'@param message a character string, the message to print
#'@param sep a character vector of strings to append after \code{message}
#'@param add.level logical, whether to add the log level to \code{message}
#'@param add.time logical, whether to add the time to \code{message}
#'
#'@return None (invisible \code{NULL}).
#'
#'@seealso
#'\code{\link{logAll}},
#'\code{\link{logTrace}},
#'\code{\link{logDebug}},
#'\code{\link{logInfo}},
#'\code{\link{logWarn}},
#'\code{\link{logError}},
#'\code{\link{logFatal}}
#'
#'@keywords internal
#'
#'@author Alessandro Barberis
logDefault = function(
    object,
    log.level,
    message,
    sep = '',
    add.level = FALSE,
    add.time = FALSE){
  level = getLevel(object)

  if(isTRUE(log.level %in% getLogLevels(level = level))){

    if(isTRUE(add.time)){
      message = paste(Sys.time(), message)
    }

    if(isTRUE(add.level)){
      # message = paste0("[",log.level,"] ", message)
      message = paste(logLevelNameFixedLength(log.level), message)
    }

    printLog(object = object, message = message, sep = sep)
  }
}

#'Log Method
#'
#'@name logAll
#'
#'@description This function prints the log information of level \code{ALL}.
#'
#'@details A log information of level \code{ALL} is printed if the provided logger
#'has a >= log level.
#'
#'@inheritParams logDefault
#'@inherit logDefault return
#'
#'@seealso
#'\code{\link{logAll}},
#'\code{\link{logTrace}},
#'\code{\link{logDebug}},
#'\code{\link{logInfo}},
#'\code{\link{logWarn}},
#'\code{\link{logError}},
#'\code{\link{logFatal}}
#'
#'@inherit logDefault author
#'
#'@examples
#'#create logger
#'logger = createLogger(level = "ALL")
#'
#'#log
#'logAll(object = logger, message = 'Hello')
#'
#'#log with level
#'logAll(object = logger, message = 'Hello', add.level = TRUE)
#'
#'#log with time
#'logAll(object = logger, message = 'Hello', add.level = TRUE, add.time = TRUE)
#'
#'@export
logAll <- function(object, message, sep = '', add.level = FALSE, add.time = FALSE){
  logDefault(object = object, log.level = "ALL", message = message, sep = sep, add.level = add.level, add.time = add.time)
}

#'Log Method
#'
#'@name logTrace
#'
#'@description This function prints the log information of level \code{TRACE}.
#'
#'@details A log information of level \code{TRACE} is printed if the provided logger
#'has a >= log level.
#'
#'@inheritParams logDefault
#'@inherit logDefault return
#'
#'@seealso
#'\code{\link{logAll}},
#'\code{\link{logDebug}},
#'\code{\link{logInfo}},
#'\code{\link{logWarn}},
#'\code{\link{logError}},
#'\code{\link{logFatal}}
#'
#'@inherit logDefault author
#'
#'@examples
#'#create logger
#'logger = createLogger(level = "TRACE")
#'
#'#log
#'logTrace(object = logger, message = 'Hello')
#'
#'#log with level
#'logTrace(object = logger, message = 'Hello', add.level = TRUE)
#'
#'#log with time
#'logTrace(object = logger, message = 'Hello', add.level = TRUE, add.time = TRUE)
#'
#'@export
logTrace <- function(object, message, sep = '', add.level = FALSE, add.time = FALSE){
  logDefault(object = object, log.level = "TRACE", message = message, sep = sep, add.level = add.level, add.time = add.time)
}

#'Log Method
#'
#'@name logDebug
#'
#'@description This function prints the log information of level \code{DEBUG}.
#'
#'@details A log information of level \code{DEBUG} is printed if the provided logger
#'has a >= log level.
#'
#'@inheritParams logDefault
#'@inherit logDefault return
#'
#'@seealso
#'\code{\link{logAll}},
#'\code{\link{logTrace}},
#'\code{\link{logInfo}},
#'\code{\link{logWarn}},
#'\code{\link{logError}},
#'\code{\link{logFatal}}
#'
#'@inherit logDefault author
#'
#'@examples
#'#create logger
#'logger = createLogger(level = "DEBUG")
#'
#'#log
#'logDebug(object = logger, message = 'Hello')
#'
#'#log with level
#'logDebug(object = logger, message = 'Hello', add.level = TRUE)
#'
#'#log with time
#'logDebug(object = logger, message = 'Hello', add.level = TRUE, add.time = TRUE)
#'
#'@export
logDebug <- function(object, message, sep = '', add.level = FALSE, add.time = FALSE){
  logDefault(object = object, log.level = "DEBUG", message = message, sep = sep, add.level = add.level, add.time = add.time)
}

#'Log Method
#'
#'@name logInfo
#'
#'@description This function prints the log information of level \code{INFO}.
#'
#'@details A log information of level \code{INFO} is printed if the provided logger
#'has a >= log level.
#'
#'@inheritParams logDefault
#'@inherit logDefault return
#'
#'@seealso
#'\code{\link{logAll}},
#'\code{\link{logTrace}},
#'\code{\link{logDebug}},
#'\code{\link{logWarn}},
#'\code{\link{logError}},
#'\code{\link{logFatal}}
#'
#'@inherit logDefault author
#'
#'@examples
#'#create logger
#'logger = createLogger(level = "INFO")
#'
#'#log
#'logInfo(object = logger, message = 'Hello')
#'
#'#log with level
#'logInfo(object = logger, message = 'Hello', add.level = TRUE)
#'
#'#log with time
#'logInfo(object = logger, message = 'Hello', add.level = TRUE, add.time = TRUE)
#'
#'@export
logInfo <- function(object, message, sep = '', add.level = FALSE, add.time = FALSE){
  logDefault(object = object, log.level = "INFO", message = message, sep = sep, add.level = add.level, add.time = add.time)
}

#'Log Method
#'
#'@name logWarn
#'
#'@description This function prints the log information of level \code{WARN}.
#'
#'@details A log information of level \code{WARN} is printed if the provided logger
#'has a >= log level.
#'
#'@inheritParams logDefault
#'@inherit logDefault return
#'
#'@seealso
#'\code{\link{logAll}},
#'\code{\link{logTrace}},
#'\code{\link{logDebug}},
#'\code{\link{logInfo}},
#'\code{\link{logError}},
#'\code{\link{logFatal}}
#'
#'@inherit logDefault author
#'
#'@examples
#'#create logger
#'logger = createLogger(level = "WARN")
#'
#'#log
#'logWarn(object = logger, message = 'Hello')
#'
#'#log with level
#'logWarn(object = logger, message = 'Hello', add.level = TRUE)
#'
#'#log with time
#'logWarn(object = logger, message = 'Hello', add.level = TRUE, add.time = TRUE)
#'
#'@export
logWarn <- function(object, message, sep = '', add.level = FALSE, add.time = FALSE){
  logDefault(object = object, log.level = "WARN", message = message, sep = sep, add.level = add.level, add.time = add.time)
}

#'Log Method
#'
#'@name logError
#'
#'@description This function prints the log information of level \code{FATAL}.
#'
#'@details A log information of level \code{FATAL} is printed if the provided logger
#'has a >= log level.
#'
#'@inheritParams logDefault
#'@inherit logDefault return
#'
#'@seealso
#'\code{\link{logAll}},
#'\code{\link{logTrace}},
#'\code{\link{logDebug}},
#'\code{\link{logInfo}},
#'\code{\link{logWarn}},
#'\code{\link{logFatal}}
#'
#'@inherit logDefault author
#'
#'@examples
#'#create logger
#'logger = createLogger(level = "ERROR")
#'
#'#log
#'logError(object = logger, message = 'Hello')
#'
#'#log with level
#'logError(object = logger, message = 'Hello', add.level = TRUE)
#'
#'#log with time
#'logError(object = logger, message = 'Hello', add.level = TRUE, add.time = TRUE)
#'
#'@export
logError <- function(object, message, sep = '', add.level = FALSE, add.time = FALSE){
  logDefault(object = object, log.level = "ERROR", message = message, sep = sep, add.level = add.level, add.time = add.time)
}

#'Log Method
#'
#'@name logFatal
#'
#'@description This function prints the log information of level \code{FATAL}.
#'
#'@details A log information of level \code{FATAL} is printed if the provided logger
#'has a >= log level.
#'
#'@inheritParams logDefault
#'@inherit logDefault return
#'
#'@seealso
#'\code{\link{logAll}},
#'\code{\link{logTrace}},
#'\code{\link{logDebug}},
#'\code{\link{logInfo}},
#'\code{\link{logWarn}},
#'\code{\link{logError}}
#'
#'@inherit logDefault author
#'
#'@examples
#'#create logger
#'logger = createLogger(level = "FATAL")
#'
#'#log
#'logFatal(object = logger, message = 'Hello')
#'
#'#log with level
#'logFatal(object = logger, message = 'Hello', add.level = TRUE)
#'
#'#log with time
#'logFatal(object = logger, message = 'Hello', add.level = TRUE, add.time = TRUE)
#'
#'@export
logFatal <- function(object, message, sep = '', add.level = FALSE, add.time = FALSE){
  logDefault(object = object, log.level = "FATAL", message = message, sep = sep, add.level = add.level, add.time = add.time)
}

#'Create a Log Line
#'
#'@description Returns a character vector representing a line break.
#'
#'@param start character vector, the starting string of the line break
#'@param middle character vector, the middle part of the line break
#'@param times integer vector, the number of times to repeat the `middle` string
#'@param end character vector, the string ending the line break
#'
#'@return A character string obtained by pasting together `start`, `middle`, and
#'`end`. If `times` is provided, the `middle` string is replicated.
#'
#'@author Alessandro Barberis
#'
#'@examples
#'createLogLine()
#'
#'@export
createLogLine <- function(
    start  = "#",
    middle = "-",
    times  = 10,
    end    = start){
  times = as.integer(times)
  if(isFALSE(is.character(start))){stop("'start' must be a character vector.")}
  if(isFALSE(is.character(middle))){stop("'middle' must be a character vector.")}
  if(isFALSE(is.character(end))){stop("'end' must be a character vector.")}
  if(isFALSE(is.integer(times) && times >=0)){
    stop("'times' must be a non-negative integer value.")
  }
  out = paste0(start, paste0(rep(middle, times), collapse = ""), end)

  return(out)
}

#'Log Lines
#'
#'@name logLines
#'
#'@description Creates a pre-defined log line.
#'
#'@param object a \code{\link{Logger}}
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
NULL

#'Create Log Line
#'
#'@name logLines
#'
#'@description Creates a pre-defined character vector.
#'
#'@param len integer, length of the output line
#'
#'@return A character string with form '----------'
#'
#'@inherit logDefault author
#'
#'@keywords internal
createLogLine1 <- function(len  = 10){
  out = createLogLine(start = "-", times = (len-2))
  return(out)
}

#'@describeIn logLines A character string with form '#--------#'
createLogLine2 <- function(len  = 10){
  out = createLogLine(start = "#", times = (len-2))
  return(out)
}

#'@describeIn logLines A character string with form '##########'
createLogLine3 <- function(len  = 10){
  out = createLogLine(start = "#", middle = "#", times = (len-2))
  return(out)
}

#'Log Line
#'
#'@name getLogLine
#'
#'@description Returns a pre-defined character vector.
#'
#'@param line character vector, the type of line to return:
#'\describe{
#'  \item{`"1"`}{character string with form '----------'}
#'  \item{`"2"`}{character string with form '#--------#'}
#'  \item{`"3"`}{character string with form '##########'}
#'}
#'@inheritParams logLines
#'
#'@return A character vector.
#'
#'@inherit logDefault author
#'
#'@export
getLogLine <- function(
    line = c("1", "2", "3"),
    len = 10){
  line = match.arg(line);

  message = switch(
    line,
    "1" = createLogLine1(len = len),
    "2" = createLogLine2(len = len),
    "3" = createLogLine3(len = len)
  )

  return(message)
}

#'@describeIn getLogLine A character string with pre-defined length
#'
#'@export
getShortLogLine <- function(
    line = c("1", "2", "3"),
    len = 42L){
  line = match.arg(line);
  message = getLogLine(line = line, len = len)
  return(message)
}
#'@describeIn getLogLine A character string with pre-defined length
#'
#'@export
getLongLogLine <- function(
    line = c("1", "2", "3"),
    len = 76L){
  line = match.arg(line);
  message = getLogLine(line = line, len = len)
  return(message)
}



# Print Functions ---------------------------------------------------------

#'Logger Print Functions
#'
#'@description This function prints the provided \code{message}
#'to the output defined via \code{object}.
#'
#'@param object a \code{\link{Logger}}
#'@param message character string, the message to print
#'@inheritParams base::cat
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
printLog <- function(object, message, sep){
  con     = getCon(object = object)
  verbose = getVerbose(object = object)

  if(isTRUE(verbose)){
    cat(message, sep = sep);
  }

  if(isTRUE(isOpenConnection(con))){
    cat(message, file = con, sep = sep);
  }

}
