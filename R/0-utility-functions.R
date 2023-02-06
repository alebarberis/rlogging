# File and Connection -----------------------------------------------------

#'Connection Utility Functions
#'
#'@description Functions to check existence, create, open, and close
#'connections.
#'
#'@name connectionUtilityFunctions
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
NULL

#'Connection Utility Functions
#'@describeIn connectionUtilityFunctions Checks the existence of a file path
#'
#'@param path character string, the file path
#'@return A logical value, whether the path to the file exists.
#'@keywords internal
filePathExists <- function(path = NULL){
  if(isTRUE(!missing(path) && !is.null(path) && length(path)>0 && dir.exists(dirname(path)))){
    bool = TRUE;
  } else {
    bool = F;
  }
  return(bool);
}

#'Connection Utility Functions
#'
#'@describeIn connectionUtilityFunctions Checks if \code{con}
#'is a connection.
#'@param con object to be tested
#'@return Returns \code{TRUE} or \code{FALSE} depending on whether
#'its argument is of \code{connection} class or not.
#'@keywords internal
is.connection <- function(con){
  return(isTRUE(sum(grepl(pattern = "connection", x = class(con)))==1))
}

#'Connection Utility Functions
#'
#'@describeIn connectionUtilityFunctions Checks the existence of a
#'file path and returns an opened connection.
#'
#'@param path character string, the file path
#'@return An opened connection.
#'@keywords internal
checkFilePathAndOpenConnection <- function(path = NULL){
  if(isTRUE(filePathExists(path))){
    #Open connection to log and warning files
    con = file(description = path, open = "a");
  } else {
    con = NULL;
    # con = character();
  }
  return(con);
}


#'Connection Utility Functions
#'
#'@describeIn connectionUtilityFunctions Returns \code{TRUE} or
#'\code{FALSE} depending on whether its argument is an opened
#'\code{connection}.
#'
#'@param con object to be tested
#'@return Returns \code{TRUE} or \code{FALSE} depending on whether
#'its argument is an opened \code{connection}.
#'@keywords internal
isOpenConnection <- function(con){

  out = FALSE;

  if(isTRUE(!missing(con) && !is.null(con))){
    if(isTRUE((sum(grepl(pattern = "connection", x = class(con)))==1) && isOpen(con = con, rw = ""))){
      out=TRUE;
    }
  }

  return(out);
}


#'Connection Utility Functions
#'
#'@describeIn connectionUtilityFunctions Closes an
#'opened connection.
#'
#'@param con object to be tested
#'
#'@keywords internal
checkConAndClose <- function(con){
  if(isTRUE(isOpenConnection(con))){
    close(con);
  }
}

# Directory ---------------------------------------------------------------

#'Check the existence of a directory
#'
#'@description This function checks the existence of a directory in input.
#'See the **Details** section below for further information.
#'
#'@param path character string, a directory path
#'
#'@return \code{TRUE} if \code{output} is a path to a directory,
#'\code{FALSE} otherwise.
#'
#'@details The function checks that the path exists.
#'If not, it creates the entire path by using
#'\code{\link[base]{dir.create}} with \code{recursive = TRUE}.
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
dirExists <- function(path){
  if(!missing(path) && !is.null(path)){hasDir = T} else {hasDir = F}
  if(hasDir && !dir.exists(path)){dir.create(path, recursive = TRUE)}

  return(hasDir)
}

# List Available  --------------------------------------------------------------

#'List available levels
#'
#'@description This function lists the available logging levels.
#'See the **Details** section below for further information.
#'
#'@param path character string, a directory path
#'
#'@return A character vector containing the available logging levels.
#'
#'@details
#'Eight levels are supported:
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
#'
#'@author Alessandro Barberis
#'
#'@keywords internal
listAvailableLevels <- function(){
  out = c("FATAL", "ERROR", "WARN", "INFO", "DEBUG", "TRACE", "ALL", "OFF")

  return(out)
}
