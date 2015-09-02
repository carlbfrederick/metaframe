## R metaframe S3 objects

#' @title meta.frame
#' @description A data.frame with metadata attached to it
#' @param data a data.frame
#' @export
meta.frame <- function(data, ...) {
  if (!is.data.frame(data)) stop("data must be a data.frame")
  attr(data, "meta.data") <- document(data, ...)
  structure(data.frame(data), class = c("meta.frame", "data.frame"))
}

#' "meta.frame" class
#'
#' @name meta.frame-class
#' @aliases meta.frame
#' @family meta.frame
#'
#' @exportClass meta.frame
setOldClass(c("meta.frame", "data.frame"))


#' meta.data data documentation class
#'
#' A meta.data object stores metadata about a given data element in R in a structured 
#' set of lists for easy output and analysis. Most commonly meta.data objects are 
#' attached to a data.frame and stored as a meta.frame. 
#'
##' @name meta.data-class
##' @aliases meta.data-class
##' @docType class
##' @section Objects from the Class: Objects are created by calls to
##' \code{\link{meta.data}}.
##' @details
##' The object has the following items
##' \itemize{
##' \item{sources - a list of character strings describing data sources}
##' \item{units - a list of character strings specifying the units of variables}
##' \item{description - a generic character string describing an object}
##' \item{annotations - character strings describing any additional details about specific data elements}
##' \item{revisions - a list of data revisions}
##' \item{var_names - a character vector of column names in last known order}
##' \item{obs_names - a charadcter vector of row names in last known order}
##' }
##' @seealso \code{\link{meta.frame}}
##' @keywords classes
##' @examples
##'
##' showClass("meta.data")
##' methods(class="meta.data")
#' @export
meta.data <- setClass("meta.data", representation(
  sources = "list",
  units = "list", 
  description = "list", 
  annotations = "list", 
  revisions = "list", 
  var_names = "character", 
  obs_names = "character"),   S3methods=TRUE)

#' @title Setting meta.data from a data.frame object
#' @rdname document
#' @method document data.frame
#' @export
document.data.frame <- function(data, sources = NULL, units=NULL, 
                                description=NULL, annotations=NULL, 
                                revisions=NULL){
  if (!is.data.frame(data)) stop("data must be a data.frame")
  K <- ncol(data) + 1
  if(is.null(sources)){
    sources <- vector(mode = "list", length = K)
    names(sources) <- c("OVERALL", colnames(data))
  } else if(class(sources) != "list"){
    sources <- as.list(sources)
  }
  if(is.null(description)){
    description <- vector(mode = "list", length = K)
    names(description) <- c("OVERALL", colnames(data))
  } else if(class(description) != "list"){
    description <- as.list(description)
  }
  if(is.null(annotations)){
    annotations <- list("No annotations listed.")
  } else if(class(annotations) != "list"){
    annotations <- as.list(annotations)
  }
  if(is.null(revisions)){
    revisions <- list("No revisions listed.")
  } else if(class(revisions) != "list"){
    revisions <- as.list(revisions)
  }
  if(is.null(units)){
    units <- vector(mode = "list", length = K-1)
    names(units) <- colnames(data)
  } else if(class(units) != "list"){
    units <- as.list(units)
  }
  
  outMD <- meta.data(sources = sources,
                     units = units, 
                     description = description, 
                     annotations = annotations, 
                     revisions = revisions, 
                     var_names = colnames(data), 
                     obs_names = rownames(data))
  return(outMD)
} 


##' Generic function to build a meta.data object
##'
##' Define and describe meta.data for a data object in R
##' @usage meta.data(data, ...)
##' @param data a data object to define metadata for
##' @param sources a list of charcter strings describing data sources
##' @param ... optional additional parameters. 
##' @return A \code{\linkS4class{meta.data}} object 
##' @details
##' The object has the following items
##' \itemize{
##' \item{sources - a list of character strings describing data sources}
##' \item{units - a list of character strings specifying the units of variables}
##' \item{description - a generic character string describing an object}
##' \item{annotations - character strings describing any additional details about specific data elements}
##' \item{revisions - a list of data revisions}
##' }
##' @note Yadda yadda yadda
##' @export document
##' @rdname document
##' @author Jared E. Knowles
document <- function(data, sources = NULL, units=NULL, 
                     description=NULL, annotations=NULL, revisions=NULL){
  UseMethod("document")
}
