## R metaframe S3 objects

#' @title meta.frame
#' @description A data.frame with metadata attached to it
#' @param data a data.frame
#' @export
meta.frame <- function(data, ...) {
  if (!is.data.frame(data)) stop("data must be a data.frame")
  attr(data, "meta.data") <- document(data, ...)
  structure(data.frame(data), class = c("data.frame", "meta.frame"))
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
##' \item{labels - a generic character string describing an object}
##' \item{notes - character strings describing any additional details about specific data elements}
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
  labels = "list", 
  notes = "list", 
  revisions = "list", 
  var_names = "character", 
  obs_names = "character", 
  summary = "list", 
  Rname = "character"),   S3methods=TRUE)

#' @title Setting meta.data from a data.frame object
#' @rdname document
#' @method document data.frame
#' @export
document.data.frame <- function(data, sources = NULL, units=NULL, 
                                labels=NULL, notes=NULL, 
                                revisions=NULL){
  if (!is.data.frame(data)) stop("data must be a data.frame")
  K <- ncol(data) + 1
  if(is.null(sources)){
    srcList <- vector(mode = "list", length = 4)
    names(srcList) <- c("Name", "Year", "Citation", "Notes")
    sources <- rep(list(srcList), K) 
    names(sources) <- c("OVERALL", colnames(data))
  } else if(class(sources) != "list"){
    sources <- as.list(sources)
  }
  if(is.null(labels)){
    labels <- vector(mode = "list", length = K)
    names(labels) <- c("OVERALL", colnames(data))
  } else if(class(labels) != "list"){
    labels <- as.list(labels)
  }
  if(is.null(notes)){
    notes <- list("No notes listed.")
  } else if(class(notes) != "list"){
    notes <- as.list(notes)
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
                     labels = labels, 
                     notes = notes, 
                     revisions = revisions, 
                     var_names = colnames(data), 
                     obs_names = rownames(data), 
                     summary = meta.summary(data), 
                     Rname = deparse(substitute(data)))
  return(outMD)
} 


#' @title Build summary data from a data.frame object
#' @rdname meta.summary
#' @method meta.summary data.frame
#' @export
meta.summary.data.frame <- function(object, n = 5){
  stataMode <- function(x){
    x <- as.character(x)
    z <- table(as.vector(x))
    m <- names(z)[z == max(z)]
      
    if (length(m) == 1){
        return(m)
      }
      return(".")
  }

   numSum <- apply(object, 2, function(x) {t(c(min = min(x, na.rm = TRUE),
                                   Q1 = quantile(x, prob = 0.25, na.rm = TRUE),
                                   median = median(x, na.rm = TRUE),
                                   Q3 = quantile(x, prob = 0.75, na.rm = TRUE),
                                   max =max(x, na.rm = TRUE),
                                   sd = sd(x, na.rm = TRUE),
                                   missing = sum(is.na(x == TRUE)),
                                   nunique = length(unique(x)), 
                                   mode = stataMode(x), 
                                   class = class(x)))})

  numSum <- data.frame(t(numSum), stringsAsFactors = FALSE)
  names(numSum) <- c("min", "Q1", "median", "Q3", "max", "sd", 
                     "missing", "nunique",  "mode", "class")
  numSum[, 1:8] <- apply(numSum[, 1:8], 2, as.numeric)
  
  
  myTab <- function(x, n, order = c("ascending", "descending"), 
                    na.rm=TRUE){
    order <- match.arg(order, several.ok = FALSE)
    z <- table(x)
    if(order == "ascending"){
      z <- z[order(-z)][1:n]
    } else {
      z <- z[order(z)][1:n]
    }
    
    if(na.rm == TRUE){
      z <- z[!is.na(z)]
    }
    return(as.list(z))
  }
  
  highObs <-  apply(object, 2, myTab, n = n, order = "ascend")
  lowObs <- apply(object, 2, myTab, n = n, order = "desce")
  dataDims <- list('rows' = nrow(object), 'cols' = ncol(object), 
                   'uniqueRows' = nrow(object[!duplicated(object),]))
  varClasses <- apply(object, 2, class)
  output <- list("numericSummary" = numSum, 
                 "highObs" = highObs, 
                 "lowObs" = lowObs, 
                 'dims' = dataDims,
                 "classes" = varClasses)
  return(output)
}




##' Generic function to build a meta.data object
##'
##' Define and describe meta.data for a data object in R. Based on the object 
##' structure this function will automatically define some attributes of the data. 
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
##' \item{labels - a generic character string describing an object}
##' \item{notes - character strings describing any additional details about specific data elements}
##' \item{revisions - a list of data revisions}
##' }
##' @note Yadda yadda yadda
##' @export document
##' @rdname document
##' @author Jared E. Knowles
document <- function(data, sources = NULL, units=NULL, 
                     labels=NULL, notes=NULL, revisions=NULL){
  UseMethod("document")
}

##' Generic function to build summary data for a meta.data object
##'
##' Summarize a data object in R. Based on the object 
##' structure this function will automatically define some attributes of the data. 
##' @usage meta.summary(object, ...)
##' @param object a data object to define metadata for
##' @param n the number of unique values to report
##' @return A \code{list} with summary features for an object
##' @note Yadda yadda yadda
##' @export meta.summary
##' @rdname meta.summary
##' @author Jared E. Knowles
meta.summary <- function(object, n){
  UseMethod("meta.summary")
}
