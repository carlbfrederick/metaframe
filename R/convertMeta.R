#' Function to convert existing data.frame attributes made by selected data import packages
#' 
#' \code{convertMeta} takes converts a data.frame (or similar) object produced by
#' some common functions (i.e. the \code{haven} and \code{foreign} packages) to
#' a meta.frame object and promotes the attributes to the proper meta.data 
#' places.
#' 
#' Currently, the list of compatible packages includes 
#' \itemize{ 
#'   \item{\code{foreign}}
#'   \item{\code{haven}}
#'   \item{\code{readstata13}}
#'   \item{\code{memisc}}
#' }
#' 
#' 
#' @param object A data.frame-type object
#' @param from.package A method specifying the function/package that generated
#'   `object`
#' @return a meta.frame object
#' @seealso \code{\link[foreign]{read.dta}}, \code{\link[foreign]{read.spss}},
#'          \code{\link[haven]{read_dta}}, \code{\link[haven]{read_spss}}, 
#'          \code{\link[haven]{read_sas}}, \code{\link[readstata13]{read.dta13}}
#' @examples 
#' #from foreign
#' require(foreign)
#' tmp <- read.dta("http://www.ats.ucla.edu/stat/stata/examples/mlm_imm/imm23.dta")
#' meta1 <- convertMeta(tmp, from.package="foreign")
#' 
#' #from haven
#' require(haven)
#' tmp2 <- read_dta("http://www.ats.ucla.edu/stat/stata/examples/mlm_imm/imm23.dta")
#' meta2 <- convertMeta(tmp2, from.package="haven")
#' 
#' @export
convertMeta <- function(object, from.package=c("foreign","haven","readstata13")) {
  Names <- c("OVERALL", colnames(object))
  nCols <- ncol(object) + 1
  from.package <- match.arg(from.package)
  if (from.package=="foreign") {
    #Labels
    labels <- as.list(c(attr(object, "datalabel", exact=TRUE), attr(object, "var.labels", exact=TRUE)))
    names(labels) <- Names
    #Notes
    notes <- list(OVERALL=c(paste("Imported into R via", from.package),
                            paste("Time Stamp:", attr(object, "time.stamp", exact=TRUE)), 
                            paste("Stata Version:", attr(object, "version", exact=TRUE))))
    #Currently Unused foreign attributes
    #  formats
    #  types
    #  val.labels
    #  label.table
    #  expansion.table
    #  missing
    sources <- NULL
    units <- NULL
    revisions <- NULL
  }
  if (from.package=="haven") {
    labels <- as.list(c(OVERALL=NULL, unlist(sapply(object, attr, which="label", exact=TRUE))))
    notes <- list(OVERALL=c(paste("Imported into R via", from.package)))
    sources <- NULL
    units <- NULL
    revisions <- NULL
  }
  if (from.package=="readstata13") {
    cat("Not written yet") 
  }
  return(document(object, labels=labels, notes=notes, sources=sources, units=units, revisions=revisions))
}