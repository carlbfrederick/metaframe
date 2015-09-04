##' Generic function to build a codebook from an R object
##'
##' Build an HTML codebook from an R object that includes metadata about the 
##' data stored in the object and its creation. For some object types it will 
##' include auto-generated summary information about the data in the fields. 
##' @usage skeleton(data, file, replace)
##' @param data a data object to construct a codebook for
##' @param file a character string indicating where the file should be saved, if 
##' NULL default is `codebook`
##' @param replace logical, should an existing codebook file be overwritten, default 
##' is FALSE
##' @param render logical, should the codebook be opened after it is written? default 
##' is FALSE
##' @return An HTML file with codebook information
##' @export codebook
##' @rdname codebook
##' @author Carl Frederick
skeleton <- function(data, file=NULL, replace=FALSE){
  UseMethod("skeleton")
}



#' @title Build a meta.data skeleton from a data.frame
#' @rdname skeleton
#' @method skeleton data.frame
#' @importFrom tools file_ext
#' @export
skeleton.data.frame <- function(data, file=NULL, replace=FALSE) {
  varNames <- c("DATA", colnames(data))
  colNames <- c("variable", "labels", "units", "notes", "sources.name", "sources.year", 
                "sources.citation", "sources.notes", "revisions")
  md_skel <- as.data.frame(setNames(replicate(length(colNames),
                                             rep(" ", length(varNames)), 
                                             simplify = F), colNames), 
                          row.names = 1:length(varNames))
  
  md_skel$variable <- varNames
  numSum <- meta.summary(data)$numericSummary
  numSum$variable <- row.names(numSum)
  md_skel <- merge(md_skel, numSum, by = "variable",all.x=TRUE)
  
  if(!is.null(file)){
    if(tools::file_ext(file)!= "csv"){
      warning("File other than csv specified, overriding to filename codebook.csv")
      file <- "codebook.csv"
    }
  }
  
  if (is.null(file)) {
    print(md_skel)
  } else {
    if (file.exists(file)) {
      if (replace) {
        message(paste("Overwriting", file))
        write.csv(md_skel, file = file, row.names = FALSE, col.names = TRUE)
      } else {
        stop(paste("File", file, "already exists. Set replace = TRUE to overwrite"))
      }
    } else {
      write.csv(md_skel, file = file, row.names = FALSE, col.names = TRUE)
    }
  }
}


#' @title Read in an external metadata file formatted by skeleton
#' @keywords internal
skel_reader <- function(file){
  md_skel <- read.csv(file = file)
  
}
