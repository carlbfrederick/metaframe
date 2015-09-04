##' Generic function to build a codebook from an R object
##'
##' Build an HTML codebook from an R object that includes metadata about the 
##' data stored in the object and its creation. For some object types it will 
##' include auto-generated summary information about the data in the fields. 
##' @usage codebook(data, file, replace, render)
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
codebook <- function(data, file=NULL, replace=FALSE, render=FALSE){
  UseMethod("codebook")
}



#' @title Build a codebook from a meta.data object
#' @rdname codebook
#' @method codebook meta.data
#' @export
codebook.meta.data <- function(object, file=NULL, replace=FALSE, render=FALSE) {
  if (!inherits(object, "meta.data")) {
    stop("not a meta.data object")
  }
  varnames <- object@var_names
  nvars <- length(varnames)
  mf.name <- object@Rname
  
  if (is.null(file)) {
    fname <- tempfile("codebook", fileext = ".Rmd")
  } else {
    if (file.exists(file)) {
      if (replace) {
        message(paste("Overwriting", file))
        file.create(file)
      } else {
        stop(paste("File", file, "already exists. Set replace = TRUE to overwrite"))
      }
    } else {
      file.create(file)
    }
    fname <- file(description = file, open = "r+")
  }
  
  header <- c("---", paste('title: "Codebook for `', mf.name, '`"', sep=""), paste("date:", date()),
              "output: ", "  html_document:", "    number_sections: no", "    toc: yes", "    toc_depth: 2", "---", "\n")
  
  cat(header, sep="\n", file=fname, append=FALSE)
  
  #Fill in data level information
  varnames <- c("DATA", varnames)
  nvars <- nvars + 1
  
  
  #Cycle through (remember the two trailing spaces!)
  for (i in 1:nvars) {
    idx <-  varnames[i]
    cb.name <-   paste("## `", varnames[i], "`  \n\n")
    cb.label <-  paste("> **Label:  **", object@labels[[idx]], "  ")
    cb.type <-   paste("> **Type:   **", ifelse(varnames[i]=="DATA",  "data.frame", 
                                                object@summary$classes[[idx]]), "  ")
    cb.source <- paste("> **Source Name: **", object@sources[[idx]]$Name, "  ")
    cb.source <- paste("> **Source Year: **", object@sources[[idx]]$Year, "  ")
    cb.source <- paste("> **Source Citation: **", object@sources[[idx]]$Citation, "  ")
    cb.source <- paste("> **Source Notes: **", object@sources[[idx]]$Notes, "  ")
    cb.unit <-   paste("> **Units:  **", object@units[[idx]], "  ")
    cb.desc <-   paste(ifelse(varnames[i]=="DATA", 
                              paste("**Metaframe Label**  \n", object@labels$OVERALL, "  \n\n"), 
                              "**Label**  \n"),
                       ifelse(is.null(object@notes[idx]), 
                              "No notes.  \n\n", 
                              paste(object@notes[idx], "\n\n")),
                       ifelse(is.null(object@revisions[idx]), 
                              "No revisions.  \n\n", 
                              paste(object@revisions[idx], "\n\n")),
                       ifelse(varnames[i]=="DATA", paste("Source: ", object@sources[["OVERALL"]], "  \n"), ""))
    
    if (varnames[i]=="") {
      cat(c(cb.desc, "\n"), 
          sep="\n", file=fname, append=TRUE)
    } else  {
      cat(c("---- \n", 
            cb.name, cb.label, cb.type, cb.source, cb.unit, "\n", 
            cb.desc, "\n",
            "**Summary**  \nInfo in here", "\n"), sep="\n", file=fname, append=TRUE)
    }
  }
  
  if (is.null(file)) {
    if (render) browseURL(rmarkdown::render(fname))
    unlink(fname)
  }
  if (!is.null(file)) {
    if (render) browseURL(rmarkdown::render(file))
    close(fname)
  }
}
