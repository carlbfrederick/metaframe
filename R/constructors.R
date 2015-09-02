#' @title Add source information to data in R
#' @param object a dataset or meta.data object to append with source information
#' @param src a properly formatted source description to append, a named list
#' @description This function will add source data to a data object in R. If 
#' source data already exists, it will modify the source data appropriately. 
#' If source data does not exist, it will convert the object to a metaframe, 
#' and append the source data correctly. 
#' @export
add_source <- function(object, src) UseMethod("add_source")


#' @title Add source information to a meta.data object
#' @describeIn add_source
#' @export
add_source.meta.data <- function(object, src){
#   if(is.null(object@sources)){
#     object@sources <- src
#   } else{
#     
#   }
#   
  
}



#' @title Add source information to a data.frame
#' @describeIn add_source
#' @export
add_source.data.frame <- function(object, src){
  data <- metaframe(data)
  attr(data, "sources") <- src
  return(data)
}

# Sources should either be a list with elements = ncols data + 1, or a subset of that 
# that specify which columns attributed to which source


#' @title Add source information to a meta.frame
#' @describeIn add_source
#' @export
add_source.metaframe <- function(data, src){
  if(is.null(attr(data, "sources"))){
    attr(data, "sources") <- src
    return(data)
  } else {
    tmp <- attr(data, "sources")
    src <- c(tmp, src) # hack to fix later
    attr(data, "sources") <- src
    return(data)
  }
}

#' @title Add a description to data in R
#' @param data a dataset to append with source information
#' @param descr a character vector describing the data
#' @description This function will add a character description to a data object 
#' in R. If a description already exists, it will be replaced. If a description 
#'  does not exist, it will convert the object to a metaframe, and append the 
#'  description
#' @export
add_description <- function(data, src) UseMethod("add_description")


#' @title Add a description to a data.frame
#' @describeIn add_description
#' @export
add_description.data.frame <- function(data, descr){
  data <- metaframe(data)
  attr(data, "description") <- descr
  return(data)
}


#' @title Add a description to a data.frame
#' @describeIn add_description
#' @export
add_description.metaframe <- function(data, descr){
  if(is.null(attr(data, "description"))){
    attr(data, "description") <- descr
    return(data)
  } else {
    tmp <- attr(data, "description")
    src <- c(tmp, descr) # hack to fix later
    attr(data, "description") <- descr
    return(data)
  }
}


#' @title Add annotation information to data in R
#' @param data a dataset to modify
#' @param ann a list containing annotation descriptions
#' @description This function will add annotations to the dataset. Annotations 
#' should be non-data descriptions of dataset features such as observation dates, 
#' log information, or equipment changes in collecting data. 
#' @export
add_annotation <- function(data, ann) UseMethod("add_annotation")

#' @title Add annotations to a data.frame
#' @describeIn add_annotation
#' @export
add_annotation.data.frame <- function(data, ann){
  data <- metaframe(data)
  attr(data, "annotations") <- ann
  return(data)
}


#' @title Add annotations to a metaframe
#' @describeIn add_annotation
#' @export
add_annotation.metaframe <- function(data, ann){
  if(is.null(attr(data, "annotations"))){
    attr(data, "annotations") <- ann
    return(data)
  } else {
    tmp <- attr(data, "annotations")
    src <- c(tmp, ann) # hack to fix later
    attr(data, "annotations") <- ann
    return(data)
  }
}

#' @title Add information about revisions to data in R
#' @param data a dataset to modify
#' @param revs a list containing descriptions of revisions
#' @description This function will add information about revisions to a dataset. 
#' Revisions are descriptions of changes made to the dataset including  changing 
#' modifications to values of a particular row, column, or cell. 
#' @export
add_revisions <- function(data, revs) UseMethod("add_revisions")



#' @title Add revisions to a data.frame
#' @describeIn add_revisions
#' @export
add_revisions.data.frame <- function(data, revs){
  data <- metaframe(data)
  attr(data, "revisions") <- revs
  return(data)
}


#' @title Add revisions to a metaframe
#' @describeIn add_revisions
#' @export
add_revisions.metaframe <- function(data, revs){
  if(is.null(attr(data, "revisions"))){
    attr(data, "revisions") <- revs
    return(data)
  } else {
    tmp <- attr(data, "revisions")
    src <- c(tmp, revs) # hack to fix later
    attr(data, "revisions") <- revs
    return(data)
  }
}
