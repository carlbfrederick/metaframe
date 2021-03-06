% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/constructors.R
\name{add_annotation}
\alias{add_annotation}
\alias{add_annotation.data.frame}
\alias{add_annotation.metaframe}
\title{Add annotation information to data in R}
\usage{
add_annotation(data, ann)

\method{add_annotation}{data.frame}(data, ann)

\method{add_annotation}{metaframe}(data, ann)
}
\arguments{
\item{data}{a dataset to modify}

\item{ann}{a list containing annotation descriptions}
}
\description{
This function will add annotations to the dataset. Annotations
should be non-data descriptions of dataset features such as observation dates,
log information, or equipment changes in collecting data.

Add annotations to a data.frame

Add annotations to a metaframe
}
\section{Methods (by class)}{
\itemize{
\item \code{data.frame}: 

\item \code{metaframe}: 
}}

