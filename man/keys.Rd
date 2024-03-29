% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shared_class_functions.r
\name{keys}
\alias{keys}
\alias{keys<-}
\title{Relational data keys}
\usage{
keys(x, ...)

keys(x, ...) <- value
}
\arguments{
\item{x}{an R object. For the given method, a \code{\link{relation_schema}}.}

\item{...}{further arguments passed on to methods.}

\item{value}{A character vector of the same length as \code{keys(x, ...)}.}
}
\value{
A list containing lists of unique character vectors, representing
candidate keys for each schema.
}
\description{
Generic function, with the only given method fetching candidate key lists for
relation schemas.
}
