% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shared_class_functions.r
\name{merge_empty_keys}
\alias{merge_empty_keys}
\title{Merge relation schemas with empty keys}
\usage{
merge_empty_keys(x)
}
\arguments{
\item{x}{a relational schema object, such as a \code{\link{relation_schema}}
or \code{\link{database_schema}} object.}
}
\value{
An R object of the same class as \code{x}, where relations with an
empty key have been merged into a single relation.
}
\description{
Merges an object's schemas with empty keys. The remaining such schema
contains all attributes contained in such schemas.
}
\details{
This function is not itself generic, but makes use of the generic functions
\code{\link{keys}} and \code{\link{merge_schemas}}. Any input class with
valid methods for these generic functions can be passed into this function.

For \code{\link{database_schema}} objects, references involving the
schemas with empty keys are updated to refer to the merged schema.
}
\seealso{
\code{\link{merge_schemas}}, on which this function is based.
}
