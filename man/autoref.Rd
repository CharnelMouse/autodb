% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autoref.r
\name{autoref}
\alias{autoref}
\title{Add foreign key references to a normalised database}
\usage{
autoref(schema)
}
\arguments{
\item{schema}{a \code{\link{relation_schema}} object, as given by
\code{\link{synthesise}}.}
}
\value{
A \code{\link{database_schema}} object, containing the given relation
schemas and the created foreign key references.
}
\description{
Add foreign key references to a normalised database
}
