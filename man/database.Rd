% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/database.r
\name{database}
\alias{database}
\title{Databases}
\usage{
database(relations, references, name = NA_character_)
}
\arguments{
\item{relations}{a \code{\link{relation}} object.}

\item{references}{a list of references, each
represented by a list containing four character elements. In order, the
elements are a scalar giving the name of the child schema, a vector giving
the child attribute names, a scalar giving the name of the parent schema,
and a vector giving the parent attribute names. The vectors must be of the
same length and contain names for attributes present in their respective
schemas, and the parent attributes must form a key, in order.}

\item{name}{a scalar character, giving the name of the database. This name
is used for the resulting graph when using \code{\link{gv.database}}, to
allow for easier combining of graphs into a single diagram if required.}
}
\value{
A \code{database} object, containing \code{relations} with
\code{references} stored in an attribute of the same name. References
are stored with their attributes in the order they appear in their
respective relations.
}
\description{
Enhances a \code{\link{relation}} object with foreign key reference
information.
}
\details{
Unlike \code{\link{relation_schema}} and \code{link{relation}}, and like
\code{\link{database_schema}}, \code{database} is not designed to be
vector-like: it only holds a single database. This adheres to the usual
package use case, where a single data frame is being analysed at a time.
However, it inherits from \code{\link{relation}}, so is vectorised with
respect to its relations.

As with \code{\link{relation}}, duplicate relations, after ordering by
attribute, are allowed, and can be removed with \code{\link{unique}}.

References, i.e. foreign key references, are allowed to have different
attribute names in the child and parent relations; this can't occur in the
output for \code{\link{autoref}} and \code{\link{normalise}}.

Subsetting removes any references that involve removed relations.
Removing duplicates with \code{\link{unique}} changes references involving
duplicates to involve the kept equivalent relations instead. Renaming
relations with \code{\link[base:names]{`names<-`}} also changes their names
in the references.
}
