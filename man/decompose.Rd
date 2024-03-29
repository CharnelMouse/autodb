% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/decompose.r
\name{decompose}
\alias{decompose}
\title{Decompose a data frame based on given normalised dependencies}
\usage{
decompose(df, schema, name = NA_character_)
}
\arguments{
\item{df}{a data.frame, containing the data to be normalised.}

\item{schema}{a database schema with foreign key references, as given by
\code{\link{autoref}}.}

\item{name}{a scalar character, giving the name of the database. This name
is used for the resulting graph when using \code{\link{gv.database}}, to
allow for easier combining of graphs into a single diagram if required.}
}
\value{
A database, represented by a named list, with three elements:
\itemize{
\item \code{name} contains the assigned name of the relation set, if any;
\item \code{relations} contains a list of relations in third normal form,
that can reproduce the original data.frame;
\item \code{references} contains foreign key references between the
relations, represented as a list of length-four character vectors. In
order, the elements are the name of the child relation, the name of the
linked attribute in the child relation, the name of the parent relation,
and the name of the linked attribute in the parent relation. Currently,
the attribute is expected to have the same name in both relations.
\item \code{attributes} contains the attribute names in priority order.
This order can be taken from their order in \code{df}, or from the
\code{attrs_order} element in \code{schema}; these orderings must be the
same.
}

Relations are lists with the following elements:
\itemize{
\item \code{df}, the data.frame containing the data.
\item \code{keys}, a list of character vectors representing
(candidate) keys for the relation. The first key in the list is the
primary key.
}
}
\description{
Decomposes a data frame into several relations, based on the given database
schema. It's intended that the data frame satisfies all the functional
dependencies implied by the schema, such as if the schema was constructed
from the same data frame. If this is not the case, the function will returns
an error.
}
\details{
If the schema was constructed using approximate dependencies for the same
data frame, \code{decompose} returns an error, to prevent either duplicate records
or lossy decompositions. This is temporary: for the next update, we plan to
add an option to allow this, or to add "approximate" equivalents of databases
and database schemas.
}
