% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rejoin.r
\name{rejoin}
\alias{rejoin}
\title{Join a database into a data frame}
\usage{
rejoin(database)
}
\arguments{
\item{database}{A database containing the data to be rejoined, as returned by
\code{\link{decompose}}.}
}
\value{
A data frame, containing all information contained \code{database} if
it is lossless and self-consistent.
}
\description{
Rejoins the relations in a database into a single data frame, if possible.
This is the inverse of calling \code{\link{autodb}}, except that the rows
might be returned in a different order.
}
\details{
The rejoining algorithm might not use all of the given relations: it begins
with the relation with the largest number of records, then joins it with enough
relations to contain all of the present attributes. This is not limited to
relations that the starting relation is linked to by foreign keys, and is not
limited to them either, since in some cases this constraint would make it
impossible to rejoin with all of the present attributes.

Since the algorithm may not use all of the given relations, the algorithm may
ignore some types of database inconsistency, where different relations hold
data inconsistent with each other. In this case, the rejoining will be lossy.
Rejoining the results of \code{\link{reduce}} can also be lossy.

Due to the above issues, the algorithm will be changed to use all of the
relations in the future.

Not all databases can be represented as a single data frame. A simple example
is any database where the same attribute name is used for several difference
sources of data, since rejoining results in inappropriate merges.
}
\examples{
# simple example
db <- autodb(ChickWeight)
rj <- rejoin(db)
rj <- rj[order(as.integer(rownames(rj))), ]
all(rj == ChickWeight) # TRUE

# showing rejoin() doesn't check for inconsistency:
# add another Chick table with the diets swapped
db2 <- db[c(1, 2, 1)]
records(db2)[[3]]$Diet <- rev(records(db2)[[3]]$Diet)
rj2 <- rejoin(db2)
rj2 <- rj2[order(as.integer(rownames(rj2))), ]
all(rj2 == ChickWeight) # TRUE
}
