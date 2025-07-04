% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/database.r
\name{database}
\alias{database}
\title{Databases}
\usage{
database(relations, references)
}
\arguments{
\item{relations}{a \code{\link{relation}} object.}

\item{references}{a list of references, each
represented by a list containing four character elements. In order, the
elements are a scalar giving the name of the child (referrer) schema, a
vector giving the child attribute names, a scalar giving the name of the
parent (referee) schema, and a vector giving the parent attribute names. The
vectors must be of the same length and contain names for attributes present
in their respective schemas, and the parent attributes must form a key.}
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
Unlike \code{\link{relation_schema}} and \code{\link{relation}}, and like
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
relations with \code{\link[base:names]{names<-}} also changes their names
in the references.
}
\examples{
rels <- relation(
  list(
    a = list(
      df = data.frame(a = logical(), b = logical()),
      keys = list("a")
    ),
    b = list(
      df = data.frame(b = logical(), c = logical()),
      keys = list("b", "c")
    )
  ),
  attrs_order = c("a", "b", "c", "d")
)
db <- database(
  rels,
  list(list("a", "b", "b", "b"))
)
print(db)
attrs(db)
stopifnot(identical(
  attrs(db),
  lapply(records(db), names)
))
keys(db)
attrs_order(db)
names(db)
references(db)

# relations can't reference themselves
\dontrun{
  database(
    relation(
      list(a = list(df = data.frame(a = 1:5), keys = list("a"))),
      c("a", "b")
    ),
    list(list("a", "a", "a", "a"))
  )
  database(
    relation(
      list(a = list(df = data.frame(a = 1:5, b = 6:10), keys = list("a"))),
      c("a", "b")
    ),
    list(list("a", "b", "a", "a"))
  )
}

# an example with references between differently-named attributes
print(database(
  relation(
    list(
      citation = list(
        df = data.frame(citer = 1:5, citee = 6:10),
        keys = list(c("citer", "citee"))
      ),
      article = list(df = data.frame(article = 1:10), keys = list("article"))
    ),
    c("citer", "citee", "article")
  ),
  list(
    list("citation", "citer", "article", "article"),
    list("citation", "citee", "article", "article")
  )
))

# inserting data
insert(db, data.frame(a = 1L, b = 2L, c = 3L, d = 4L))
# data is only inserted into relations where all columns are given...
insert(db, data.frame(a = 1L, b = 2L, c = 3L))
# and that are listed in relations argument
insert(
  db,
  data.frame(a = 1L, b = 2L, c = 3L, d = 4L),
  relations = "b"
)
# inserted data can't violate keys
\dontrun{
  insert(
    db,
    data.frame(a = 1L, b = 1:2)
  )
}
# inserted data can't violate foreign key references
\dontrun{
  insert(
    db,
    data.frame(a = 1L, b = 2L, c = 3L, d = 4L),
    relations = "a"
  )
}

# vector operations
db2 <- database(
  relation(
    list(
      e = list(df = data.frame(a = 1:5, e = 6:10), keys = list("e"))
    ),
    attrs_order = c("a", "e")
  ),
  list()
)
c(db, db2) # attrs_order attributes are merged
unique(c(db, db))

# subsetting
db[1]
stopifnot(identical(db[[1]], db[1]))
db[c(1, 2, 1, 2)] # replicates the foreign key references
c(db[c(1, 2)], db[c(1, 2)]) # doesn't reference between separate copies of db
unique(db[c(1, 2, 1, 2)]) # unique() also merges references

# another example of unique() merging references
db_merge <- database(
  relation(
    list(
      a = list(
        df = data.frame(a = logical(), b = logical()),
        keys = list("a")
      ),
      b = list(
        df = data.frame(b = logical(), c = logical(), d = logical()),
        keys = list("b")
      ),
      c_d = list(
        df = data.frame(c = logical(), d = logical(), e = logical()),
        keys = list(c("c", "d"))
      ),
      a.1 = list(
        df = data.frame(a = logical(), b = logical()),
        keys = list("a")
      ),
      b.1 = list(
        df = data.frame(b = logical(), c = logical(), d = logical()),
        keys = list("b")
      )
    ),
    c("a", "b", "c", "d", "e")
  ),
  list(
    list("a", "b", "b", "b"),
    list("b.1", c("c", "d"), "c_d", c("c", "d"))
  )
)
print(db_merge)
unique(db_merge)

# reassignment
# can't change keys included in references
\dontrun{keys(db)[[2]] <- list("c")}
# can't remove attributes included in keys
\dontrun{attrs(db)[[2]] <- list("c", "d")}
# can't remove attributes included in references
\dontrun{attrs(db)[[1]] <- c("a", "d")}
db3 <- db
# can change subset of schema, but loses references between altered and
# non-altered subsets
db3[2] <- database(
  relation(
    list(d = list(df = data.frame(d = logical(), c = logical()), keys = list("d"))),
    attrs_order(db3)
  ),
  list()
)
print(db3) # note the schema's name doesn't change
# names(db3)[2] <- "d" # this would change the name
keys(db3)[[2]] <- list(character()) # removing keys first...
# for a database_schema, we could then change the attrs for
# the second database. For a created relation, this is not
# allowed.
\dontrun{
  attrs(db3)[[2]] <- c("b", "c")
  names(records(db3)[[2]]) <- c("b", "c")
}

# changing appearance priority for attributes
attrs_order(db3) <- c("d", "c", "b", "a")
print(db3)

# changing relation schema names changes them in references
names(db3) <- paste0(names(db3), "_long")
print(db3)

# reconstructing from components
db_recon <- database(
  relation(
    Map(list, df = records(db), keys = keys(db)),
    attrs_order(db)
  ),
  references(db)
)
stopifnot(identical(db_recon, db))
db_recon2 <- database(
  subrelations(db),
  references(db)
)
stopifnot(identical(db_recon2, db))

# can be a data frame column
data.frame(id = 1:2, relation = db)
}
