# Databases

Enhances a
[`relation`](https://charnelmouse.github.io/autodb/reference/relation.md)
object with foreign key reference information.

## Usage

``` r
database(relations, references, check = TRUE)
```

## Arguments

- relations:

  a
  [`relation`](https://charnelmouse.github.io/autodb/reference/relation.md)
  object.

- references:

  a list of references, each represented by a list containing four
  character elements. In order, the elements are a scalar giving the
  name of the child (referrer) schema, a vector giving the child
  attribute names, a scalar giving the name of the parent (referee)
  schema, and a vector giving the parent attribute names. The vectors
  must be of the same length and contain names for attributes present in
  their respective schemas, and the parent attributes must form a key.

- check:

  a logical, indicating whether to check that `relations` satisfies the
  foreign key references given in `references` before creating the
  result. This is redundant if `relations` and `references` were
  constructed based on the same functional dependencies, such as when
  using
  [`autodb`](https://charnelmouse.github.io/autodb/reference/autodb.md).
  Only set to FALSE if the references are definitely satisfied: not
  checking references that violate the data will result in an invalid
  database, which will not be detected until further operated on.

## Value

A `database` object, containing `relations` with `references` stored in
an attribute of the same name. References are stored with their
attributes in the order they appear in their respective relations.

## Details

Unlike
[`relation_schema`](https://charnelmouse.github.io/autodb/reference/relation_schema.md)
and
[`relation`](https://charnelmouse.github.io/autodb/reference/relation.md),
and like
[`database_schema`](https://charnelmouse.github.io/autodb/reference/database_schema.md),
`database` is not designed to be vector-like: it only holds a single
database. This adheres to the usual package use case, where a single
data frame is being analysed at a time. However, it inherits from
[`relation`](https://charnelmouse.github.io/autodb/reference/relation.md),
so is vectorised with respect to its relations.

As with
[`relation`](https://charnelmouse.github.io/autodb/reference/relation.md),
duplicate relations, after ordering by attribute, are allowed, and can
be removed with [`unique`](https://rdrr.io/r/base/unique.html).

References, i.e. foreign key references, are allowed to have different
attribute names in the child and parent relations; this can't occur in
the output for
[`autoref`](https://charnelmouse.github.io/autodb/reference/autoref.md)
and
[`normalise`](https://charnelmouse.github.io/autodb/reference/normalise.md).

Subsetting removes any references that involve removed relations.
Removing duplicates with [`unique`](https://rdrr.io/r/base/unique.html)
changes references involving duplicates to involve the kept equivalent
relations instead. Renaming relations with
[`names<-`](https://rdrr.io/r/base/names.html) also changes their names
in the references.

## Examples

``` r
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
#> database with 2 relations
#> 4 attributes: a, b, c, d
#> relation a: a, b; 0 records
#>   key 1: a
#> relation b: b, c; 0 records
#>   key 1: b
#>   key 2: c
#> references:
#> a.{b} -> b.{b}
attrs(db)
#> $a
#> [1] "a" "b"
#> 
#> $b
#> [1] "b" "c"
#> 
stopifnot(identical(
  attrs(db),
  lapply(records(db), names)
))
keys(db)
#> $a
#> $a[[1]]
#> [1] "a"
#> 
#> 
#> $b
#> $b[[1]]
#> [1] "b"
#> 
#> $b[[2]]
#> [1] "c"
#> 
#> 
attrs_order(db)
#> [1] "a" "b" "c" "d"
names(db)
#> [1] "a" "b"
references(db)
#> [[1]]
#> [[1]][[1]]
#> [1] "a"
#> 
#> [[1]][[2]]
#> [1] "b"
#> 
#> [[1]][[3]]
#> [1] "b"
#> 
#> [[1]][[4]]
#> [1] "b"
#> 
#> 

# relations can't reference themselves
if (FALSE) { # \dontrun{
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
} # }

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
#> database with 2 relations
#> 3 attributes: citer, citee, article
#> relation citation: citer, citee; 5 records
#>   key 1: citer, citee
#> relation article: article; 10 records
#>   key 1: article
#> references:
#> citation.{citer} -> article.{article}
#> citation.{citee} -> article.{article}

# inserting data
insert(db, data.frame(a = 1L, b = 2L, c = 3L, d = 4L))
#> database with 2 relations
#> 4 attributes: a, b, c, d
#> relation a: a, b; 1 record
#>   key 1: a
#> relation b: b, c; 1 record
#>   key 1: b
#>   key 2: c
#> references:
#> a.{b} -> b.{b}
# data is only inserted into relations where all columns are given...
insert(db, data.frame(a = 1L, b = 2L, c = 3L))
#> database with 2 relations
#> 4 attributes: a, b, c, d
#> relation a: a, b; 1 record
#>   key 1: a
#> relation b: b, c; 1 record
#>   key 1: b
#>   key 2: c
#> references:
#> a.{b} -> b.{b}
# and that are listed in relations argument
insert(
  db,
  data.frame(a = 1L, b = 2L, c = 3L, d = 4L),
  relations = "b"
)
#> database with 2 relations
#> 4 attributes: a, b, c, d
#> relation a: a, b; 0 records
#>   key 1: a
#> relation b: b, c; 1 record
#>   key 1: b
#>   key 2: c
#> references:
#> a.{b} -> b.{b}
# inserted data can't violate keys
if (FALSE) { # \dontrun{
  insert(
    db,
    data.frame(a = 1L, b = 1:2)
  )
} # }
# inserted data can't violate foreign key references
if (FALSE) { # \dontrun{
  insert(
    db,
    data.frame(a = 1L, b = 2L, c = 3L, d = 4L),
    relations = "a"
  )
} # }

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
#> database with 3 relations
#> 5 attributes: a, b, c, d, e
#> relation a: a, b; 0 records
#>   key 1: a
#> relation b: b, c; 0 records
#>   key 1: b
#>   key 2: c
#> relation e: e, a; 5 records
#>   key 1: e
#> references:
#> a.{b} -> b.{b}
unique(c(db, db))
#> database with 2 relations
#> 4 attributes: a, b, c, d
#> relation a: a, b; 0 records
#>   key 1: a
#> relation b: b, c; 0 records
#>   key 1: b
#>   key 2: c
#> references:
#> a.{b} -> b.{b}

# subsetting
db[1]
#> database with 1 relation
#> 4 attributes: a, b, c, d
#> relation a: a, b; 0 records
#>   key 1: a
#> no references
stopifnot(identical(db[[1]], db[1]))
db[c(1, 2, 1, 2)] # replicates the foreign key references
#> database with 4 relations
#> 4 attributes: a, b, c, d
#> relation a: a, b; 0 records
#>   key 1: a
#> relation b: b, c; 0 records
#>   key 1: b
#>   key 2: c
#> relation a.1: a, b; 0 records
#>   key 1: a
#> relation b.1: b, c; 0 records
#>   key 1: b
#>   key 2: c
#> references:
#> a.{b} -> b.{b}
#> a.{b} -> b.1.{b}
#> a.1.{b} -> b.{b}
#> a.1.{b} -> b.1.{b}
c(db[c(1, 2)], db[c(1, 2)]) # doesn't reference between separate copies of db
#> database with 4 relations
#> 4 attributes: a, b, c, d
#> relation a: a, b; 0 records
#>   key 1: a
#> relation b: b, c; 0 records
#>   key 1: b
#>   key 2: c
#> relation a.1: a, b; 0 records
#>   key 1: a
#> relation b.1: b, c; 0 records
#>   key 1: b
#>   key 2: c
#> references:
#> a.{b} -> b.{b}
#> a.1.{b} -> b.1.{b}
unique(db[c(1, 2, 1, 2)]) # unique() also merges references
#> database with 2 relations
#> 4 attributes: a, b, c, d
#> relation a: a, b; 0 records
#>   key 1: a
#> relation b: b, c; 0 records
#>   key 1: b
#>   key 2: c
#> references:
#> a.{b} -> b.{b}

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
#> database with 5 relations
#> 5 attributes: a, b, c, d, e
#> relation a: a, b; 0 records
#>   key 1: a
#> relation b: b, c, d; 0 records
#>   key 1: b
#> relation c_d: c, d, e; 0 records
#>   key 1: c, d
#> relation a.1: a, b; 0 records
#>   key 1: a
#> relation b.1: b, c, d; 0 records
#>   key 1: b
#> references:
#> a.{b} -> b.{b}
#> b.1.{c, d} -> c_d.{c, d}
unique(db_merge)
#> database with 3 relations
#> 5 attributes: a, b, c, d, e
#> relation a: a, b; 0 records
#>   key 1: a
#> relation b: b, c, d; 0 records
#>   key 1: b
#> relation c_d: c, d, e; 0 records
#>   key 1: c, d
#> references:
#> a.{b} -> b.{b}
#> b.{c, d} -> c_d.{c, d}

# reassignment
# can't change keys included in references
if (FALSE) keys(db)[[2]] <- list("c") # \dontrun{}
# can't remove attributes included in keys
if (FALSE) attrs(db)[[2]] <- list("c", "d") # \dontrun{}
# can't remove attributes included in references
if (FALSE) attrs(db)[[1]] <- c("a", "d") # \dontrun{}
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
#> database with 2 relations
#> 4 attributes: a, b, c, d
#> relation a: a, b; 0 records
#>   key 1: a
#> relation b: d, c; 0 records
#>   key 1: d
#> no references
# names(db3)[2] <- "d" # this would change the name
keys(db3)[[2]] <- list(character()) # removing keys first...
# for a database_schema, we could then change the attrs for
# the second database. For a created relation, this is not
# allowed.
if (FALSE) { # \dontrun{
  attrs(db3)[[2]] <- c("b", "c")
  names(records(db3)[[2]]) <- c("b", "c")
} # }

# changing appearance priority for attributes
attrs_order(db3) <- c("d", "c", "b", "a")
print(db3)
#> database with 2 relations
#> 4 attributes: d, c, b, a
#> relation a: a, b; 0 records
#>   key 1: a
#> relation b: d, c; 0 records
#>   key 1: 
#> no references

# changing relation schema names changes them in references
names(db3) <- paste0(names(db3), "_long")
print(db3)
#> database with 2 relations
#> 4 attributes: d, c, b, a
#> relation a_long: a, b; 0 records
#>   key 1: a
#> relation b_long: d, c; 0 records
#>   key 1: 
#> no references

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
#>   id               relation
#> 1  1 relation a (0 records)
#> 2  2 relation b (0 records)

# setting check = FALSE can give invalid databases
chickfds <- discover(ChickWeight)
chickschema <- synthesise(chickfds)
chickrels <- insert(create(chickschema), ChickWeight)
badrefs <- list(list("Time_Chick", "weight", "Chick", "Chick"))
# check = TRUE stops on non-satisfied references
if (FALSE) database(chickrels, badrefs) # \dontrun{}
# check = FALSE returns an invalid database
baddb <- database(chickrels, badrefs, check = FALSE)
# only returns an error when further manipulated by certain methods
if (FALSE) insert(baddb, ChickWeight) # \dontrun{}
```
