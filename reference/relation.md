# Relation vectors

Creates a set of relation schemas, including the relation's attributes
and candidate keys.

## Usage

``` r
relation(relations, attrs_order)
```

## Arguments

- relations:

  a named list of relations, in the form of two-element lists: the first
  element contains a data frame, where the column names are the
  attributes in the associated schema, and the second element contains a
  list of character vectors, each representing a candidate key.

- attrs_order:

  a character vector, giving the names of all attributes. These need not
  be present in `schemas`, but all attributes in `schemas` must be
  present in `attrs_order`.

## Value

A `relation` object, containing the list given in `relations`, with
`attrs_order` stored in an attribute of the same name. Relation schemas
are returned with their keys' attributes sorted according to the
attribute order in `attrs_order`, and the keys then sorted by priority
order. Attributes in the data frame are also sorted, first by order of
appearance in the sorted keys, then by order in `attrs_order` for
non-prime attributes.

## Details

Relation vectors are unlikely to be needed by the user directly, since
they are essentially
[`database`](https://charnelmouse.github.io/autodb/reference/database.md)
objects that can't have foreign key references. They are mostly used to
mirror the use of the vector-like
[`relation_schema`](https://charnelmouse.github.io/autodb/reference/relation_schema.md)
class for the
[`database_schema`](https://charnelmouse.github.io/autodb/reference/database_schema.md)
class to be a wrapper around. This makes creating a
[`database`](https://charnelmouse.github.io/autodb/reference/database.md)
from a
[`relation_schema`](https://charnelmouse.github.io/autodb/reference/relation_schema.md)
a two-step process, where the two steps can be done in either order:
creation with
[`create`](https://charnelmouse.github.io/autodb/reference/create.md)
and
[`insert`](https://charnelmouse.github.io/autodb/reference/insert.md),
and adding references with
[`database_schema`](https://charnelmouse.github.io/autodb/reference/database_schema.md)
or
[`database`](https://charnelmouse.github.io/autodb/reference/database.md).

Duplicate schemas, after ordering by attribute, are allowed, and can be
removed with [`unique`](https://rdrr.io/r/base/unique.html).

When several sets of relation schemas are concatenated, their
`attrs_order` attributes are merged, so as to preserve all of the
original attribute orders, if possible. If this is not possible, because
the orderings disagree, then the returned value of the `attrs_order`
attribute is their union instead.

## See also

[`records`](https://charnelmouse.github.io/autodb/reference/records.md),
[`attrs`](https://charnelmouse.github.io/autodb/reference/attrs.md),
[`keys`](https://charnelmouse.github.io/autodb/reference/keys.md), and
[`attrs_order`](https://charnelmouse.github.io/autodb/reference/attrs_order.md)
for extracting parts of the information in a `relation_schema`;
[`gv`](https://charnelmouse.github.io/autodb/reference/gv.md) for
converting the schema into Graphviz code;
[`rename_attrs`](https://charnelmouse.github.io/autodb/reference/rename_attrs.md)
for renaming the attributes in `attrs_order`.

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
print(rels)
#> 2 relations
#> 4 attributes: a, b, c, d
#> relation a: a, b; 0 records
#>   key 1: a
#> relation b: b, c; 0 records
#>   key 1: b
#>   key 2: c
records(rels)
#> $a
#> [1] a b
#> <0 rows> (or 0-length row.names)
#> 
#> $b
#> [1] b c
#> <0 rows> (or 0-length row.names)
#> 
attrs(rels)
#> $a
#> [1] "a" "b"
#> 
#> $b
#> [1] "b" "c"
#> 
stopifnot(identical(
  attrs(rels),
  lapply(records(rels), names)
))
keys(rels)
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
attrs_order(rels)
#> [1] "a" "b" "c" "d"
names(rels)
#> [1] "a" "b"

# inserting data
insert(rels, data.frame(a = 1L, b = 2L, c = 3L, d = 4L))
#> 2 relations
#> 4 attributes: a, b, c, d
#> relation a: a, b; 1 record
#>   key 1: a
#> relation b: b, c; 1 record
#>   key 1: b
#>   key 2: c
# data is only inserted into relations where all columns are given...
insert(rels, data.frame(a = 1L, b = 2L, c = 3L))
#> 2 relations
#> 4 attributes: a, b, c, d
#> relation a: a, b; 1 record
#>   key 1: a
#> relation b: b, c; 1 record
#>   key 1: b
#>   key 2: c
# and that are listed in relations argument
insert(
  rels,
  data.frame(a = 1L, b = 2L, c = 3L, d = 4L),
  relations = "a"
)
#> 2 relations
#> 4 attributes: a, b, c, d
#> relation a: a, b; 1 record
#>   key 1: a
#> relation b: b, c; 0 records
#>   key 1: b
#>   key 2: c

# vector operations
rels2 <- relation(
  list(
    e = list(
      df = data.frame(a = logical(), e = logical()),
      keys = list("e")
    )
  ),
  attrs_order = c("a", "e")
)
c(rels, rels2) # attrs_order attributes are merged
#> 3 relations
#> 5 attributes: a, b, c, d, e
#> relation a: a, b; 0 records
#>   key 1: a
#> relation b: b, c; 0 records
#>   key 1: b
#>   key 2: c
#> relation e: e, a; 0 records
#>   key 1: e
unique(c(rels, rels))
#> 2 relations
#> 4 attributes: a, b, c, d
#> relation a: a, b; 0 records
#>   key 1: a
#> relation b: b, c; 0 records
#>   key 1: b
#>   key 2: c

# subsetting
rels[1]
#> 1 relation
#> 4 attributes: a, b, c, d
#> relation a: a, b; 0 records
#>   key 1: a
rels[c(1, 2, 1)]
#> 3 relations
#> 4 attributes: a, b, c, d
#> relation a: a, b; 0 records
#>   key 1: a
#> relation b: b, c; 0 records
#>   key 1: b
#>   key 2: c
#> relation a.1: a, b; 0 records
#>   key 1: a
stopifnot(identical(rels[[1]], rels[1]))

# reassignment
rels3 <- rels
rels3[2] <- relation(
  list(
    d = list(
      df = data.frame(d = logical(), c = logical()),
      keys = list("d")
    )
  ),
  attrs_order(rels3)
)
print(rels3) # note the relation's name doesn't change
#> 2 relations
#> 4 attributes: a, b, c, d
#> relation a: a, b; 0 records
#>   key 1: a
#> relation b: d, c; 0 records
#>   key 1: d
# names(rels3)[2] <- "d" # this would change the name
keys(rels3)[[2]] <- list(character()) # removing keys first...
# for a relation_schema, we could then change the attrs for
# the second relation. For a created relation, this is not
# allowed.
if (FALSE) { # \dontrun{
  attrs(rels3)[[2]] <- c("b", "c")
  names(records(rels3)[[2]]) <- c("b", "c")
} # }

# changing appearance priority for attributes
rels4 <- rels
attrs_order(rels4) <- c("d", "c", "b", "a")
print(rels4)
#> 2 relations
#> 4 attributes: d, c, b, a
#> relation a: a, b; 0 records
#>   key 1: a
#> relation b: c, b; 0 records
#>   key 1: c
#>   key 2: b

# reconstructing from components
rels_recon <- relation(
  Map(list, df = records(rels), keys = keys(rels)),
  attrs_order(rels)
)
stopifnot(identical(rels_recon, rels))

# can be a data frame column
data.frame(id = 1:2, relation = rels)
#>   id               relation
#> 1  1 relation a (0 records)
#> 2  2 relation b (0 records)
```
