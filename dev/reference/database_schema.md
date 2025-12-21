# Database schemas

Enhances a
[`relation_schema`](https://charnelmouse.github.io/autodb/dev/reference/relation_schema.md)
object with foreign key reference information.

## Usage

``` r
database_schema(relation_schemas, references)
```

## Arguments

- relation_schemas:

  a
  [`relation_schema`](https://charnelmouse.github.io/autodb/dev/reference/relation_schema.md)
  object, as returned by
  [`synthesise`](https://charnelmouse.github.io/autodb/dev/reference/synthesise.md)
  or
  [`relation_schema`](https://charnelmouse.github.io/autodb/dev/reference/relation_schema.md).

- references:

  a list of references, each represented by a list containing four
  character elements. In order, the elements are a scalar giving the
  name of the child (referrer) schema, a vector giving the child
  attribute names, a scalar giving the name of the parent (referee)
  schema, and a vector giving the parent attribute names. The vectors
  must be of the same length and contain names for attributes present in
  their respective schemas, and the parent attributes must form a key.

## Value

A `database_schema` object, containing `relation_schemas` with
`references` stored in an attribute of the same name. References are
stored with their attributes in the order they appear in their
respective relation schemas.

## Details

Unlike
[`functional_dependency`](https://charnelmouse.github.io/autodb/dev/reference/functional_dependency.md)
and
[`relation_schema`](https://charnelmouse.github.io/autodb/dev/reference/relation_schema.md),
`database_schema` is not designed to be vector-like: it only holds a
single database schema. This adheres to the usual package use case,
where a single data frame is being analysed at a time. However, it
inherits from
[`relation_schema`](https://charnelmouse.github.io/autodb/dev/reference/relation_schema.md),
so is vectorised with respect to its relation schemas.

As with
[`relation_schema`](https://charnelmouse.github.io/autodb/dev/reference/relation_schema.md),
duplicate relation schemas, after ordering by attribute, are allowed,
and can be removed with [`unique`](https://rdrr.io/r/base/unique.html).

References, i.e. foreign key references, are allowed to have different
attribute names in the child and parent relations; this can't occur in
the output for
[`autoref`](https://charnelmouse.github.io/autodb/dev/reference/autoref.md)
and
[`normalise`](https://charnelmouse.github.io/autodb/dev/reference/normalise.md).

Subsetting removes any references that involve removed relation schemas.
Removing duplicates with [`unique`](https://rdrr.io/r/base/unique.html)
changes references involving duplicates to involve the kept equivalent
schemas instead. Renaming relation schemas with
[`names<-`](https://rdrr.io/r/base/names.html) also changes their names
in the references.

## See also

[`attrs`](https://charnelmouse.github.io/autodb/dev/reference/attrs.md),
[`keys`](https://charnelmouse.github.io/autodb/dev/reference/keys.md),
[`attrs_order`](https://charnelmouse.github.io/autodb/dev/reference/attrs_order.md),
and
[`references`](https://charnelmouse.github.io/autodb/dev/reference/references.md)
for extracting parts of the information in a `database_schema`;
[`create`](https://charnelmouse.github.io/autodb/dev/reference/create.md)
for creating a
[`database`](https://charnelmouse.github.io/autodb/dev/reference/database.md)
object that uses the given schema;
[`gv`](https://charnelmouse.github.io/autodb/dev/reference/gv.md) for
converting the schema into Graphviz code;
[`rename_attrs`](https://charnelmouse.github.io/autodb/dev/reference/rename_attrs.md)
for renaming the attributes in `attrs_order`;
[`reduce`](https://charnelmouse.github.io/autodb/dev/reference/reduce.md)
for filtering a schema's relations to those connected to a given
relation by foreign key references;
[`subschemas`](https://charnelmouse.github.io/autodb/dev/reference/subschemas.md)
to return the
[`relation_schema`](https://charnelmouse.github.io/autodb/dev/reference/relation_schema.md)
that the given schema contains;
[`merge_empty_keys`](https://charnelmouse.github.io/autodb/dev/reference/merge_empty_keys.md)
for combining relations with an empty key;
[`merge_schemas`](https://charnelmouse.github.io/autodb/dev/reference/merge_schemas.md)
for combining relations with matching sets of keys.

## Examples

``` r
rs <- relation_schema(
  list(
    a = list(c("a", "b"), list("a")),
    b = list(c("b", "c"), list("b", "c"))
  ),
  attrs_order = c("a", "b", "c", "d")
)
ds <- database_schema(
  rs,
  list(list("a", "b", "b", "b"))
)
print(ds)
#> database schema with 2 relation schemas
#> 4 attributes: a, b, c, d
#> schema a: a, b
#>   key 1: a
#> schema b: b, c
#>   key 1: b
#>   key 2: c
#> references:
#> a.{b} -> b.{b}
attrs(ds)
#> $a
#> [1] "a" "b"
#> 
#> $b
#> [1] "b" "c"
#> 
keys(ds)
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
attrs_order(ds)
#> [1] "a" "b" "c" "d"
names(ds)
#> [1] "a" "b"
references(ds)
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
  database_schema(
    relation_schema(
      list(a = list("a", list("a"))),
      c("a", "b")
    ),
    list(list("a", "a", "a", "a"))
  )
  database_schema(
    relation_schema(
      list(a = list(c("a", "b"), list("a"))),
      c("a", "b")
    ),
    list(list("a", "b", "a", "a"))
  )
} # }

# an example with references between differently-named attributes
print(database_schema(
  relation_schema(
    list(
      citation = list(c("citer", "citee"), list(c("citer", "citee"))),
      article = list("article", list("article"))
    ),
    c("citer", "citee", "article")
  ),
  list(
    list("citation", "citer", "article", "article"),
    list("citation", "citee", "article", "article")
  )
))
#> database schema with 2 relation schemas
#> 3 attributes: citer, citee, article
#> schema citation: citer, citee
#>   key 1: citer, citee
#> schema article: article
#>   key 1: article
#> references:
#> citation.{citer} -> article.{article}
#> citation.{citee} -> article.{article}

# vector operations
ds2 <- database_schema(
  relation_schema(
    list(
      e = list(c("a", "e"), list("e"))
    ),
    attrs_order = c("a", "e")
  ),
  list()
)
c(ds, ds2) # attrs_order attributes are merged
#> database schema with 3 relation schemas
#> 5 attributes: a, b, c, d, e
#> schema a: a, b
#>   key 1: a
#> schema b: b, c
#>   key 1: b
#>   key 2: c
#> schema e: e, a
#>   key 1: e
#> references:
#> a.{b} -> b.{b}
unique(c(ds, ds))
#> database schema with 2 relation schemas
#> 4 attributes: a, b, c, d
#> schema a: a, b
#>   key 1: a
#> schema b: b, c
#>   key 1: b
#>   key 2: c
#> references:
#> a.{b} -> b.{b}

# subsetting
ds[1]
#> database schema with 1 relation schema
#> 4 attributes: a, b, c, d
#> schema a: a, b
#>   key 1: a
#> no references
stopifnot(identical(ds[[1]], ds[1]))
ds[c(1, 2, 1, 2)] # replicates the foreign key references
#> database schema with 4 relation schemas
#> 4 attributes: a, b, c, d
#> schema a: a, b
#>   key 1: a
#> schema b: b, c
#>   key 1: b
#>   key 2: c
#> schema a.1: a, b
#>   key 1: a
#> schema b.1: b, c
#>   key 1: b
#>   key 2: c
#> references:
#> a.{b} -> b.{b}
#> a.{b} -> b.1.{b}
#> a.1.{b} -> b.{b}
#> a.1.{b} -> b.1.{b}
c(ds[c(1, 2)], ds[c(1, 2)]) # doesn't reference between separate copies of ds
#> database schema with 4 relation schemas
#> 4 attributes: a, b, c, d
#> schema a: a, b
#>   key 1: a
#> schema b: b, c
#>   key 1: b
#>   key 2: c
#> schema a.1: a, b
#>   key 1: a
#> schema b.1: b, c
#>   key 1: b
#>   key 2: c
#> references:
#> a.{b} -> b.{b}
#> a.1.{b} -> b.1.{b}
unique(ds[c(1, 2, 1, 2)]) # unique() also merges references
#> database schema with 2 relation schemas
#> 4 attributes: a, b, c, d
#> schema a: a, b
#>   key 1: a
#> schema b: b, c
#>   key 1: b
#>   key 2: c
#> references:
#> a.{b} -> b.{b}

# another example of unique() merging references
ds_merge <- database_schema(
  relation_schema(
    list(
      a = list(c("a", "b"), list("a")),
      b = list(c("b", "c", "d"), list("b")),
      c_d = list(c("c", "d", "e"), list(c("c", "d"))),
      a.1 = list(c("a", "b"), list("a")),
      b.1 = list(c("b", "c", "d"), list("b"))
    ),
    c("a", "b", "c", "d", "e")
  ),
  list(
    list("a", "b", "b", "b"),
    list("b.1", c("c", "d"), "c_d", c("c", "d"))
  )
)
print(ds_merge)
#> database schema with 5 relation schemas
#> 5 attributes: a, b, c, d, e
#> schema a: a, b
#>   key 1: a
#> schema b: b, c, d
#>   key 1: b
#> schema c_d: c, d, e
#>   key 1: c, d
#> schema a.1: a, b
#>   key 1: a
#> schema b.1: b, c, d
#>   key 1: b
#> references:
#> a.{b} -> b.{b}
#> b.1.{c, d} -> c_d.{c, d}
unique(ds_merge)
#> database schema with 3 relation schemas
#> 5 attributes: a, b, c, d, e
#> schema a: a, b
#>   key 1: a
#> schema b: b, c, d
#>   key 1: b
#> schema c_d: c, d, e
#>   key 1: c, d
#> references:
#> a.{b} -> b.{b}
#> b.{c, d} -> c_d.{c, d}

# reassignment
# can't change keys included in references
if (FALSE) keys(ds)[[2]] <- list("c") # \dontrun{}
# can't remove attributes included in keys
if (FALSE) attrs(ds)[[2]] <- list("c", "d") # \dontrun{}
# can't remove attributes included in references
if (FALSE) attrs(ds)[[1]] <- c("a", "d") # \dontrun{}
ds3 <- ds
# can change subset of schema, but loses references between altered and
# non-altered subsets
ds3[2] <- database_schema(
  relation_schema(
    list(d = list(c("d", "c"), list("d"))),
    attrs_order(ds3)
  ),
  list()
)
print(ds3) # note the schema's name doesn't change
#> database schema with 2 relation schemas
#> 4 attributes: a, b, c, d
#> schema a: a, b
#>   key 1: a
#> schema b: d, c
#>   key 1: d
#> no references
# names(ds3)[2] <- "d" # this would change the name
keys(ds3)[[2]] <- list(character()) # removing keys first...
attrs(ds3)[[2]] <- c("b", "c") # so we can change the attrs legally
keys(ds3)[[2]] <- list("b", "c") # add the new keys
# add the reference lost during subset replacement
references(ds3) <- c(references(ds3), list(list("a", "b", "b", "b")))
stopifnot(identical(ds3, ds))

# changing appearance priority for attributes
attrs_order(ds3) <- c("d", "c", "b", "a")
print(ds3)
#> database schema with 2 relation schemas
#> 4 attributes: d, c, b, a
#> schema a: a, b
#>   key 1: a
#> schema b: c, b
#>   key 1: c
#>   key 2: b
#> references:
#> a.{b} -> b.{b}

# changing relation schema names changes them in references
names(ds3) <- paste0(names(ds3), "_long")
print(ds3)
#> database schema with 2 relation schemas
#> 4 attributes: d, c, b, a
#> schema a_long: a, b
#>   key 1: a
#> schema b_long: c, b
#>   key 1: c
#>   key 2: b
#> references:
#> a_long.{b} -> b_long.{b}

# reconstructing from components
ds_recon <- database_schema(
  relation_schema(
    Map(list, attrs(ds), keys(ds)),
    attrs_order(ds)
  ),
  references(ds)
)
stopifnot(identical(ds_recon, ds))
ds_recon2 <- database_schema(
  subschemas(ds),
  references(ds)
)
stopifnot(identical(ds_recon2, ds))

# can be a data frame column
data.frame(id = 1:2, schema = ds)
#>   id   schema
#> 1  1 schema a
#> 2  2 schema b
```
