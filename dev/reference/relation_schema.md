# Relation schema vectors

Creates a set of relation schemas, including the relation's attributes
and candidate keys.

## Usage

``` r
relation_schema(schemas, attrs_order)
```

## Arguments

- schemas:

  a named list of schemas, in the form of two-element lists: the first
  element contains a character vector of all attributes in the relation
  schema, and the second element contains a list of character vectors,
  each representing a candidate key.

- attrs_order:

  a character vector, giving the names of all attributes. These need not
  be present in `schemas`, but all attributes in `schemas` must be
  present in `attrs_order`.

## Value

A `relation_schema` object, containing the list given in `schemas`, with
`attrs_order` stored in an attribute of the same name. Relation schemas
are returned with their keys' attributes sorted according to the
attribute order in `attrs_order`, and the keys then sorted by priority
order. Attributes in the schema are also sorted, first by order of
appearance in the sorted keys, then by order in `attrs_order` for
non-prime attributes.

## Details

Duplicate schemas, after ordering by attribute, are allowed, and can be
removed with `\code{\link{unique}}`.

When several sets of relation schemas are concatenated, their
`attrs_order` attributes are merged, so as to preserve all of the
original attribute orders, if possible. If this is not possible, because
the orderings disagree, then the returned value of the `attrs_order`
attribute is their union instead.

## See also

[`attrs`](https://charnelmouse.github.io/autodb/dev/reference/attrs.md),
[`keys`](https://charnelmouse.github.io/autodb/dev/reference/keys.md),
and
[`attrs_order`](https://charnelmouse.github.io/autodb/dev/reference/attrs_order.md)
for extracting parts of the information in a `relation_schema`;
[`create`](https://charnelmouse.github.io/autodb/dev/reference/create.md)
for creating a
[`relation`](https://charnelmouse.github.io/autodb/dev/reference/relation.md)
object that uses the given schema;
[`gv`](https://charnelmouse.github.io/autodb/dev/reference/gv.md) for
converting the schema into Graphviz code;
[`rename_attrs`](https://charnelmouse.github.io/autodb/dev/reference/rename_attrs.md)
for renaming the attributes in `attrs_order`;
[`merge_empty_keys`](https://charnelmouse.github.io/autodb/dev/reference/merge_empty_keys.md)
for combining relations with an empty key;
[`merge_schemas`](https://charnelmouse.github.io/autodb/dev/reference/merge_schemas.md)
for combining relations with matching sets of keys.

## Examples

``` r
schemas <- relation_schema(
  list(
    a = list(c("a", "b"), list("a")),
    b = list(c("b", "c"), list("b", "c"))
  ),
  attrs_order = c("a", "b", "c", "d")
)
print(schemas)
#> 2 relation schemas
#> 4 attributes: a, b, c, d
#> schema a: a, b
#>   key 1: a
#> schema b: b, c
#>   key 1: b
#>   key 2: c
attrs(schemas)
#> $a
#> [1] "a" "b"
#> 
#> $b
#> [1] "b" "c"
#> 
keys(schemas)
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
attrs_order(schemas)
#> [1] "a" "b" "c" "d"
names(schemas)
#> [1] "a" "b"

# vector operations
schemas2 <- relation_schema(
  list(
    e = list(c("a", "e"), list("e"))
  ),
  attrs_order = c("a", "e")
)
c(schemas, schemas2) # attrs_order attributes are merged
#> 3 relation schemas
#> 5 attributes: a, b, c, d, e
#> schema a: a, b
#>   key 1: a
#> schema b: b, c
#>   key 1: b
#>   key 2: c
#> schema e: e, a
#>   key 1: e
unique(c(schemas, schemas))
#> 2 relation schemas
#> 4 attributes: a, b, c, d
#> schema a: a, b
#>   key 1: a
#> schema b: b, c
#>   key 1: b
#>   key 2: c

# subsetting
schemas[1]
#> 1 relation schema
#> 4 attributes: a, b, c, d
#> schema a: a, b
#>   key 1: a
schemas[c(1, 2, 1)]
#> 3 relation schemas
#> 4 attributes: a, b, c, d
#> schema a: a, b
#>   key 1: a
#> schema b: b, c
#>   key 1: b
#>   key 2: c
#> schema a.1: a, b
#>   key 1: a
stopifnot(identical(schemas[[1]], schemas[1]))

# reassignment
schemas3 <- schemas
schemas3[2] <- relation_schema(
  list(d = list(c("d", "c"), list("d"))),
  attrs_order(schemas3)
)
print(schemas3) # note the schema's name doesn't change
#> 2 relation schemas
#> 4 attributes: a, b, c, d
#> schema a: a, b
#>   key 1: a
#> schema b: d, c
#>   key 1: d
# names(schemas3)[2] <- "d" # this would change the name
keys(schemas3)[[2]] <- list(character()) # removing keys first...
attrs(schemas3)[[2]] <- c("b", "c") # so we can change the attrs legally
keys(schemas3)[[2]] <- list("b", "c") # add the new keys
stopifnot(identical(schemas3, schemas))

# changing appearance priority for attributes
attrs_order(schemas3) <- c("d", "c", "b", "a")
print(schemas3)
#> 2 relation schemas
#> 4 attributes: d, c, b, a
#> schema a: a, b
#>   key 1: a
#> schema b: c, b
#>   key 1: c
#>   key 2: b

# reconstructing from components
schemas_recon <- relation_schema(
  Map(list, attrs(schemas), keys(schemas)),
  attrs_order(schemas)
)
stopifnot(identical(schemas_recon, schemas))

# can be a data frame column
data.frame(id = 1:2, schema = schemas)
#>   id   schema
#> 1  1 schema a
#> 2  2 schema b
```
