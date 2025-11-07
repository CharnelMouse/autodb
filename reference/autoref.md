# Add foreign key references to a normalised database

Adds foreign key references to a
[`relation_schema`](https://charnelmouse.github.io/autodb/reference/relation_schema.md)
object automatically, replacing any existing references.

## Usage

``` r
autoref(schema, single_ref = FALSE)
```

## Arguments

- schema:

  a
  [`relation_schema`](https://charnelmouse.github.io/autodb/reference/relation_schema.md)
  object, as given by
  [`synthesise`](https://charnelmouse.github.io/autodb/reference/synthesise.md).

- single_ref:

  a logical, FALSE by default. If TRUE, then only one reference between
  each relation pair is kept when generating foreign key references. If
  a pair has multiple references, the kept reference refers to the
  earliest key for the child relation, as sorted by priority order.

## Value

A
[`database_schema`](https://charnelmouse.github.io/autodb/reference/database_schema.md)
object, containing the given relation schemas and the created foreign
key references.

## Details

The method for generating references is simple. First, it finds every
link between two relation schemas, where the parent contains all the
attributes in one of the child's keys. This can be done separately for
all of the child's keys, so there can be multiple links with the same
parent and child if `single_ref` is `TRUE`.

Second, any transitive references are removed: if there are link
relation pairs a -\> b, b -\> c, and a -\> c, then the latter is
transitive, and so is removed. If there is a cyclic reference, e.g.
where c -\> a, then the choice of which link to remove is arbitrary.
Cycles cannot occur in sets of relation schemas resulting from
decomposing a single table.

## Examples

``` r
rs <- relation_schema(
  list(
    a_b_c = list(c("a", "b", "c", "d"), list(c("a", "b", "c"))),
    a_b = list(c("a", "b", "d"), list(c("a", "b"), c("b", "d")))
  ),
  letters[1:4]
)
autoref(rs, single_ref = FALSE)
#> database schema with 2 relation schemas
#> 4 attributes: a, b, c, d
#> schema a_b_c: a, b, c, d
#>   key 1: a, b, c
#> schema a_b: a, b, d
#>   key 1: a, b
#>   key 2: b, d
#> references:
#> a_b_c.{a, b} -> a_b.{a, b}
#> a_b_c.{b, d} -> a_b.{b, d}
autoref(rs, single_ref = TRUE)
#> database schema with 2 relation schemas
#> 4 attributes: a, b, c, d
#> schema a_b_c: a, b, c, d
#>   key 1: a, b, c
#> schema a_b: a, b, d
#>   key 1: a, b
#>   key 2: b, d
#> references:
#> a_b_c.{a, b} -> a_b.{a, b}
```
