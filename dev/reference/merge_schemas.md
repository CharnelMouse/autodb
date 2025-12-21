# Merge relation schemas in given pairs

Generic function that merges pairs of an object's schemas with matching
sets of keys. The remaining schemas contain all the attributes from the
schemas merged into them.

## Usage

``` r
merge_schemas(x, to_remove, merge_into, ...)
```

## Arguments

- x:

  a relational schema object, such as a
  [`relation_schema`](https://charnelmouse.github.io/autodb/dev/reference/relation_schema.md)
  or
  [`database_schema`](https://charnelmouse.github.io/autodb/dev/reference/database_schema.md)
  object.

- to_remove:

  an integer vector, giving the indices for schemas to be merged into
  other schemas, then removed.

- merge_into:

  an integer vector of the same length as `to_remove`, giving the
  indices for the schemas into which to merge.

- ...:

  further arguments passed on to methods.

## Value

An R object of the same class as `x`, where the relations have been
merged as indicated.

## See also

[`merge_empty_keys`](https://charnelmouse.github.io/autodb/dev/reference/merge_empty_keys.md),
which is based on this function.

## Examples

``` r
rs <- relation_schema(
  list(
    a = list(c("a", "b"), list("a")),
    b = list(c("b", "c"), list("b")),
    b.1 = list(c("b", "d"), list("b")),
    d = list(c("d", "e"), list("d", "e"))
  ),
  letters[1:5]
)
ds <- database_schema(
  rs,
  list(
    list("a", "b", "b", "b"),
    list("b.1", "d", "d", "d")
   )
)
merge_schemas(rs, 3, 2) # merging b and b.1
#> 3 relation schemas
#> 5 attributes: a, b, c, d, e
#> schema a: a, b
#>   key 1: a
#> schema b: b, c, d
#>   key 1: b
#> schema d: d, e
#>   key 1: d
#>   key 2: e
merge_schemas(ds, 3, 2) # also merging their references
#> database schema with 3 relation schemas
#> 5 attributes: a, b, c, d, e
#> schema a: a, b
#>   key 1: a
#> schema b: b, c, d
#>   key 1: b
#> schema d: d, e
#>   key 1: d
#>   key 2: e
#> references:
#> a.{b} -> b.{b}
#> b.{d} -> d.{d}

# merging a schema into itself just removes it
merge_schemas(rs, 3, 3)
#> 3 relation schemas
#> 5 attributes: a, b, c, d, e
#> schema a: a, b
#>   key 1: a
#> schema b: b, c
#>   key 1: b
#> schema d: d, e
#>   key 1: d
#>   key 2: e
merge_schemas(ds, 3, 3)
#> database schema with 3 relation schemas
#> 5 attributes: a, b, c, d, e
#> schema a: a, b
#>   key 1: a
#> schema b: b, c
#>   key 1: b
#> schema d: d, e
#>   key 1: d
#>   key 2: e
#> references:
#> a.{b} -> b.{b}
```
