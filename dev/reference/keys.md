# Relational data keys

Generic function, with the only given method fetching candidate key
lists for relation schemas.

## Usage

``` r
keys(x, ...)

keys(x, ...) <- value
```

## Arguments

- x:

  a relational schema object, such as a
  [`relation_schema`](https://charnelmouse.github.io/autodb/dev/reference/relation_schema.md)
  or
  [`database_schema`](https://charnelmouse.github.io/autodb/dev/reference/database_schema.md)
  object, or a relational data object, such as a
  [`relation`](https://charnelmouse.github.io/autodb/dev/reference/relation.md)
  or
  [`database`](https://charnelmouse.github.io/autodb/dev/reference/database.md)
  object.

- ...:

  further arguments passed on to methods.

- value:

  A list of lists of character vectors, of the same length as
  `keys(x, ...)`. The number of keys for an element of `x` can be
  changed.

## Value

A list containing lists of unique character vectors, representing
candidate keys for each element of `x`.
