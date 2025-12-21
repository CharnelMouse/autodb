# Relational data attributes

Generic function, for fetching attribute sets for elements of a
relational object.

## Usage

``` r
attrs(x, ...)

attrs(x, ...) <- value
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

  A character vector of the same length as `attrs(x, ...)`.

## Value

A list, containing a character vector for each element of `x`.
