# Schema references

Generic function, returning present (foreign key) references.

## Usage

``` r
references(x, ...)

references(x) <- value
```

## Arguments

- x:

  an R object with references, such as a
  [`database_schema`](https://charnelmouse.github.io/autodb/reference/database_schema.md)
  or
  [`database`](https://charnelmouse.github.io/autodb/reference/database.md)
  object.

- ...:

  further arguments passed on to methods.

- value:

  A list, of the same length as `references`(x, ...).

## Value

A list, giving references.
