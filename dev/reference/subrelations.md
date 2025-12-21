# Database subrelations

Generic function, returning subrelations for `x`.

## Usage

``` r
subrelations(x, ...)
```

## Arguments

- x:

  an R object, intended to be some sort of database-like object that
  contains relations, such as a
  [`database`](https://charnelmouse.github.io/autodb/dev/reference/database.md)
  object.

- ...:

  further arguments passed on to methods.

## Value

A relation-type object, or a list of relation-type objects if the
subrelation isn't vectorised. For example, if `x` is a
[`database`](https://charnelmouse.github.io/autodb/dev/reference/database.md),
the result is the contained
[`relation`](https://charnelmouse.github.io/autodb/dev/reference/relation.md).
