# Determinant sets

Generic function, with the only given method fetching determinant sets
for functional dependencies.

## Usage

``` r
detset(x, ...)

detset(x, ...) <- value
```

## Arguments

- x:

  an R object. For the given method, a
  [`functional_dependency`](https://charnelmouse.github.io/autodb/dev/reference/functional_dependency.md).

- ...:

  further arguments passed on to methods.

- value:

  A character vector of the same length as `detset(x, ...)`.

## Value

A list containing determinant sets, each consisting of a character
vector with unique elements.
