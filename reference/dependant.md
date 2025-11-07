# Dependants

Generic function, with the only given method fetching dependants for
functional dependencies.

## Usage

``` r
dependant(x, ...)

dependant(x, ...) <- value
```

## Arguments

- x:

  an R object. For the given method, a
  [`functional_dependency`](https://charnelmouse.github.io/autodb/reference/functional_dependency.md).

- ...:

  further arguments passed on to methods.

- value:

  A character vector of the same length as `dependant(x, ...)`.

## Value

A character vector containing dependants.
