# Relational data attribute order

Generic function, fetching attribute order for relational objects.

## Usage

``` r
attrs_order(x, ...)

attrs_order(x, ...) <- value
```

## Arguments

- x:

  an R object, such as a
  [`functional_dependency`](https://charnelmouse.github.io/autodb/reference/functional_dependency.md),
  [`relation_schema`](https://charnelmouse.github.io/autodb/reference/relation_schema.md),
  [`relation`](https://charnelmouse.github.io/autodb/reference/relation.md),
  [`database_schema`](https://charnelmouse.github.io/autodb/reference/database_schema.md),
  or
  [`database`](https://charnelmouse.github.io/autodb/reference/database.md)
  object.

- ...:

  further arguments passed on to methods.

- value:

  A character vector of the same length as `attrs_order(x, ...)`.

## Value

A character vector, giving attributes in the order in which they're
prioritised for sorting within `x`.

## Details

All classes in `autodb` contain an `attrs_order` attribute. It gives an
easy way to find a list of all attributes/variables involved in an
object, but its main purpose is to also assign those attributes a
consistent order when printing or plotting the object.
