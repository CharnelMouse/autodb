# Schema subschemas

Generic function, returning subschemas for `x`.

## Usage

``` r
subschemas(x, ...)
```

## Arguments

- x:

  an R object, intended to be some sort of schema that contains other
  schemas, such as a
  [`database_schema`](https://charnelmouse.github.io/autodb/reference/database_schema.md)
  object.

- ...:

  further arguments passed on to methods.

## Value

A schema-type object, or a list of schema-type objects if the subschema
isn't vectorised. For example, if `x` is a
[`database_schema`](https://charnelmouse.github.io/autodb/reference/database_schema.md),
the result is the contained
[`relation_schema`](https://charnelmouse.github.io/autodb/reference/relation_schema.md).
