# Rename relational data attributes

Generic function, for renaming attributes present in a database-like
structure.

## Usage

``` r
rename_attrs(x, names, ...)
```

## Arguments

- x:

  an object with an `attrs_order` attribute. This includes relational
  schema objects, such as a
  [`relation_schema`](https://charnelmouse.github.io/autodb/reference/relation_schema.md)
  or
  [`database_schema`](https://charnelmouse.github.io/autodb/reference/database_schema.md)
  object, relational data objects, such as a
  [`relation`](https://charnelmouse.github.io/autodb/reference/relation.md)
  or
  [`database`](https://charnelmouse.github.io/autodb/reference/database.md)
  object, and
  [`functional_dependency`](https://charnelmouse.github.io/autodb/reference/functional_dependency.md)
  objects.

- names:

  a character vector of the same length as `attrs_order(x)`, with no
  duplicated elements, to be used as the new attribute names.

- ...:

  further arguments passed on to methods.

## Value

A relational object of the same type as `x`, with attributes renamed
consistently across the whole object.

## Details

This function has a different intended use to re-assigning
[`attrs_order`](https://charnelmouse.github.io/autodb/reference/attrs_order.md):
that is intended only for rearranging the order of the attributes,
without renaming them. This is intended for renaming the attributes
without re-ordering them.
