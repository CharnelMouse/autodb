# Create instance of a schema

Create a relation data object, using the given relational schema object,
with the resulting relations empty and ready for data insertion using
[`insert`](https://charnelmouse.github.io/autodb/reference/insert.md).

## Usage

``` r
create(x, ...)
```

## Arguments

- x:

  a relational schema object, representing the schema to create an
  instance of, such as a
  [`relation_schema`](https://charnelmouse.github.io/autodb/reference/relation_schema.md)
  or
  [`database_schema`](https://charnelmouse.github.io/autodb/reference/database_schema.md)
  object.

- ...:

  further arguments passed on to methods.

## Value

An instance of the schema. For example, calling `create` on a
[`database_schema`](https://charnelmouse.github.io/autodb/reference/database_schema.md)
creates a
[`database`](https://charnelmouse.github.io/autodb/reference/database.md),
where all the relations contain zero records.
