# Remove database schema relations not linked to the given relations

Filters a database schema's relations, keeping only the given relations,
and those considered ancestors via foreign key references. Foreign key
references involving removed relations are also removed.

## Usage

``` r
# S3 method for class 'database_schema'
reduce(x, main, ...)
```

## Arguments

- x:

  A database schema, whose relations are to be filtered.

- main:

  A character vector, containing names of relations to be considered as
  the "main" relations.

- ...:

  further arguments passed to or from other methods.

## Value

A database schema, with the auxiliary relations and foreign key
references removed.

## Details

This method takes a given set of main relations, rather than inferring
them.

Using
[`rejoin`](https://charnelmouse.github.io/autodb/dev/reference/rejoin.md)
on the database resulting from decomposing a data frame with the reduced
schema is likely to fail or return incomplete results.
