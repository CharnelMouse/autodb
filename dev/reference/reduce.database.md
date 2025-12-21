# Remove database relations not linked to the main relations

Filters a database's relations, keeping only the main relations, and
those considered ancestors via foreign key references. Foreign key
references involving removed relations are also removed.

## Usage

``` r
# S3 method for class 'database'
reduce(x, main, ...)
```

## Arguments

- x:

  A database, whose relations are to be filtered.

- main:

  A character vector, containing names of relations to be considered as
  the "main" relations. If missing, taken to be the names of all
  relations with the largest record count.

- ...:

  further arguments passed to or from other methods.

## Value

A database, with the auxiliary relations and foreign key references
removed.

## Details

The main relations are considered to be the relations with the largest
number of records.

Using
[`rejoin`](https://charnelmouse.github.io/autodb/dev/reference/rejoin.md)
on the database resulting from `reduce` is likely to fail or return
incomplete results.
