# Remove relations not linked to the main relations

Filters an object's relations, keeping only the main relations, and
those considered ancestors via foreign key references. Foreign key
references involving removed relations are also removed.

## Usage

``` r
reduce(x, ...)
```

## Arguments

- x:

  An object whose relations are to be filtered.

- ...:

  further arguments passed to or from other methods.

## Value

An object of the same class as `x`, with the auxiliary relations and
foreign key references removed.

## Details

Details on how the main tables are chosen are given in individual
methods.

This function is mostly intended for simplifying a database, or a
database schema, for the purposes of exploration, particularly by
examining plots. While the filtering might remove important auxiliary
relations, it's also likely to remove any based on spurious
dependencies, of which some databases can contain many.

## See also

[`reduce.database_schema`](https://charnelmouse.github.io/autodb/dev/reference/reduce.database_schema.md),
[`reduce.database`](https://charnelmouse.github.io/autodb/dev/reference/reduce.database.md).
