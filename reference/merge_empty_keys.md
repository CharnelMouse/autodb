# Merge relation schemas with empty keys

Merges an object's schemas with empty keys. The remaining such schema
contains all attributes contained in such schemas.

## Usage

``` r
merge_empty_keys(x)
```

## Arguments

- x:

  a relational schema object, such as a
  [`relation_schema`](https://charnelmouse.github.io/autodb/reference/relation_schema.md)
  or
  [`database_schema`](https://charnelmouse.github.io/autodb/reference/database_schema.md)
  object.

## Value

An R object of the same class as `x`, where relations with an empty key
have been merged into a single relation.

## Details

This function is not itself generic, but makes use of the generic
functions
[`keys`](https://charnelmouse.github.io/autodb/reference/keys.md) and
[`merge_schemas`](https://charnelmouse.github.io/autodb/reference/merge_schemas.md).
Any input class with valid methods for these generic functions can be
passed into this function.

For
[`database_schema`](https://charnelmouse.github.io/autodb/reference/database_schema.md)
objects, references involving the schemas with empty keys are updated to
refer to the merged schema.

## See also

[`merge_schemas`](https://charnelmouse.github.io/autodb/reference/merge_schemas.md),
on which this function is based.
