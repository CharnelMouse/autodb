# Generate Graphviz input text to plot database schemas

Produces text input for Graphviz to make an HTML diagram of a given
database schema.

## Usage

``` r
# S3 method for class 'database_schema'
gv(x, name = NA_character_, ...)
```

## Arguments

- x:

  a database schema, as given by
  [`normalise`](https://charnelmouse.github.io/autodb/reference/normalise.md),
  [`synthesise`](https://charnelmouse.github.io/autodb/reference/synthesise.md),
  or
  [`autoref`](https://charnelmouse.github.io/autodb/reference/autoref.md).

- name:

  a character scalar, giving the name of the schema, if any.

- ...:

  further arguments passed to or from other methods.

## Value

A scalar character, containing text input for Graphviz.

## Details

Each relation in the schema is presented as a set of rows, one for each
attribute in the relation. These rows do not include information about
the attribute classes.

Any foreign key references are represented by arrows between the
attribute pairs.

## See also

The generic
[`gv`](https://charnelmouse.github.io/autodb/reference/gv.md).
