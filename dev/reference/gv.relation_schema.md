# Generate Graphviz input text to plot relation schemas

Produces text input for Graphviz to make an HTML diagram of a given
relation schema.

## Usage

``` r
# S3 method for class 'relation_schema'
gv(x, name = NA_character_, ...)
```

## Arguments

- x:

  a relation schema, as given by
  [`relation_schema`](https://charnelmouse.github.io/autodb/dev/reference/relation_schema.md)
  or
  [`synthesise`](https://charnelmouse.github.io/autodb/dev/reference/synthesise.md).

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

## See also

The generic
[`gv`](https://charnelmouse.github.io/autodb/dev/reference/gv.md).
