# Generate Graphviz input text to plot relations

Produces text input for Graphviz to make an HTML diagram of a given
relation.

## Usage

``` r
# S3 method for class 'relation'
gv(x, name = NA_character_, ...)
```

## Arguments

- x:

  a
  [`relation`](https://charnelmouse.github.io/autodb/reference/relation.md).

- name:

  a character scalar, giving the name of the schema, if any.

- ...:

  further arguments passed to or from other methods.

## Value

A scalar character, containing text input for Graphviz.

## Details

Each relation is presented as a set of rows, one for each attribute in
the relation. These rows include information about the attribute
classes.

## See also

The generic
[`gv`](https://charnelmouse.github.io/autodb/reference/gv.md).
