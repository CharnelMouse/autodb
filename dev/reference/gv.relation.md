# Generate Graphviz input text to plot relations

Produces text input for Graphviz to make an HTML diagram of a given
relation.

## Usage

``` r
# S3 method for class 'relation'
gv(x, name = NA_character_, nest_level = Inf, ...)
```

## Arguments

- x:

  a
  [`relation`](https://charnelmouse.github.io/autodb/dev/reference/relation.md).

- name:

  a character scalar, giving the name of the schema, if any.

- nest_level:

  an integer, giving the amount of nesting allowed when giving the class
  of a list column. Since lists can hold anything in R, this allows
  showing common element classes and lengths.

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
[`gv`](https://charnelmouse.github.io/autodb/dev/reference/gv.md).
