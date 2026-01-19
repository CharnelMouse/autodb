# Generate Graphviz input text to plot databases

Produces text input for Graphviz to make an HTML diagram of a given
database.

## Usage

``` r
# S3 method for class 'database'
gv(x, name = NA_character_, nest_level = Inf, ...)
```

## Arguments

- x:

  a database, as returned by
  [`autoref`](https://charnelmouse.github.io/autodb/dev/reference/autoref.md)
  or
  [`autodb`](https://charnelmouse.github.io/autodb/dev/reference/autodb.md).

- name:

  a scalar character, giving the name of the database, if any. This name
  is used for the resulting graph, to allow for easier combining of
  graphs into a single diagram if required.

- nest_level:

  an integer, giving the amount of nesting allowed when giving the class
  of a list column. Since lists can hold anything in R, this allows
  showing common element classes and lengths.

- ...:

  further arguments passed to or from other methods.

## Value

A scalar character, containing text input for Graphviz.

## Details

Each relation in the database is presented as a set of rows, one for
each attribute in the relation. These rows include information about the
attribute classes.

## See also

The generic
[`gv`](https://charnelmouse.github.io/autodb/dev/reference/gv.md).
