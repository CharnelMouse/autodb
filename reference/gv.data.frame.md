# Generate Graphviz input text to plot a data frame

Produces text input for Graphviz to make an HTML diagram of a given data
frame.

## Usage

``` r
# S3 method for class 'data.frame'
gv(x, name = NA_character_, ...)
```

## Arguments

- x:

  a data.frame.

- name:

  a character scalar, giving the name of the record, if any. The name
  must be non-empty, since it is also used to name the single table in
  the plot. Defaults to `NA`: if left missing, it is set to "data".

- ...:

  further arguments passed to or from other methods.

## Value

A scalar character, containing text input for Graphviz.

## Details

The rows in the plotted data frame include information about the
attribute classes.

## See also

The generic
[`gv`](https://charnelmouse.github.io/autodb/reference/gv.md).
