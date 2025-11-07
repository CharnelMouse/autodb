# Combine R Objects by Rows or Columns

[`rbind`](https://rdrr.io/r/base/cbind.html) takes "a sequence of
vector, matrix or data-frame arguments", and combines by rows for the
latter. However, as of R 4.1, calling this on data frame with zero
columns always returns zero rows, due to the issue mentioned for
[`df_duplicated`](https://charnelmouse.github.io/autodb/reference/df_duplicated.md).
This function adds zero-column data frames as a special case.

## Usage

``` r
df_rbind(...)
```

## Arguments

- ...:

  data frames.

## Value

A data frame containing the `...` arguments row-wise.

## See also

[`df_duplicated`](https://charnelmouse.github.io/autodb/reference/df_duplicated.md)
