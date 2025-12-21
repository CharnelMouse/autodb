# Test data frames for equivalence under row reordering

A convenience function, mostly used to testing that
[`rejoin`](https://charnelmouse.github.io/autodb/dev/reference/rejoin.md)
works as intended. It checks that data frames have the same dimensions
and column names, with duplicates allowed, then checks they contain the
same data. For the latter step, column names are made unique first, so
columns with duplicate names must be presented in the same order in both
data frames.

## Usage

``` r
df_equiv(df1, df2, digits = getOption("digits"))
```

## Arguments

- df1, df2:

  Data frames.

- digits:

  a positive integer, indicating how many significant digits are to be
  used for numeric and complex variables. A value of NA results in no
  rounding. By default, this uses `getOption("digits")`, similarly to
  [`format`](https://rdrr.io/r/base/format.html). See the note in
  [`print.default`](https://rdrr.io/r/base/print.default.html) about
  digits \>= 16.

## Value

A logical.
