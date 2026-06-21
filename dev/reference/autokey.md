# Create a relation from a data frame

This is a wrapper function for applying
[`discover_keys`](https://charnelmouse.github.io/autodb/dev/reference/discover_keys.md),
[`relation_schema`](https://charnelmouse.github.io/autodb/dev/reference/relation_schema.md),
[`create`](https://charnelmouse.github.io/autodb/dev/reference/create.md),
and
[`insert`](https://charnelmouse.github.io/autodb/dev/reference/insert.md).
This takes a data frame and adds its keys, resulting in a single
relation. If only the keys are required, this can be much quicker than
running
[`autodb`](https://charnelmouse.github.io/autodb/dev/reference/autodb.md).

## Usage

``` r
autokey(
  df,
  keep_rownames = FALSE,
  digits = getOption("digits"),
  progress = FALSE,
  progress_file = "",
  ...
)
```

## Arguments

- df:

  a data.frame, containing the data to be normalised.

- keep_rownames:

  a logical or a string, indicating whether to include the row names as
  a column. If a string is given, it is used as the name for the column,
  otherwise the column is named "row". Like with the other column names,
  the function returns an error if this results in duplicate column
  names. Set to FALSE by default.

- digits:

  a positive integer, indicating how many significant digits are to be
  used for numeric and complex variables. This is used for both
  pre-formatting in
  [`discover_keys`](https://charnelmouse.github.io/autodb/dev/reference/discover_keys.md),
  and for rounding the data before use in
  [`insert`](https://charnelmouse.github.io/autodb/dev/reference/insert.md),
  so that the data satisfies the resulting schema. A value of `NA`
  results in no rounding. By default, this uses `getOption("digits")`,
  similarly to [`format`](https://rdrr.io/r/base/format.html). See the
  "Floating-point variables" section for
  [`discover`](https://charnelmouse.github.io/autodb/dev/reference/discover.md)
  for why this rounding is necessary for consistent results across
  different machines. See the note in
  [`print.default`](https://rdrr.io/r/base/print.default.html) about
  `digits >= 16`.

- progress:

  a logical, for whether to display progress to the user during
  dependency search in
  [`discover`](https://charnelmouse.github.io/autodb/dev/reference/discover.md).

- progress_file:

  a scalar character or a connection. If `progress` is non-zero,
  determines where the progress is written to, in the same way as the
  `file` argument for [`cat`](https://rdrr.io/r/base/cat.html).

- ...:

  further arguments passed on to
  [`discover_keys`](https://charnelmouse.github.io/autodb/dev/reference/discover_keys.md).

## Value

A
[`relation`](https://charnelmouse.github.io/autodb/dev/reference/relation.md)
of length 1, containing the data in `df` and its keys.

## Examples

``` r
# simple example
autokey(ChickWeight)
#> 1 relation
#> 4 attributes: weight, Time, Chick, Diet
#> relation data: Time, Chick, weight, Diet; 578 records
#>   key 1: Time, Chick
```
