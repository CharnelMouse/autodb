# Create a normalised database from a data frame

This is a wrapper function for applying
[`normalise`](https://charnelmouse.github.io/autodb/reference/normalise.md),
[`autoref`](https://charnelmouse.github.io/autodb/reference/autoref.md),
and
[`decompose`](https://charnelmouse.github.io/autodb/reference/decompose.md).
This takes a data frame and converts it straight into a database, which
is the main intended use case for the package.

## Usage

``` r
autodb(
  df,
  keep_rownames = FALSE,
  digits = getOption("digits"),
  single_ref = FALSE,
  ensure_lossless = TRUE,
  remove_avoidable = FALSE,
  constants_name = "constants",
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
  [`discover`](https://charnelmouse.github.io/autodb/reference/discover.md),
  and for rounding the data before use in
  [`decompose`](https://charnelmouse.github.io/autodb/reference/decompose.md),
  so that the data satisfies the resulting schema. A value of `NA`
  results in no rounding. By default, this uses `getOption("digits")`,
  similarly to [`format`](https://rdrr.io/r/base/format.html). See the
  "Floating-point variables" section for
  [`discover`](https://charnelmouse.github.io/autodb/reference/discover.md)
  for why this rounding is necessary for consistent results across
  different machines. See the note in
  [`print.default`](https://rdrr.io/r/base/print.default.html) about
  `digits >= 16`.

- single_ref:

  a logical, FALSE by default. If TRUE, then only one reference between
  each relation pair is kept when generating foreign key references. If
  a pair has multiple references, the kept reference refers to the
  earliest key for the child relation, as sorted by priority order.

- ensure_lossless:

  a logical, indicating whether to check whether the normalisation is
  lossless. If it is not, then an additional relation is added to the
  final "database", containing a key for `df`. This is enough to make
  the normalisation lossless.

- remove_avoidable:

  a logical, indicating whether to remove avoidable attributes in
  relations. If so, then an attribute are removed from relations if the
  keys can be changed such that it is not needed to preserve the given
  functional dependencies.

- constants_name:

  a scalar character, giving the name for any relation created to store
  constant attributes. If this is the same as a generated relation name,
  it will be changed, with a warning, to ensure that all relations have
  a unique name.

- progress:

  a logical, for whether to display progress to the user during
  dependency search in
  [`discover`](https://charnelmouse.github.io/autodb/reference/discover.md).

- progress_file:

  a scalar character or a connection. If `progress` is non-zero,
  determines where the progress is written to, in the same way as the
  `file` argument for [`cat`](https://rdrr.io/r/base/cat.html).

- ...:

  further arguments passed on to
  [`discover`](https://charnelmouse.github.io/autodb/reference/discover.md).

## Value

A
[`database`](https://charnelmouse.github.io/autodb/reference/database.md),
containing the data in `df` within the inferred database schema.

## Details

Since `decompose` only works with functional dependencies, not
approximate dependencies, the accuracy in `discover` is fixed as 1.

## Examples

``` r
# simple example
autodb(ChickWeight)
#> database with 2 relations
#> 4 attributes: weight, Time, Chick, Diet
#> relation Chick: Chick, Diet; 50 records
#>   key 1: Chick
#> relation Time_Chick: Time, Chick, weight; 578 records
#>   key 1: Time, Chick
#> references:
#> Time_Chick.{Chick} -> Chick.{Chick}
```
