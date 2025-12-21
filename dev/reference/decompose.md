# Decompose a data frame based on given normalised dependencies

Decomposes a data frame into several relations, based on the given
database schema. It's intended that the data frame satisfies all the
functional dependencies implied by the schema, such as if the schema was
constructed from the same data frame. If this is not the case, the
function will returns an error.

## Usage

``` r
decompose(
  df,
  schema,
  keep_rownames = FALSE,
  digits = getOption("digits"),
  check = TRUE
)
```

## Arguments

- df:

  a data.frame, containing the data to be normalised.

- schema:

  a database schema with foreign key references, such as given by
  [`autoref`](https://charnelmouse.github.io/autodb/dev/reference/autoref.md).

- keep_rownames:

  a logical or a string, indicating whether to include the row names as
  a column. If a string is given, it is used as the name for the column,
  otherwise the column is named "row". Set to FALSE by default.

- digits:

  a positive integer, indicating how many significant digits are to be
  used for numeric and complex variables. A value of `NA` results in no
  rounding. By default, this uses `getOption("digits")`, similarly to
  [`format`](https://rdrr.io/r/base/format.html). See the
  "Floating-point variables" section for
  [`discover`](https://charnelmouse.github.io/autodb/dev/reference/discover.md)
  for why this rounding is necessary for consistent results across
  different machines. See the note in
  [`print.default`](https://rdrr.io/r/base/print.default.html) about
  `digits >= 16`.

- check:

  a logical, indicating whether to check that `df` satisfies the
  functional dependencies enforced by `schema` before creating the
  result. This can find key violations without spending time creating
  the result first, but is redundant if `df` was used to create `schema`
  in the first place.

## Value

A
[`database`](https://charnelmouse.github.io/autodb/dev/reference/database.md)
object, containing the data in `df` within the database schema given in
`schema`.

## Details

If the schema was constructed using approximate dependencies for the
same data frame, `decompose` returns an error, to prevent either
duplicate records or lossy decompositions. This is temporary: for the
next update, we plan to add an option to allow this, or to add
"approximate" equivalents of databases and database schemas.
