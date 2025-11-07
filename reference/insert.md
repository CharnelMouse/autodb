# Insert data

Generic function for inserting a data frame of data into an object.

## Usage

``` r
insert(
  x,
  vals,
  relations = names(x),
  all = FALSE,
  keep_rownames = FALSE,
  digits = getOption("digits"),
  ...
)
```

## Arguments

- x:

  a relational data object, into which to insert data, such as a
  [`relation`](https://charnelmouse.github.io/autodb/reference/relation.md)
  or
  [`database`](https://charnelmouse.github.io/autodb/reference/database.md)
  object.

- vals:

  a data frame, containing data to insert. Column names must be unique.

- relations:

  a character vector, containing names of elements of `x` into which to
  insert data. By default, `insert` attempts to insert data into every
  element.

- all:

  a logical, indicating whether `vals` is required to contain all
  attributes of all elements of `x[relations]`. By default, it is not,
  and data is only inserted into elements of `x[relations]` whose
  attributes are all present in `vals`.

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
  [`discover`](https://charnelmouse.github.io/autodb/reference/discover.md)
  for why this rounding is necessary for consistent results across
  different machines. See the note in
  [`print.default`](https://rdrr.io/r/base/print.default.html) about
  `digits >= 16`.

- ...:

  further arguments pass on to methods.

## Value

An R object of the same class as `x`, containing the additional new
data.

## Details

This function is intended for inserting into an object that is itself
comprised of data frames, such as a
[`relation`](https://charnelmouse.github.io/autodb/reference/relation.md)
or a
[`database`](https://charnelmouse.github.io/autodb/reference/database.md).
The given methods have the following behaviour:

- If an empty set of data is inserted, into a non-empty object element,
  nothing happens.

- If an empty set of data is inserted into an empty object element, the
  resulting element is also empty, but takes on the attribute/column
  classes of the inserted data. This is done to prevent having to know
  attribute classes during object creation.

- Insertion can fail if inserting would violate object constraints. For
  example, databases cannot have data inserted that would violate
  candidate/foreign key constraints.

- For other cases, the data is inserted in an object element in the same
  way as using [`rbind`](https://rdrr.io/r/base/cbind.html), followed by
  [`unique`](https://rdrr.io/r/base/unique.html).

While key violations prevent insertion, re-insertion of existing records
in an object element does not. This makes insertion equivalent to an
`INSERT OR IGNORE` expression in SQL. In particular, it is somewhat like
using this expression in SQLite, since that implementation uses dynamic
typing.

If `vals` contains attributes not included in
[`attrs_order`](https://charnelmouse.github.io/autodb/reference/attrs_order.md)`(x)`,
`insert` throws an error, since those attributes can't be inserted.

If a partial set of attributes is inserted, and `all` is `FALSE`, then
data is only inserted into components of `x[relations]` whose required
attributes are all present in `vals`. If `all` is `TRUE`, `insert`
returns an error instead. This is useful when specifying `relations`: in
that case, you often intend to insert into all of the specified
elements, so not including all the required attributes is a mistake, and
`all = TRUE` prevents it.

If `all` is `TRUE`, `insert` throws an error in this case: This ensures
you insert into all members of a specified value of `relations`.
