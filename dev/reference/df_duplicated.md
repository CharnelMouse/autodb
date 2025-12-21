# Determine Duplicate Elements

[`duplicated`](https://rdrr.io/r/base/duplicated.html) "determines which
elements of a vector or data frame are duplicates of elements with
smaller subscripts, and returns a logical vector indicating which
elements (rows) are duplicates". However, as of R 4.1, calling this on a
data frame with zero columns always returns an empty logical vector.
This has repercussions on other functions that use `duplicated`, such as
[`unique`](https://rdrr.io/r/base/unique.html) and
[`anyDuplicated`](https://rdrr.io/r/base/duplicated.html). These
functions add zero-column data frames as a special case.

## Usage

``` r
df_duplicated(x, incomparables = FALSE, fromLast = FALSE, ...)

df_unique(x, incomparables = FALSE, fromLast = FALSE, ...)

df_anyDuplicated(x, incomparables = FALSE, fromLast = FALSE, ...)

df_records(x, use_rownames = FALSE, use_colnames = FALSE)
```

## Arguments

- x:

  a data frame.

- incomparables:

  a vector of values that cannot be compared. `FALSE` is a special
  value, meaning that all values can be compared, and may be the only
  value accepted for methods other than the default. It will be coerced
  internally to the same type as `x`.

- fromLast:

  logical indicating if duplication should be considered from the
  reverse side, i.e., the last (or rightmost) of identical elements
  would correspond to `duplicated = FALSE`.

- ...:

  arguments for particular methods.

- use_rownames:

  a logical, FALSE by default, indicating whether row values should keep
  the row names from `x`. Defaults to FALSE.

- use_colnames:

  a logical, FALSE by default, indicating whether row values should keep
  the column names from `x` for their elements. Defaults to FALSE.

## Value

For `df_duplicated`, a logical vector with one element for each row.

For `df_unique`, a data frame is returned with the same columns, but
possible fewer rows (and with row names from the first occurrences of
the unique rows).

For `df_anyDuplicated`, an integer or real vector of length one with
value the 1-based index of the first duplicate if any, otherwise 0.

For `df_records`, a list of the row values in `x`. This is based on a
step in
[`duplicated.data.frame`](https://rdrr.io/r/base/duplicated.html).
However, for data frames with zero columns, special handling returns a
list of empty row values, one for each row in `x`. Without special
handling, this step returns an empty list. This was the cause for
[`duplicated`](https://rdrr.io/r/base/duplicated.html) returning
incorrect results for zero-column data frames in older versions of R.

## See also

[`df_rbind`](https://charnelmouse.github.io/autodb/dev/reference/df_rbind.md)

## Examples

``` r
# row values for a 5x0 data frame
x <- data.frame(a = 1:5)[, FALSE, drop = FALSE]
do.call(Map, unname(c(list, x))) # original step returns empty list
#> list()
df_records(x) # corrected version preserves row count
#> [[1]]
#> list()
#> 
#> [[2]]
#> list()
#> 
#> [[3]]
#> list()
#> 
#> [[4]]
#> list()
#> 
#> [[5]]
#> list()
#> 
```
