# Key discovery with MCSS

Finds all the keys for a data frame, ignoring duplicate rows.

## Usage

``` r
discover_keys(
  df,
  keep_rownames = FALSE,
  digits = getOption("digits"),
  exclude = character(),
  exclude_class = character(),
  size_limit = ncol(df),
  progress = FALSE,
  progress_file = ""
)
```

## Arguments

- df:

  a data.frame, the relation to evaluate.

- keep_rownames:

  a logical or a string, indicating whether to include the row names as
  a column. If a string is given, it is used as the name for the column,
  otherwise the column is named "row". Like with the other column names,
  the function returns an error if this results in duplicate column
  names. Set to FALSE by default.

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

- exclude:

  a character vector, containing names of attributes to not consider as
  members of keys. If names are given that aren't present in `df`, the
  user is given a warning.

- exclude_class:

  a character vector, indicating classes of attributes to not consider
  as members of keys. Attributes are excluded if they inherit from any
  given class.

- size_limit:

  an integer, indicating the largest key size to search for. By default,
  this is large enough to allow all attributes.

- progress:

  a logical, for whether to display progress to the user during
  dependency search in
  [`discover`](https://charnelmouse.github.io/autodb/dev/reference/discover.md).

- progress_file:

  a scalar character or a connection. If `progress` is non-zero,
  determines where the progress is written to, in the same way as the
  `file` argument for [`cat`](https://rdrr.io/r/base/cat.html).

## Value

A list of character vectors, containing the discovered keys. The
attributes within each key are given in the same order as in `df`.

## Details

Column names for `df` must be unique.

The search algorithm was adapted from the FDHits algorithm used for
[`discover`](https://charnelmouse.github.io/autodb/dev/reference/discover.md).
It is likely to be an implementation of the HPIValid algorithm, although
it wasn't used directly as a source. It has the same implications with
respect to floating-point variables.

## References

FDHits: Bleifuss T., Papenbrock T., Bläsius T., Schirneck M, Naumann F.
(2024) Discovering Functional Dependencies through Hitting Set
Enumeration. *Proc. ACM Manag. Data*, **2, 1**, 43:1–24.

HPIValid: Birnick J., Bläsius T., Friedrich T., Naumann F., Papenbrock
T., Schirneck M. (2020) Hitting set enumeration with partial information
for unique column combination discovery. *Proceedings of the VLDB
Endowment*, **13, 12**, 2270–2283.

## Examples

``` r
# simple example
discover_keys(ChickWeight)
#> [[1]]
#> [1] "Time"  "Chick"
#> 

# example with spurious key
discover_keys(CO2)
#> [[1]]
#> [1] "Treatment" "conc"      "uptake"   
#> 
#> [[2]]
#> [1] "Plant" "conc" 
#> 
# exclude attributes that can't be determinants.
# in this case, the numeric attributes are now
# not determined by anything, because of repeat measurements
# with no variable to mark them as such.
discover_keys(CO2, exclude_class = "numeric")
#> list()
# exclude keys spuriously using the measurement attribute
discover_keys(CO2, exclude = "uptake")
#> [[1]]
#> [1] "Plant" "conc" 
#> 
```
