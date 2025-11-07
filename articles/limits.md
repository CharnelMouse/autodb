# Limitations

``` r
library(autodb)
#> 
#> Attaching package: 'autodb'
#> The following object is masked from 'package:stats':
#> 
#>     decompose
```

``` r
if (requireNamespace("DiagrammeR", quietly = TRUE)) {
  show <- function(x) DiagrammeR::grViz(gv(x), width = "100%")
  maybe_plot <- function(x) DiagrammeR::grViz(gv(x), width = "100%")
}else{
  show <- print
  maybe_plot <- function(x) invisible(NULL)
}
```

Automatic data normalisation, such as done by `autodb`, is useful, but
it isn’t magic. This vignette covers some common issues that this
approach can’t address.

## Meaningful duplicate rows / row order

`autodb` assumes that the input data is relational data (1NF), where the
data can be considered as a set of records. This means no duplicate rows
– we can remove any duplicates without loss of information – and that
the order of the rows doesn’t matter. If this isn’t the case, the data
will need to be manipulated first, so that it is.

## Value-based constraints

The FD search is agnostic to what kind of data is stored in each
attribute; it just cares about when records have the same value for a
given attribute. It can’t find constraints that depend on the actual
data values. For example, it can’t determine whether an numeric
attribute’s values are within an expected interval: that would require
using knowledge about the attribute’s class. Similarly, it can’t
determine inequality constraints between numeric attributes: that would
require knowing both attributes are numeric, and that comparing them is
suitable (they aren’t integers that represent non-ordinal IDs, for
example).

## Semantic types

The above section noted that the search can’t account for data classes,
i.e. the attributes’ syntactic classes (integer, float, etc.). It also
can’t account for semantic classes.

Suppose we have a dataset of publication citations, where each row
contains the citer and citee IDs, plus supplementary information about
both publications. The resulting database given by `autodb` has the
following schema, after giving the relations appropriate names:

``` r
relation_schema(
  list(
    citation = list(c("citer_id", "citee_id"), list(c("citer_id", "citee_id"))),
    citer = list(c("citer_id", "citer_title", "citer_author", "citer_year"), list("citer_id")),
    citee = list(c("citee_id", "citee_title", "citee_author", "citee_year"), list("citee_id"))
  ),
  c(
    "citer_id", "citer_title", "citer_author", "citer_year",
    "citee_id", "citee_title", "citee_author", "citee_year"
  )
) |>
  database_schema(
    list(
      list("citation", "citer_id", "citer", "citer_id"),
      list("citation", "citee_id", "citee", "citee_id")
    )
  ) |>
  show()
```

Of course, citers and citees are both publications of the same type, so
they shouldn’t have separate relations:

``` r
database_schema(
  relation_schema(
    list(
      citation = list(c("citer", "citee"), list(c("citer", "citee"))),
      publication = list(c("id", "title", "author", "year"), list("id"))
    ),
    c("citer", "citee", "id", "title", "author", "year")
  ),
  list(
    list("citation", "citer", "publication", "id"),
    list("citation", "citee", "publication", "id")
  )
) |>
  show()
```

We make this improvement because `citer_id` and `citee_id` values are,
semantically, the same class of object, and the same goes for authors,
titles, etc. Semantic classes are not something that can be inferred by
just looking at the syntactic/data classes.

If we don’t account for this semantic identity, the separate citer and
citee information relations in the original schema can both hold
information for the same publication. This introduces the possibility
that they hold different information, so the data for that publication
is incoherent.

Currently, the only way to account for this semantic identity to create
or modify the schema manually, as above.

## Table merges don’t fix issues with `merge.data.frame`

This is specific to `autodb`, rather than the relational model in
general.

Rejoining databases, and checking relations satisfy foreign key
constraints, is done using `merge.data.frame`. This means that data
classes that don’t work properly in `merge` aren’t guaranteed to work
properly in `autodb`.

Any such issues come from how certain data classes are handled during
merges in the base language, so they are issues with R, rather than with
`autodb`, and I have no plans to fix them. If `autodb` seems to have odd
failures, check that used data classes behave correctly in merges.

For example, in older versions of R, the built-in POSIXct date/time
class didn’t have values merged properly, because the merge didn’t
account for differences in time zone / daylight saving time. This would
result in, for example, the `nycflights13::weather` data set violating
the foreign key constraints of its own discovered schema, since one
foreign key used a POSIXct attribute.

A more complex example, that still applies and probably always will, is
a merge where two attributes being merged on have different classes. In
general, this is allowed: since `autodb` is written for R, a
dynamically-typed language, it follows SQLite in not constraining the
user much when it comes to data classes in schemas. For primitive
classes, R’s class coercion usually makes things work as you’d expect.

In practice, having an attribute’s class vary across the relation it
belongs to is asking for trouble.

In particular, if it’s represented by a factor in one relation, and a
non-factor, non-character class in another, where the latter has values
not in the former’s levels, then merging them will cause issues. This is
not unexpected, it’s just how coercing on factors works in R.

For example, we can define these data frames:

``` r
df_badmerge_int <- cbind(
  expand.grid(
    a = c(NA, 0L, 1L),
    b = c(NA, FALSE, TRUE)
  ),
  row = 1:9
)
df_badmerge_factor <- df_badmerge_int
df_badmerge_factor$a <- as.factor(df_badmerge_factor$a)
knitr::kable(df_badmerge_int)
```

|   a | b     | row |
|----:|:------|----:|
|  NA | NA    |   1 |
|   0 | NA    |   2 |
|   1 | NA    |   3 |
|  NA | FALSE |   4 |
|   0 | FALSE |   5 |
|   1 | FALSE |   6 |
|  NA | TRUE  |   7 |
|   0 | TRUE  |   8 |
|   1 | TRUE  |   9 |

``` r
df_badmerge_logical <- df_badmerge_int
df_badmerge_logical$a <- as.logical(df_badmerge_logical$a)
names(df_badmerge_logical)[[3]] <- "row2"
knitr::kable(df_badmerge_logical)
```

| a     | b     | row2 |
|:------|:------|-----:|
| NA    | NA    |    1 |
| FALSE | NA    |    2 |
| TRUE  | NA    |    3 |
| NA    | FALSE |    4 |
| FALSE | FALSE |    5 |
| TRUE  | FALSE |    6 |
| NA    | TRUE  |    7 |
| FALSE | TRUE  |    8 |
| TRUE  | TRUE  |    9 |

We can then merge the data frame with logical `a` with the other two,
keeping the `row` attributes to track which records were merged.

Whichever other data frame we merge with, the two sets of `a` values
have different classes, so R does coercion. When merging with just `a`,
this gives the result we’d expect, for both other data frames and
regardless of merge order. For the integer version, the logical values
are coerced to integers:

``` r
knitr::kable(merge(
  df_badmerge_int[, c("a", "row")],
  df_badmerge_logical[, c("a", "row2")]
))
```

|   a | row | row2 |
|----:|----:|-----:|
|   0 |   2 |    2 |
|   0 |   2 |    5 |
|   0 |   2 |    8 |
|   0 |   5 |    2 |
|   0 |   5 |    5 |
|   0 |   5 |    8 |
|   0 |   8 |    2 |
|   0 |   8 |    5 |
|   0 |   8 |    8 |
|   1 |   6 |    6 |
|   1 |   6 |    3 |
|   1 |   6 |    9 |
|   1 |   3 |    6 |
|   1 |   3 |    3 |
|   1 |   3 |    9 |
|   1 |   9 |    6 |
|   1 |   9 |    3 |
|   1 |   9 |    9 |
|  NA |   1 |    1 |
|  NA |   1 |    7 |
|  NA |   1 |    4 |
|  NA |   7 |    1 |
|  NA |   7 |    7 |
|  NA |   7 |    4 |
|  NA |   4 |    1 |
|  NA |   4 |    7 |
|  NA |   4 |    4 |

``` r
knitr::kable(merge(
  df_badmerge_logical[, c("a", "row2")],
  df_badmerge_int[, c("a", "row")]
))
```

| a     | row2 | row |
|:------|-----:|----:|
| FALSE |    2 |   2 |
| FALSE |    2 |   5 |
| FALSE |    2 |   8 |
| FALSE |    5 |   2 |
| FALSE |    5 |   5 |
| FALSE |    5 |   8 |
| FALSE |    8 |   2 |
| FALSE |    8 |   5 |
| FALSE |    8 |   8 |
| TRUE  |    6 |   6 |
| TRUE  |    6 |   3 |
| TRUE  |    6 |   9 |
| TRUE  |    3 |   6 |
| TRUE  |    3 |   3 |
| TRUE  |    3 |   9 |
| TRUE  |    9 |   6 |
| TRUE  |    9 |   3 |
| TRUE  |    9 |   9 |
| NA    |    1 |   1 |
| NA    |    1 |   7 |
| NA    |    1 |   4 |
| NA    |    7 |   1 |
| NA    |    7 |   7 |
| NA    |    7 |   4 |
| NA    |    4 |   1 |
| NA    |    4 |   7 |
| NA    |    4 |   4 |

For the factor version, the logical values are coerced to factors, but
they don’t match any of the given levels, so they all become `NA`:

``` r
knitr::kable(merge(
  df_badmerge_factor[, c("a", "row")],
  df_badmerge_logical[, c("a", "row2")]
))
```

| a   | row | row2 |
|:----|----:|-----:|
| NA  |   7 |    7 |
| NA  |   7 |    4 |
| NA  |   7 |    1 |
| NA  |   4 |    7 |
| NA  |   4 |    4 |
| NA  |   4 |    1 |
| NA  |   1 |    7 |
| NA  |   1 |    4 |
| NA  |   1 |    1 |

``` r
knitr::kable(merge(
  df_badmerge_logical[, c("a", "row2")],
  df_badmerge_factor[, c("a", "row")]
))
```

| a   | row2 | row |
|:----|-----:|----:|
| NA  |    7 |   7 |
| NA  |    7 |   4 |
| NA  |    7 |   1 |
| NA  |    4 |   7 |
| NA  |    4 |   4 |
| NA  |    4 |   1 |
| NA  |    1 |   7 |
| NA  |    1 |   4 |
| NA  |    1 |   1 |

However, we see unexpected behaviour with the factor version, when also
merging on another attribute, `b`: the merge result now depends on the
input order. With the factor version first, the result is similar to
before:

``` r
knitr::kable(merge(
  df_badmerge_factor,
  df_badmerge_logical
))
#> Warning in `[<-.factor`(`*tmp*`, ri, value = c(NA, FALSE, TRUE, NA, FALSE, :
#> invalid factor level, NA generated
```

| a   | b     | row | row2 |
|:----|:------|----:|-----:|
| NA  | FALSE |   4 |    4 |
| NA  | FALSE |   4 |    5 |
| NA  | FALSE |   4 |    6 |
| NA  | NA    |   1 |    1 |
| NA  | NA    |   1 |    2 |
| NA  | NA    |   1 |    3 |
| NA  | TRUE  |   7 |    7 |
| NA  | TRUE  |   7 |    8 |
| NA  | TRUE  |   7 |    9 |

With the logical version first, however, only the logical `a` values
that are `NA` before coercion are kept, rather than all of them:

``` r
knitr::kable(merge(
  df_badmerge_logical,
  df_badmerge_factor
))
```

| a   | b     | row2 | row |
|:----|:------|-----:|----:|
| NA  | FALSE |    4 |   4 |
| NA  | NA    |    1 |   1 |
| NA  | TRUE  |    7 |   7 |

As said above, letting an attribute’s class vary across relations should
be done with caution.

## Synthesis doesn’t minimise relation key count

Bernstein’s synthesis is guaranteed to minimise the number of relations
created for a given set of functional dependencies, and removing
avoidable attributes can reduce the number of attributes in those
relations. However, there can still be redundant keys. For example, we
can take the following set of functional dependencies:

``` r
fds_redkey <- functional_dependency(
  list(
    list("a", "b"),
    list("d", "c"),
    list(c("b", "d"), "a"),
    list("a", "c"),
    list(c("b", "c"), "d")
  ),
  letters[1:4]
)
fds_redkey
#> 5 functional dependencies
#> 4 attributes: a, b, c, d
#>    a -> b
#>    d -> c
#> b, d -> a
#>    a -> c
#> b, c -> d
```

Normalising gives the following relations:

``` r
normalise(fds_redkey, remove_avoidable = TRUE)
#> database schema with 2 relation schemas
#> 4 attributes: a, b, c, d
#> schema a: a, b, c, d
#>   key 1: a
#>   key 2: b, c
#>   key 3: b, d
#> schema d: d, c
#>   key 1: d
#> references:
#> a.{d} -> d.{d}
```

``` r
show(normalise(fds_redkey, remove_avoidable = TRUE))
```

These relations have some redundancy: relation `a` implies
`{b, d} -> c`, but relation `d` implies that `{d} -> c`. This isn’t
resolved by removing avoidable attributes, because `d` still needs to be
in relation `a`: we just need to remove `{b, d}` as a key. However, this
is resolved if we instead use this set of functional dependencies, which
is equivalent to the previous set:

``` r
fds_redkey_fix <- functional_dependency(
  list(
    list("a", "b"),
    list("d", "c"),
    list(c("b", "c"), "a"),
    list("a", "d")
  ),
  letters[1:4]
)
fds_redkey_fix
#> 4 functional dependencies
#> 4 attributes: a, b, c, d
#>    a -> b
#>    d -> c
#> b, c -> a
#>    a -> d
schema_redkey_fix <- normalise(fds_redkey_fix, remove_avoidable = TRUE)
```

``` r
show(schema_redkey_fix)
```

There’s no way in `autodb` to find better sets like this.

## Normal forms are’t all there is to database design

Normal forms only refer to one relation at a time: referring to a
database as being in a given normal form just means that each of its
relations are that normal form individually. This independence means
that it’s easy to make database schemas that are technically
well-normalised, but have obvious issues.

For example, take this database schema, whose relation schemas are in
third normal form:

``` r
dup_db <- autodb(ChickWeight)
```

``` r
show(dup_db)
```

If we now create copies of these relations, with the intention that
copies always contain the same data, then all relations are still in
third normal form, and so we’d say this database is also in third normal
form:

``` r
show(dup_db[c(1, 1, 2, 2, 2)])
```

However, no one would claim that this is a good database design, since
there is clearly a large amount of data redundancy. Higher normal forms
would not change this. In fact, since I’ve implemented
subset-based-copying with the copies not referencing each other, it is
trivial to insert data to make this database incoherent. For example,
`Chick` and `Chick.1` could contain different diet assignments.
