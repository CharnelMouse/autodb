# Handling missing values

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

## Missing values

A functional dependency $\left. X\rightarrow y \right.$ is satisfied if,
for any two records whose values in `X` are equal, their values in `y`
are also equal. The result of the equality comparisons must be true or
false, otherwise it’s unclear how to interpret them.

This forbids the use of missing values (`NA` or `NaN`), which is a
problem when so much data handled in R has them.

`autodb` uses what, I believe, is the standard fix: instead of searching
for functional dependencies, it searches for a weaker variant, called a
*literal* functional dependency (LFD), which treats missing values as
equal to each other, and not equal to anything else.

LFDs are more generic than standard FDs: since practically every class
takes the identity operator, they make weak assumptions about the
attribute classes present in a data set, and we can use them on just
about anything.

There are other FD variants that handle missing values, but LFDs are
noteworthy for still satisfying Armstrong’s axioms. For example, they
still respect transitivity: if $\left. X\rightarrow Y \right.$ and
$\left. Y\rightarrow Z \right.$ literally, then
$\left. X\rightarrow Z \right.$ literally. This allows us to construct a
database schema with Bernstein synthesis, using LFDs instead of FDs, and
get something coherent.

## Decomposing to remove missing values

However, ignoring the special status of missing values in this way
ignores important structural information. Ideally, we want to avoid
missing values, because they raise awkward questions about how to handle
missing comparison results when filtering or joining relations.

For example, take the following data frame:

``` r
df_nas <- data.frame(
  patient = c(1L, 2L, 3L, 4L),
  trial_entry_date = as.Date(c("2022/05/02", "2022/06/06", "2022/04/01", "2022/03/19")),
  trial_exit_date = as.Date(c(NA, NA, "2022/10/07", NA))
)
knitr::kable(df_nas)
```

| patient | trial_entry_date | trial_exit_date |
|--------:|:-----------------|:----------------|
|       1 | 2022-05-02       | NA              |
|       2 | 2022-06-06       | NA              |
|       3 | 2022-04-01       | 2022-10-07      |
|       4 | 2022-03-19       | NA              |

`autodb` currently treats `NA` as just another value, so the data is all
kept together in the database schema:

``` r
show(autodb(df_nas))
```

In this case, a missing exit date represents no exit date: the patient
is still in the trial. To make this difference explicit – and enforced –
we move exit information to a separate relation, containing only
patients with a exit date. This removes the need for missing values.
This decomposition isn’t done by `autodb` itself, but we can do it
manually:

``` r
ds_trial <- database_schema(
  relation_schema(
    list(
      patient = list(c("patient", "trial_entry_date"), list("patient")),
      patient_exit = list(c("patient", "trial_exit_date"), list("patient"))
    ),
    names(df_nas)
  ),
  list(list("patient_exit", "patient", "patient", "patient"))
)

# approach 1: decompose, then remove
ideal_db <- decompose(df_nas, ds_trial)
records(ideal_db)$patient_exit <- subset(
  records(ideal_db)$patient_exit,
  !is.na(trial_exit_date)
)

# approach 2: create and insert
ideal_db2 <- create(ds_trial) |>
  insert(df_nas, relations = "patient") |>
  insert(subset(df_nas, !is.na(trial_exit_date)), relations = "patient_exit")

stopifnot(identical(ideal_db2, ideal_db))
```

``` r
show(ideal_db)
```

## Structure conditional on value presence

The above case has simple conditions under which values can be missing,
but missing values introduce other complications. For example, whether
an attribute is missing can depend on whether other attributes are
missing:

- Sets of attributes are missing or non-missing together.
- Two sets of attributes are mutually exclusive: exactly one is
  non-missing. These sets might not be disjoint. This is common when
  different records represent different types of entity, and so use
  different attributes. Ideally, these would be stored in separate
  relations instead, but they often aren’t.

The data below records knowledge about values. This can either be known
single values, or an interval with an associated distribution, which may
have some distribution parameters. This format is common when listing
model parameters.

``` r
df_options <- data.frame(
  id = 1:20,
  value = c(2.3, 2.3, 5.7, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_, NA_real_),
  lower_bound = c(NA_real_, NA_real_, NA_real_, 2.4, 0, 1, 0, 5.6, 2.4, 5.3, 5.3, 2.4, 2.4, 2.4, 2.4, 2.4, 2.4, 2.4, 5.6, 2.4),
  upper_bound = c(NA_real_, NA_real_, NA_real_, 7.1, 10, 10, 13.1, 25.8, 10, 13.1, 10, 25.8, 25.8, 25.8, 25.8,13.1, 13.1, 25.8, 25.8, 25.8),
  interval_distribution = factor(c(NA, NA, NA, "uniform", "uniform", "uniform", "uniform", "uniform", "Beta", "Beta", "Beta", "Beta", "Kumaraswamy", "Kumaraswamy", "Kumaraswamy", "Kumaraswamy", "PERT", "PERT", "PERT", "PERT")),
  param1 = c(NA, NA, NA, NA, NA, NA, NA, NA, 1, 1, 1, 2, 2, 2.1, 2, 2, 2, 1, 2, 2),
  param2 = c(NA, NA, NA, NA, NA, NA, NA, NA, 1, 2, 2, 2, 2, 1, 1, 1, NA, NA, NA, NA)
)
knitr::kable(df_options)
```

|  id | value | lower_bound | upper_bound | interval_distribution | param1 | param2 |
|----:|------:|------------:|------------:|:----------------------|-------:|-------:|
|   1 |   2.3 |          NA |          NA | NA                    |     NA |     NA |
|   2 |   2.3 |          NA |          NA | NA                    |     NA |     NA |
|   3 |   5.7 |          NA |          NA | NA                    |     NA |     NA |
|   4 |    NA |         2.4 |         7.1 | uniform               |     NA |     NA |
|   5 |    NA |         0.0 |        10.0 | uniform               |     NA |     NA |
|   6 |    NA |         1.0 |        10.0 | uniform               |     NA |     NA |
|   7 |    NA |         0.0 |        13.1 | uniform               |     NA |     NA |
|   8 |    NA |         5.6 |        25.8 | uniform               |     NA |     NA |
|   9 |    NA |         2.4 |        10.0 | Beta                  |    1.0 |      1 |
|  10 |    NA |         5.3 |        13.1 | Beta                  |    1.0 |      2 |
|  11 |    NA |         5.3 |        10.0 | Beta                  |    1.0 |      2 |
|  12 |    NA |         2.4 |        25.8 | Beta                  |    2.0 |      2 |
|  13 |    NA |         2.4 |        25.8 | Kumaraswamy           |    2.0 |      2 |
|  14 |    NA |         2.4 |        25.8 | Kumaraswamy           |    2.1 |      1 |
|  15 |    NA |         2.4 |        25.8 | Kumaraswamy           |    2.0 |      1 |
|  16 |    NA |         2.4 |        13.1 | Kumaraswamy           |    2.0 |      1 |
|  17 |    NA |         2.4 |        13.1 | PERT                  |    2.0 |     NA |
|  18 |    NA |         2.4 |        25.8 | PERT                  |    1.0 |     NA |
|  19 |    NA |         5.6 |        25.8 | PERT                  |    2.0 |     NA |
|  20 |    NA |         2.4 |        25.8 | PERT                  |    2.0 |     NA |

Since `autodb` doesn’t treat missing values as a special case, it can
not detect these relationships, so the resulting schema ignores them:

``` r
db_options <- autodb(df_options)
```

``` r
show(db_options)
```

However, from looking at the data ourselves, we can see a
clearly-implied structure:

- Records either have a perfectly-known value, or a distribution of what
  the value could be. These two options have separate attributes.
- Distribution information contains a lower and upper bound, and a
  distribution type. Different distributions have different numbers of
  distribution parameters, which are given in shared parameter
  attributes.
- Parameter attributes are ordered: there can only be a second parameter
  if there’s a first parameter.

As a general hack for finding these sorts of relationships, I like
taking each attribute with missing values, and adding a presence
indicator attribute, that states whether its value is present or
missing:

``` r
df_options_presence <- df_options[vapply(df_options, anyNA, logical(1))]
df_options_presence[] <- lapply(df_options_presence, Negate(is.na))
names(df_options_presence) <- paste0(names(df_options_presence), "_present")
df_options_with_presence <- cbind(df_options, df_options_presence)
```

This is not always practical, because adding columns rapidly increases
the search time for the dependency search. In this case, search time is
not an issue, and it makes the structure in the database schema more
apparent:

``` r
db_options_with_presence <- autodb(df_options_with_presence)
```

``` r
show(db_options_with_presence)
```

Some of the new information is trivially true: attributes always
determine their own presence. Of more interest is the `value_present`
relation, which shows that the values, bounds, and interval
distributions inform the presence of each other:

``` r
knitr::kable(records(db_options_with_presence)$value_present)
```

|     | value_present | lower_bound_present | upper_bound_present | interval_distribution_present |
|:----|:--------------|:--------------------|:--------------------|:------------------------------|
| 1   | TRUE          | FALSE               | FALSE               | FALSE                         |
| 4   | FALSE         | TRUE                | TRUE                | TRUE                          |

In the `interval_distribution` relation, we can also see how the
interval distribution determines how many parameters are required:

``` r
knitr::kable(records(db_options_with_presence)$interval_distribution)
```

|     | interval_distribution | value_present | param1_present | param2_present |
|:----|:----------------------|:--------------|:---------------|:---------------|
| 1   | NA                    | TRUE          | FALSE          | FALSE          |
| 4   | uniform               | FALSE         | FALSE          | FALSE          |
| 9   | Beta                  | FALSE         | TRUE           | TRUE           |
| 13  | Kumaraswamy           | FALSE         | TRUE           | TRUE           |
| 17  | PERT                  | FALSE         | TRUE           | FALSE          |

We can use this enhanced database to decide how to manually split up the
actual database.
