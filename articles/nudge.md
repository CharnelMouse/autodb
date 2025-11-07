# A larger example: the nudge dataset

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

Included in the package is a 447-by-25 data frame called `nudge`:

``` r
knitr::kable(data.frame(
  attribute = names(nudge),
  class = vapply(nudge, \(x) class(x)[[1]], character(1)),
  NAs = vapply(nudge, \(x) sum(is.na(x)), integer(1)),
  row.names = NULL
))
```

| attribute              | class     | NAs |
|:-----------------------|:----------|----:|
| publication_id         | integer   |   0 |
| study_id               | integer   |   0 |
| es_id                  | integer   |   0 |
| reference              | character |   0 |
| title                  | character |   0 |
| year                   | integer   |   0 |
| location               | factor    |   0 |
| domain                 | factor    |   0 |
| intervention_category  | factor    |   0 |
| intervention_technique | factor    |   0 |
| type_experiment        | factor    |   0 |
| population             | factor    |   0 |
| n_study                | integer   |   0 |
| n_comparison           | integer   |   0 |
| n_control              | integer   |   0 |
| n_intervention         | integer   |   0 |
| binary_outcome         | logical   |   0 |
| mean_control           | numeric   |  13 |
| sd_control             | numeric   | 286 |
| mean_intervention      | numeric   |  13 |
| sd_intervention        | numeric   | 286 |
| cohens_d               | numeric   |   0 |
| variance_d             | numeric   |   0 |
| approximation          | logical   |   0 |
| wansink                | logical   |   0 |

This is the data set for a meta-analysis, looking at the effectiveness
of “nudge” interventions. Like any meta-analysis, the data is strictly
hierarchical: publications contain studies, which contain effect size
measurements. We expect to see this hierarchy in the normalisation.

## Initial decomposition

The full dependency information for this data is rather large. This
results in a relatively large search time for a data set of this size,
but it still only takes a few seconds.

``` r
nudge_deps_big <- discover(nudge)
nudge_schema_big <- normalise(nudge_deps_big, remove_avoidable = TRUE)
nudge_db_big <- decompose(nudge, nudge_schema_big)
```

However, the resulting schema is hard to make anything out of, with more
relations than we’d want to go through manually:

``` r
length(nudge_schema_big)
#> [1] 473
```

There are a few reasons for this, but the main one is the sheer number
of functional dependencies discovered:

``` r
length(nudge_deps_big)
#> [1] 3732
```

Even if we remove the transitive dependencies, which are implied by the
others, we still have 597 dependencies.

## Simplifying the search result

This is a rather large set for the simple approach to simplifying a set
of FDs: manually looking for spurious cases to remove before
normalisation.

As a hack, we can use `reduce` on the database. This finds the relations
with the most records – i.e. the relations with one record for each
unique row in the original data – and returns a reduced database,
containing only those relations, and relations that they reference,
either directly or indirectly.

``` r
nudge_reduced_big <- reduce(nudge_db_big)
```

The relations removed by `reduce` are often spurious, but not always:
see the reference chains section of the [Planned
improvements](https://charnelmouse.github.io/autodb/articles/plans.md)
vignette
([`vignette("plans", package = "autodb")`](https://charnelmouse.github.io/autodb/articles/plans.md))
for an example of non-spurious tables not being connected. However,
`reduce` at least gives us something smaller to look at.

In this case, the reduced database is still too large to easily review:

``` r
length(nudge_reduced_big)
#> [1] 124
```

Instead, we get a grip on what’s going on by collecting statistics on
attributes involved in the determinants.

For example, we can also see how large the determinants are:

``` r
table(lengths(detset(nudge_deps_big)))
#> 
#>   1   2   3   4   5   6   7 
#>  41 545 780 993 926 435  12
```

While there are some large true determinants in real data, a common rule
of thumb is that FDs with larger determinants are more likely to be
spurious. This doesn’t make for an obvious way to filter the FDs with
some hard size limit, though.

We can also see how often each attribute appears in a determinant:

``` r
sort(table(unlist(detset(nudge_deps_big))), decreasing = TRUE)
#> 
#>           n_comparison        type_experiment  intervention_category 
#>                   1095                   1085                   1061 
#>              n_control                 domain         n_intervention 
#>                   1029                   1016                    993 
#>                   year          approximation intervention_technique 
#>                    889                    766                    745 
#>               location             variance_d        sd_intervention 
#>                    739                    727                    687 
#>             sd_control                n_study               cohens_d 
#>                    684                    628                    549 
#>         binary_outcome           mean_control      mean_intervention 
#>                    503                    370                    313 
#>             population                wansink         publication_id 
#>                    269                    248                    119 
#>              reference                  title               study_id 
#>                    108                     83                     37 
#>                  es_id 
#>                     24
```

We can be more specific, and see how often each attributes appears in a
determinant of a given size:

``` r
sort_by_rowSums <- function(x, ...) x[order(rowSums(x), ...), , drop = FALSE]
level_table <- function(x, levels) table(factor(x, levels))
attrs_table <- function(x) level_table(unlist(x), names(nudge))
by_lengths <- function(x, f) do.call(cbind, tapply(x, lengths(x), f))

by_lengths(detset(nudge_deps_big), attrs_table) |>
  sort_by_rowSums(decreasing = TRUE)
#>                         1   2   3   4   5   6  7
#> n_comparison            0  73  64 342 395 217  4
#> type_experiment         0   6 114 286 376 295  8
#> intervention_category   0   2 140 185 389 341  4
#> n_control               0  53 148 305 361 162  0
#> domain                  0  23 109 335 453  84 12
#> n_intervention          0  64 142 283 290 206  8
#> year                    0  11 266 408 192  12  0
#> approximation           0   0  35 125 317 277 12
#> intervention_technique  1  25 124 309 249  37  0
#> location                0   4  91 191 259 182 12
#> variance_d              0  76 201 241 164  45  0
#> sd_intervention         0  15  65 228 227 148  4
#> sd_control              0  12  65 228 227 148  4
#> n_study                 0  60  81 224 182  77  4
#> cohens_d                0 211 258  23  46  11  0
#> binary_outcome          0   3  20  74 225 177  4
#> mean_control            1  89 196  59  25   0  0
#> mean_intervention       1 103 141  46  22   0  0
#> population              0   0   7  38 112 104  8
#> wansink                 0   0   0  42 119  87  0
#> publication_id          0  97  22   0   0   0  0
#> reference               1  85  22   0   0   0  0
#> title                   3  58  22   0   0   0  0
#> study_id               10  20   7   0   0   0  0
#> es_id                  24   0   0   0   0   0  0
```

Either way, we can see that there are attributes that we wouldn’t expect
to be in a determinant, but often are, especially in larger ones. We
don’t expect measurement attributes like `variance_d` and `sd_control`
to co-determine anything else, but they often do.

More generally, there are certain data classes we don’t expect to see
appear as determinants. For example, numerics (floats) are probably
measurements, and shouldn’t co-determine anything else. However, if we
count determinant appearances by the attribute’s class, we find that
they often appear in determinants:

``` r
attr_classes <- vapply(nudge, \(x) class(x)[[1]], character(1))
class_table <- function(x) {
  level_table(attr_classes[unlist(x)], sort(unique(attr_classes)))
}

by_lengths(detset(nudge_deps_big), class_table) |>
  sort_by_rowSums(decreasing = TRUE)
#>            1   2   3    4    5    6  7
#> factor     1  60 585 1344 1838 1043 44
#> integer   34 378 730 1562 1420  674 16
#> numeric    2 506 926  825  711  352  8
#> logical    0   3  55  241  661  541 16
#> character  4 143  44    0    0    0  0
```

This suggests that a simple first step is to remove any FD with a float
in the determinant. We can write this as a filter vector:

``` r
det_nofloat <- vapply(
  detset(nudge_deps_big),
  \(x) all(attr_classes[x] != "numeric"),
  logical(1)
)
summary(det_nofloat)
#>    Mode   FALSE    TRUE 
#> logical    2784     948
```

This removes a lot!

If we use the filtered set of FDs, we still get a large schema:

``` r
length(normalise(nudge_deps_big[det_nofloat]))
#> [1] 176
```

However, if we reduce the resulting database, we get something much more
manageable:

``` r
nudge_schema_filtered <- normalise(nudge_deps_big[det_nofloat])
nudge_db_filtered <- reduce(decompose(nudge, nudge_schema_filtered))
```

``` r
show(nudge_db_filtered)
```

Let’s look at the resulting relations, in two sets. Here’s the left-hand
set:

``` r
subsample <- c(
  "es_id",
  "study_id",
  "intervention_technique_n_comparison_n_control",
  "intervention_technique",
  "n_comparison_n_control"
)
```

``` r
show(nudge_db_filtered[subsample])
```

We see two relations, `es_id` and `study_id`, that clearly represent the
effect and study levels of the data hierarchy, which is encouraging.
They both contain the ID for the next level, too, as they should.

The relation that the effect relation references, with the long name,
looks rather arbitrary, but the relations it references look useful. One
shows that the intervention technique determines the intervention
category, i.e. techniques are subcategories.

Less simply, the other relation contains the three effect sample sizes:
comparison size, intervention arm size, and control arm size. Each pair
of these sizes determines the other, which is what we’d see if the
comparison size is the sum of the other two. However, we can’t assume
that this is the nature of the relationship. Indeed, we can find a case
where it’s false:

``` r
knitr::kable(
  subset(
    nudge,
    n_comparison != n_control + n_intervention,
    c(reference, study_id, es_id, n_study, n_comparison, n_control, n_intervention)
  ),
  row.names = FALSE
)
```

| reference                | study_id | es_id | n_study | n_comparison | n_control | n_intervention |
|:-------------------------|---------:|------:|--------:|-------------:|----------:|---------------:|
| Hedlin & Sunstein (2016) |      137 |   180 |    1037 |         1037 |       345 |            346 |
| Hedlin & Sunstein (2016) |      137 |   181 |    1037 |         1037 |       345 |            346 |

It turns out to be difficult to identify which results in the relevant
paper these are referring to, since the presentation of the results is
rather obfuscated. However, it is clear that these are referring to a
part of the study with three arms instead of two (actually nine arms
aggregated to groups of three). If we account for having two treatment
arms, then the arm sizes do sum to the comparison size, and there is
still a sum relationship of sorts, just not the simpler one we might
expect.

In any case, most of these relations in this set seem reasonable. The
same can’t be said of the other set, which should represent the
publication-level data:

``` r
show(nudge_db_filtered[setdiff(names(nudge_db_filtered), subsample)])
```

There is a lot going on here, so let’s look at the last two:

``` r
show(nudge_db_filtered[c("title", "reference")])
```

This is our publication-level data, but something is wrong. The
publication ID is not a simple key, as we’d expect. Neither is the
reference, which we’d expect to be unique, otherwise the bibliography
will have duplicate entries. Instead, the only simple key is the
publication’s title. To use the ID or the reference as an identifier for
publications, we need to use both together as a compound key.

Without having looked at the data itself, these keys tell us that there
are duplicate publication IDs and duplicate references. Since the keys
for the `title` relation showed us the problem, we can search that
relation to find the duplicates, with all the attributes needed for
context:

``` r
duplicates <- function(x) unique(x[duplicated(x)])
subset_duplicates <- function(x, attr) {
  x[x[[attr]] %in% duplicates(x[[attr]]), , drop = FALSE]
}
```

``` r
nudge_title_relation <- records(nudge_db_filtered)$title
knitr::kable(subset_duplicates(nudge_title_relation, "publication_id"))
```

|     | title                                                                      | publication_id | reference            | year |
|:----|:---------------------------------------------------------------------------|---------------:|:---------------------|-----:|
| 44  | Enhanced active choice: A new method to motivate behavior change           |             95 | Keller et al. (2011) | 2011 |
| 130 | Nudging product choices: The effect of position change on snack bar choice |             95 | Keller et al. (2015) | 2015 |

``` r
knitr::kable(subset_duplicates(nudge_title_relation, "reference"))
```

|     | title                                                                                          | publication_id | reference   | year |
|:----|:-----------------------------------------------------------------------------------------------|---------------:|:------------|-----:|
| 214 | Nudge vs superbugs: A behavioural economics trial to reduce the overprescribing of antibiotics |             18 | BETA (2018) | 2018 |
| 399 | Energy labels that make cents                                                                  |             19 | BETA (2018) | 2018 |

The publications with the same ID have the same first author in their
references; this looks like a simple data entry error.

BETA is the Behavioural Economics Team of the Australian Government, so
it’s not surprising that they’d have multiple publications/reports per
year. However the references for publications were generated, the
possibility of a group publishing multiple relevant papers in a year
wasn’t accounted for.

Looking at the keys for the other relations, we see that they often
include `publication_id` or `reference`, and the relationships they
describe seem rather questionable. These are not worth going into in
detail, since we can guess that they stem from the duplication problems.

In a real project, this would be a good point to go back to the data
provider with questions, since we now have examples from the data of
surprising behaviour.

## Fixing the data

There are two ideal results when we find surprising behaviour:

- The behaviour is as it should be, and we can correct our understanding
  of the data;
- The behaviour is unexpected, and we can fix the data to get rid of it.

In this example, `publication_id` and `reference` are synthetic
variables, so we can easily fix the data ourselves:

``` r
nudge_fixed <- within(nudge, {
  publication_id[publication_id == 95 & year == 2015] <- max(publication_id) + 1L
  reference[publication_id == 19] <- "BETA (2018a)"
})
```

We can then re-run `autodb` to find the new schema. Note that we use the
`exclude_class` argument to exclude FDs with floats in their
determinants, instead of removing them ourselves. This prunes them from
the search space, speeding up the search.

``` r
db_fixed <- autodb(
  nudge_fixed,
  exclude_class = "numeric"
)
length(db_fixed)
#> [1] 170
```

``` r
show(reduce(db_fixed))
```

This is an improvement: the publications now have an ID and a reference
as simple keys, as they should, and some of the questionable relations
have disappeared.

## Simplifying further with search filters

Some of the relations are still questionable. If we examine them, we see
that they all have large keys, containing three attributes or more. To
simplify things, we can decide to remove them. Rather than manually
filter the FDs again, we can do this using another filtering argument
for `discover`/`autodb`, `detset_limit`:

``` r
db_final <- autodb(
  nudge_fixed,
  exclude_class = "numeric",
  detset_limit = 2
)
length(db_final)
#> [1] 11
```

The database is now small enough to not need to reduce it:

``` r
show(db_final)
```

However, the relations not referred to by the effect relation look
spurious too, so we can reduce the database to remove them too:

``` r
show(reduce(db_final))
```

This is a great improvement. We are left with only two relations that we
don’t expect in general, but hold for this dataset in particular:

- `publication_id_type_experiment` states that studies from the same
  publication, that use the same type of experiment, also take place in
  the same location. This is probably not interesting, since the
  “location” is just a logical value for whether the study was conducted
  in the USA.
- `publication_id_n_study` states that studies from the same
  publication, and with the same sample size, have the same location,
  domain, and target population. This is likely to be a coincidence, and
  not of much interest either.

Dataset-particular relations like this are useful to check, since they
often indicate sampling limitations, which can make our planned
statistical analysis infeasible.

We can remove either of these relations easily using `exclude`, since
their keys contain attributes that don’t appear in keys elsewhere. If we
do, we get a schema that looks reasonable for data of this sort in
general, apart from the mentioned issue with the effect sample sizes:

``` r
show(reduce(autodb(
  nudge_fixed,
  exclude = c("type_experiment", "n_study"),
  exclude_class = "numeric",
  detset_limit = 2
)))
```

## How not to remove spurious structure

Instead of adding search filters, or removing functional dependencies,
we could try to simplify the original schema by removing questionable
relations. This section is about why this is a bad idea.

This is the database we had:

``` r
show(nudge_db_filtered)
```

What happens if we remove the offending relations from the schema?

``` r
nudge_schema_relfiltered <- nudge_schema_filtered[
  vapply(
    keys(nudge_schema_filtered),
    \(ks) all(lengths(ks) <= 2) &&
      sum(c("publication_id", "reference") %in% ks[[1]]) != 1,
    logical(1)
  )
]
```

``` r
show(nudge_schema_relfiltered)
```

The offending relations are removed, but so are any foreign key
references involving them. The resulting schema isn’t invalid, but it’s
less connected than before.

Calling `autoref` to re-generate the foreign key references doesn’t
entirely fix the problem, since, for example, no other relation contains
a key for the `title` relation:

``` r
show(autoref(nudge_schema_relfiltered))
```

This partly happens because removing relations removes more information
than it needs to. If we remove the relevant functional dependencies
instead, then we can have new intermediate relations appear in the new
schema, whose dependencies were originally redundant by transitivity.
Removing relations directly can’t account for this.

## Alternative approach: hierarchical limits

An alternative approach is to make use of our knowledge that the data is
hierarchical, and remove dependencies where an attribute is
co-determining an attribute on a lower level. For example,
publication-level attributes shouldn’t co-determine study- or
measurement-level attributes. This means we don’t find cases where they
do, but we could decide that any such cases are spurious, and we don’t
want to spend time on them.

``` r
hlev <- c(
  publication_id = 3,
  study_id = 2,
  es_id = 1,
  reference = 3,
  title = 3,
  year = 3,
  location = 2,
  domain = 2,
  intervention_category = 2,
  intervention_technique = 2,
  type_experiment = 2,
  population = 2,
  n_study = 2,
  n_comparison = 1,
  n_control = 1,
  n_intervention = 1,
  binary_outcome = 1,
  mean_control = 1,
  sd_control = 1,
  mean_intervention = 1,
  sd_intervention = 1,
  cohens_d = 1,
  variance_d = 1,
  approximation = 1,
  wansink = 2
)
hfilter <- function(fds, hlev) {
  fds[mapply(
    \(det, dep) all(hlev[det] <= hlev[[dep]]),
    detset(fds),
    dependant(fds)
  )]
}
```

Filtered result for the original data:

``` r
nudge |>
  discover(exclude_class = "numeric", detset_limit = 2) |>
  hfilter(hlev) |>
  normalise(remove_avoidable = TRUE) |>
  decompose(df = nudge) |>
  reduce() |>
  show()
```

Filtered result for the fixed data:

``` r
nudge_fixed |>
  discover(exclude_class = "numeric", detset_limit = 2) |>
  hfilter(hlev) |>
  normalise(remove_avoidable = TRUE) |>
  decompose(df = nudge_fixed) |>
  show()
```
