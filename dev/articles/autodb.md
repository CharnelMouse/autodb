# Using autodb

``` r
library(autodb)
```

    ## 
    ## Attaching package: 'autodb'

    ## The following object is masked from 'package:stats':
    ## 
    ##     decompose

## Terminology

To avoid confusion, this vignette is consistent with terminology: a
*data frame* (table) has *records* (rows) with the same *attributes*
(columns/variables), and we split it up into a *database*, that consists
of *relations* (data frames with key and foreign key constraints between
them). This is in line with terms used in the relational model.

When we talk about the type of values an attribute can take, we talk
about a data *class*, as in the relational model and R, rather than a
value type, as in many other programming languages.

## Motivation

Let’s expand on the package DESCRIPTION:

> Automatic normalisation of a data frame to third normal form, with the
> intention of easing the process of data cleaning. (Usage to design
> your actual database for you is not advised.)

### Database normalisation

Database normalisation is, roughly, the idea of taking a dataset and
splitting it up into several linked tables. This is useful, because it
makes the structure of the data more explicit, and enforceable.

As an example, let’s look at the `ChickWeight` dataset, included with
base R.

``` r
summary(ChickWeight)
```

    ##      weight           Time           Chick     Diet   
    ##  Min.   : 35.0   Min.   : 0.00   13     : 12   1:220  
    ##  1st Qu.: 63.0   1st Qu.: 4.00   9      : 12   2:120  
    ##  Median :103.0   Median :10.00   20     : 12   3:120  
    ##  Mean   :121.8   Mean   :10.72   10     : 12   4:118  
    ##  3rd Qu.:163.8   3rd Qu.:16.00   17     : 12          
    ##  Max.   :373.0   Max.   :21.00   19     : 12          
    ##                                  (Other):506

This is a simple dataset, but the flat table format obscures some
information about the structure of the data. Specifically, chicks have
their diet assigned at the beginning of the study, and it’s never
changed. To make this more explicit, we could split the data into two
separate tables:

``` r
measurement <- unique(subset(ChickWeight, , -Diet))
chick <- unique(subset(ChickWeight, , c(Chick, Diet)))
summary(measurement)
```

    ##      weight           Time           Chick    
    ##  Min.   : 35.0   Min.   : 0.00   13     : 12  
    ##  1st Qu.: 63.0   1st Qu.: 4.00   9      : 12  
    ##  Median :103.0   Median :10.00   20     : 12  
    ##  Mean   :121.8   Mean   :10.72   10     : 12  
    ##  3rd Qu.:163.8   3rd Qu.:16.00   17     : 12  
    ##  Max.   :373.0   Max.   :21.00   19     : 12  
    ##                                  (Other):506

``` r
summary(chick)
```

    ##      Chick    Diet  
    ##  18     : 1   1:20  
    ##  16     : 1   2:10  
    ##  15     : 1   3:10  
    ##  13     : 1   4:10  
    ##  9      : 1         
    ##  20     : 1         
    ##  (Other):44

``` r
stopifnot(
  identical(
    merge(measurement, chick, sort = FALSE)[names(ChickWeight)],
    data.frame(ChickWeight)
  ),
  setequal(measurement$Chick, chick$Chick)
)
```

We can also add the restriction that each chick can only appear in the
chick table once, and that each chick-time pair can only appear in the
measurement table once.

For data management, this is good because it prevents some common types
of data error:

- Consistency errors. In the flat table, we can accidentally add a row
  where a chick is on a different diet to before. In the split tables,
  the above restrictions mean that this is impossible.
- Insertion/deletion errors. Suppose we have a chick that’s been
  assigned a diet, but hasn’t been measured yet. In the flat table, this
  information can’t be added, unless we use a temporary row where the
  time and weight are missing, and hope we remember to remove it later.
  In the split tables, we can just add the chick to the chick table
  without issue. Conversely, if we removed all of a chick’s measurements
  in the flat table, we’d also lose the information about which diet
  it’s been assigned.
- Update errors. Suppose we notice that a chick is recorded under the
  wrong diet, and change values to correct it. In the flat table, a
  chick’s diet is recorded for every measurement, so we could only
  update some of them by mistake, resulting in inconsistency. In the
  split tables, the diet is only given once, so this can’t happen.

These all tie into the same idea: reducing data redundancy so that each
fact is only stated in one place.

### For data cleaning

All of the above points are also useful for data exploration and
validation, and that’s the emphasis of the package. Being able to make
the data’s structure more explicit is valuable for making sure that we
understand the problem domain before we try to model it. This is true
even if we still use the original flat table for the analysis itself,
since tools like R generally expect that format.

I’ve often started data exploration under the following conditions:

- Little prior knowledge of the data, since it’s provided by a third
  party.
- One-off analyses, where the data is discard at the end, so it’s not
  worth the time to get the data into a proper database.
- Limited, or unreliable, documentation.
- Data that fits in local memory, presenting less problems for
  automation.

It’s in this context that I would usually have to do the normalisation
by hand, and I would like to have a tool that does it
semi-automatically. This package is intended to be that tool, using a
minimal amount of additional libraries.

For example, we can pass `ChickWeight` into the main function, `autodb`,
and get the expected normalisation:

``` r
chick_db <- autodb(ChickWeight)
chick_db
```

    ## database with 2 relations
    ## 4 attributes: weight, Time, Chick, Diet
    ## relation Chick: Chick, Diet; 50 records
    ##   key 1: Chick
    ## relation Time_Chick: Time, Chick, weight; 578 records
    ##   key 1: Time, Chick
    ## references:
    ## Time_Chick.{Chick} -> Chick.{Chick}

We can also plot it, by using `gv` to turn the result into code for the
Graphviz language, and then calling Graphviz however we prefer.

``` r
cat(gv(chick_db))
```

    ## digraph {
    ##   rankdir = "LR"
    ##   node [shape=plaintext];
    ## 
    ##   "Chick" [label = <
    ##     <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
    ##     <TR><TD COLSPAN="3">Chick (50 records)</TD></TR>
    ##     <TR><TD PORT="TO_chick">Chick</TD><TD BGCOLOR="black"></TD><TD PORT="FROM_chick">ordered</TD></TR>
    ##     <TR><TD PORT="TO_diet">Diet</TD><TD></TD><TD PORT="FROM_diet">factor</TD></TR>
    ##     </TABLE>>];
    ##   "Time_Chick" [label = <
    ##     <TABLE BORDER="0" CELLBORDER="1" CELLSPACING="0" CELLPADDING="4">
    ##     <TR><TD COLSPAN="3">Time_Chick (578 records)</TD></TR>
    ##     <TR><TD PORT="TO_time">Time</TD><TD BGCOLOR="black"></TD><TD PORT="FROM_time">numeric</TD></TR>
    ##     <TR><TD PORT="TO_chick">Chick</TD><TD BGCOLOR="black"></TD><TD PORT="FROM_chick">ordered</TD></TR>
    ##     <TR><TD PORT="TO_weight">weight</TD><TD></TD><TD PORT="FROM_weight">numeric</TD></TR>
    ##     </TABLE>>];
    ## 
    ##   "Time_Chick":FROM_chick -> "Chick":TO_chick;
    ## }

``` r
if (requireNamespace("DiagrammeR", quietly = TRUE)) {
  show <- function(x) DiagrammeR::grViz(gv(x), width = "100%")
  maybe_plot <- function(x) DiagrammeR::grViz(gv(x), width = "100%")
}else{
  show <- print
  maybe_plot <- function(x) invisible(NULL)
}
```

``` r
show(chick_db)
```

In an interactive session, the simplest way to do this is to call
[`DiagrammeR::grViz`](https://rich-iannone.github.io/DiagrammeR/reference/grViz.html)
on the output from `gv`. In a Quarto file, we can write the output to a
file, and call that file from a `dot` code block. The advantage to the
latter approach is that DiagrammeR creates an HTML widget, and the
Quarto block does not, so we avoid needing to include about 2MB of
Javascript.

The plot represents each table/relation as a box: the top row gives the
relation name and the number of records, and the other rows list the
attributes and their classes.

In the middle is a grid containing black cells: each column of black
cells represents a *key*, or uniqueness constraint, for the relation.
Keys are irreducible subsets of a relation’s attributes, that take a
unique set of values in each record. They can be treated as a unique row
identifier. For example, each chick has a unique `Chick` value, and each
measurement has a unique `Time`-`Chick` pair.

### Not for database design

Good database design is a hard and involved process, which requires a
strong grasp on the domain that the database will hold information on.
Please don’t try to use this package to do it on autopilot. If you do,
you’ll quickly find some shortcomings with the results:

- Since the package can only work with the data it’s got, the resulting
  database schema is specific to the given dataset, rather than data of
  that type in general. This is useful for data exploration, since it
  can give useful information about sampling issues, but not for
  designing a database.
- The resulting “databases” might not be something SQL etc. will accept
  as-is. For example, `autodb` needs to handle constant attributes, that
  have the same value in every record. This is done by making a relation
  with an empty key, which is allowed in the relation model, but SQL
  won’t let you do it. In a real database, we would either not store
  constants in the database at all, or place them with the expectation
  that they won’t stay constant when more data is added.
- It doesn’t introduce artificial keys, if you like using those, or
  single-attribute lookups, which are a relational database’s equivalent
  to factor levels.
- It doesn’t create “virtual relations”, also known as “views”, which
  are data-less relations defined in terms of manipulating real
  relations (usually to create aggregates).
- If a relation has multiple keys, it doesn’t choose one as a “primary
  key”. Primary keys are implementation details for databases, not part
  of the relational model, and they’re not especially relevant for data
  exploration. However, SQL etc. will ask for one.
- It can’t account for semantic knowledge, which a person would spot
  easily.

See the
[Limitations](https://charnelmouse.github.io/autodb/dev/articles/limits.md)
vignette
([`vignette("limits", package = "autodb")`](https://charnelmouse.github.io/autodb/dev/articles/limits.md))
for more details.

### To third normal form

`autodb` gets a given data frame to third normal form (3NF): every
attribute depends on the whole key(s), and non-key attributes depend on
nothing but the key(s). This was mostly chosen because there is an
existing algorithm, Bernstein’s synthesis, for normalising to third
normal form.

`autodb` works by searching for functional dependencies in the data, and
the highest normal form attainable with functional dependencies is a
higher form called Boyes-Codd normal form (BCNF). However, normalising
to BCNF requires more complicated infrastructure, and `autodb` isn’t set
up for that yet. This may change in the future.

An additional enhancement to 3NF, called LTK form, is available as an
option: see the section on avoidable attributes for more details.

## Individual steps

### Finding functional dependencies

Having looked at the final result for `ChickWeight` first, we now look
at the individual steps. The first step is to find the (non-trivial and
minimal) dependencies present in the given data frame. There are various
ways to do this; by default, the package uses FDHitsSep, a depth-first
search algorithm. We run this by using the `discover` function, setting
`progress` to `TRUE` to see the steps taken:

``` r
deps <- discover(ChickWeight, progress = TRUE)
```

    ## formatting numerical/complex variables with 7 significant digits
    ## simplifying data types
    ## calculating single-attribute PLIs
    ## sampling difference sets
    ## 7 initial diffsets
    ## 
    ## dependant weight
    ## dependant Time
    ## dependant Chick
    ## dependant Diet
    ## 
    ## FDHitsSep complete
    ## 9 final diffsets
    ## 10 nodes visited
    ## 7 partitions cached

``` r
deps
```

    ## 2 functional dependencies
    ## 4 attributes: weight, Time, Chick, Diet
    ## Time, Chick -> weight
    ##       Chick -> Diet

The result is a list of *functional dependencies*, in the format
`determinant -> dependant`, with an attribute named `attrs_order` that
gives all the attribute names in their original order. Each of these
three parts can be extracted:

``` r
detset(deps)
```

    ## [[1]]
    ## [1] "Time"  "Chick"
    ## 
    ## [[2]]
    ## [1] "Chick"

``` r
dependant(deps)
```

    ## [1] "weight" "Diet"

``` r
attrs_order(deps)
```

    ## [1] "weight" "Time"   "Chick"  "Diet"

The former two are useful for filtering.

### Normalisation

Now that we have a list of discovered dependencies, we can construct a
database schema, where the relation schemas are normalised to third
normal form. This is done using Bernstein’s synthesis.

``` r
schema <- synthesise(deps)
schema
```

    ## 2 relation schemas
    ## 4 attributes: weight, Time, Chick, Diet
    ## schema Chick: Chick, Diet
    ##   key 1: Chick
    ## schema Time_Chick: Time, Chick, weight
    ##   key 1: Time, Chick

We can also plot this schema:

``` r
show(schema)
```

This is similar to the database plot given before, but there is some
information not present, that requires the data frame itself: record
counts, and data classes. We do have automatically-generated names for
the individual relations, which are created using the keys.

At this point, we have no references between the relation schemas, since
Bernstein’s synthesis doesn’t supply information about foreign key
references. We could use this database schema to build a database, but
we’d rather add these foreign key references first.

### Adding foreign key references

Getting back to our `ChickWeight` example, we now have a database
schema, consisting of a list of `relation` schemas. However, we have no
information about how these relation schemas are linked to each other.
In particular, we have no information about foreign keys. We can add
this information using `autoref`:

``` r
linked_schema <- autoref(schema)
linked_schema
```

    ## database schema with 2 relation schemas
    ## 4 attributes: weight, Time, Chick, Diet
    ## schema Chick: Chick, Diet
    ##   key 1: Chick
    ## schema Time_Chick: Time, Chick, weight
    ##   key 1: Time, Chick
    ## references:
    ## Time_Chick.{Chick} -> Chick.{Chick}

We could also have used `normalise`, instead of `synthesise` and
`autoref` separately:

``` r
normalise(deps)
```

    ## database schema with 2 relation schemas
    ## 4 attributes: weight, Time, Chick, Diet
    ## schema Chick: Chick, Diet
    ##   key 1: Chick
    ## schema Time_Chick: Time, Chick, weight
    ##   key 1: Time, Chick
    ## references:
    ## Time_Chick.{Chick} -> Chick.{Chick}

Plotting this updated database schema shows the same relation schemas as
before, linked together by foreign key references:

``` r
show(linked_schema)
```

### Decomposing the original relation

Finally, once we have our normalised database schema, we can apply it to
our original data frame, or a new one with the same structure. This
results in a normalised database, as we got from using `autodb`:

``` r
db <- decompose(ChickWeight, linked_schema)
```

``` r
show(db)
```

### Rejoining a database back into a data frame

We can reverse the process of turning a data frame into a database with
the `rejoin` function. This may not be identical to `ChickWeight`, since
the rows may have been rearranged. However, we can use the convenience
function `df_equiv` to check for equivalence under row reordering:

``` r
rejoined <- rejoin(db)
summary(rejoined)
```

    ##      weight           Time           Chick     Diet   
    ##  Min.   : 35.0   Min.   : 0.00   13     : 12   1:220  
    ##  1st Qu.: 63.0   1st Qu.: 4.00   9      : 12   2:120  
    ##  Median :103.0   Median :10.00   20     : 12   3:120  
    ##  Mean   :121.8   Mean   :10.72   10     : 12   4:118  
    ##  3rd Qu.:163.8   3rd Qu.:16.00   17     : 12          
    ##  Max.   :373.0   Max.   :21.00   19     : 12          
    ##                                  (Other):506

``` r
identical(rejoined, ChickWeight)
```

    ## [1] FALSE

``` r
df_equiv(rejoined, ChickWeight)
```

    ## [1] TRUE

### Tuning detection and normalisation

Let’s look at a different dataset for a moment, to look at some cases
where we don’t want to use the dependencies as given. We’ll use the
Titanic data set, also provided with base R. This data is in array form,
so we first convert it to data frame form:

``` r
knitr::kable(as.data.frame(Titanic))
```

| Class | Sex    | Age   | Survived | Freq |
|:------|:-------|:------|:---------|-----:|
| 1st   | Male   | Child | No       |    0 |
| 2nd   | Male   | Child | No       |    0 |
| 3rd   | Male   | Child | No       |   35 |
| Crew  | Male   | Child | No       |    0 |
| 1st   | Female | Child | No       |    0 |
| 2nd   | Female | Child | No       |    0 |
| 3rd   | Female | Child | No       |   17 |
| Crew  | Female | Child | No       |    0 |
| 1st   | Male   | Adult | No       |  118 |
| 2nd   | Male   | Adult | No       |  154 |
| 3rd   | Male   | Adult | No       |  387 |
| Crew  | Male   | Adult | No       |  670 |
| 1st   | Female | Adult | No       |    4 |
| 2nd   | Female | Adult | No       |   13 |
| 3rd   | Female | Adult | No       |   89 |
| Crew  | Female | Adult | No       |    3 |
| 1st   | Male   | Child | Yes      |    5 |
| 2nd   | Male   | Child | Yes      |   11 |
| 3rd   | Male   | Child | Yes      |   13 |
| Crew  | Male   | Child | Yes      |    0 |
| 1st   | Female | Child | Yes      |    1 |
| 2nd   | Female | Child | Yes      |   13 |
| 3rd   | Female | Child | Yes      |   14 |
| Crew  | Female | Child | Yes      |    0 |
| 1st   | Male   | Adult | Yes      |   57 |
| 2nd   | Male   | Adult | Yes      |   14 |
| 3rd   | Male   | Adult | Yes      |   75 |
| Crew  | Male   | Adult | Yes      |  192 |
| 1st   | Female | Adult | Yes      |  140 |
| 2nd   | Female | Adult | Yes      |   80 |
| 3rd   | Female | Adult | Yes      |   76 |
| Crew  | Female | Adult | Yes      |   20 |

This is a simple set of data, with a single count observation, `Freq`,
for each combination of the four determining attributes. In other words,
the relation is already normalised, so we only expect one relation in
the normalised database.

If we use `autodb` again, we get the following database:

``` r
show(autodb(as.data.frame(Titanic)))
```

Oops! The search found some functional dependencies where the count
could be used to determine another attribute. These are clearly
spurious: frequency count can’t causally determine age, for example.

There are two ways we can remove these spurious dependencies: limiting
the search to not check them in the first place, and removing them from
the dependencies before using `synthesise`/`normalise`.

To limit the search, we can set certain attributes to not be considered
as members of determinants, or we can exclude attributes that inherit
from certain classes. In this example, we could exclude `Freq` from
being considered:

``` r
titanic_deps_freqonly <- discover(as.data.frame(Titanic), exclude = "Freq")
titanic_deps_freqonly
```

    ## 1 functional dependency
    ## 5 attributes: Class, Sex, Age, Survived, Freq
    ## Class, Sex, Age, Survived -> Freq

Alternatively, we could exclude all attributes that inherit from
“numeric”:

``` r
stopifnot(setequal(
  titanic_deps_freqonly,
  discover(as.data.frame(Titanic), exclude_class = "numeric")
))
```

We can also limit the search when using `autodb`:

``` r
show(autodb(as.data.frame(Titanic), exclude = "Freq"))
```

Excluding numeric attributes as determinants is often useful, because we
expect non-integer numbers to be a measurement that doesn’t co-determine
anything else. The main exception is when some attributes are summaries
of others.

Alternatively, we can remove the unwanted dependencies. Here are all the
found dependencies, if we don’t exclude anything:

``` r
titanic_deps <- discover(as.data.frame(Titanic))
titanic_deps
```

    ## 3 functional dependencies
    ## 5 attributes: Class, Sex, Age, Survived, Freq
    ##       Sex, Survived, Freq -> Age
    ##     Class, Survived, Freq -> Age
    ## Class, Sex, Age, Survived -> Freq

We can remove the unwanted dependencies, where `Age` is the dependant,
using subsetting, `Filter`, etc.:

``` r
titanic_deps[dependant(titanic_deps) == "Age"]
```

    ## 2 functional dependencies
    ## 5 attributes: Class, Sex, Age, Survived, Freq
    ##   Sex, Survived, Freq -> Age
    ## Class, Survived, Freq -> Age

## Avoidable attributes

The next normal form after third normal form (3NF) is Boyes-Codd normal
form (BCNF). Ensuring BCNF is enforced by the database is trickier: in
some cases, it can’t be enforced with just relations and foreign key
constraints.

However, the package includes an option to convert to enhanced third
normal form, also known as LTK form, which can be so enforced. This
enhancement is tangential to BCNF, and could also be used to enhance
schemas in BCNF.

In brief, the standard normal forms only put constraints on the
attributes present in the relations one relation at a time. The
enhancement is a constraint on the attributes present in a relation,
while considering their presence in other relations. If a attribute in a
relation can be removed, and still be determined from that relation by
joining it to others, then the attribute is “avoidable”, and can be
removed. If the attribute is in any of the relation’s keys, they’ll be
replaced by keys that use the attributes not being removed. This removes
attributes from relations without removing any information from the
database as a whole.

For example, we can take this simple example from Chapter 6 of The
Theory of Relational Databases, by David Maier:

``` r
avoid_deps <- functional_dependency(
  list(
    list("A", "B"),
    list("B", "A"),
    list(c("A", "C"), "D"),
    list(c("A", "C"), "E"),
    list(c("B", "D"), "C")
  ),
  attrs_order = c("A", "B", "C", "D", "E")
)
avoid_deps
```

    ## 5 functional dependencies
    ## 5 attributes: A, B, C, D, E
    ##    A -> B
    ##    B -> A
    ## A, C -> D
    ## A, C -> E
    ## B, D -> C

``` r
normalise(avoid_deps)
```

    ## database schema with 2 relation schemas
    ## 5 attributes: A, B, C, D, E
    ## schema A: A, B
    ##   key 1: A
    ##   key 2: B
    ## schema A_C: A, C, B, D, E
    ##   key 1: A, C
    ##   key 2: B, D
    ## references:
    ## A_C.{A} -> A.{A}
    ## A_C.{B} -> A.{B}

``` r
show(normalise(avoid_deps))
```

Attributes `A` and `B` are equivalent, since relation `A` has them both
as a key. In other words, relation `A` is a simple lookup relation.
Because of this, we could remove `B` from relation `A_C`, and replace
the key `B, D` with `A, D`, which is equivalent when accounting for
relation `A`.

We can have this removal of avoidable attributes done automatically,
using the `remove_avoidable` flag for `normalise`:

``` r
normalise(
  avoid_deps,
  remove_avoidable = TRUE
) |>
  show()
```

This schema is now in LTK form, with no remaining avoidable attributes.
We could have also removed `A` from relation `A_C` instead of `B`, so
this process may not have a unique result. The package’s implementation
prefers to remove attributes that appear later in the original relation.
