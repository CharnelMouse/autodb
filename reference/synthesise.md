# Synthesise relation schemas from functional dependencies

Synthesises the dependency relationships in dependencies into a database
schema satisfying at least third normal form, using Bernstein's
synthesis.

## Usage

``` r
synthesise(
  dependencies,
  ensure_lossless = TRUE,
  reduce_attributes = TRUE,
  remove_avoidable = FALSE,
  constants_name = "constants",
  progress = FALSE,
  progress_file = ""
)
```

## Arguments

- dependencies:

  a
  [`functional_dependency`](https://charnelmouse.github.io/autodb/reference/functional_dependency.md)
  object, as given by
  [`discover`](https://charnelmouse.github.io/autodb/reference/discover.md).

- ensure_lossless:

  a logical, TRUE by default. If TRUE, and the decomposition isn't
  lossless, an extra relation is added to make the decomposition
  lossless.

- reduce_attributes:

  a logical, TRUE by default. If TRUE, `dependencies` are checked for
  determinant attributes that are made redundant by the other
  dependencies. This is redundant if `dependencies` is output from
  `discover`, since there will be no such redundant attributes.

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

## Value

A
[`relation_schema`](https://charnelmouse.github.io/autodb/reference/relation_schema.md)
object, containing the synthesised relation schemas.

## Details

Bernstein's synthesis is a synthesis algorithm for normalisation of a
set of dependencies into a set of relations that are in third normal
form. This implementation is based on the version given in the
referenced paper.

The implementation also includes a common additional step, to ensure
that the resulting decomposition is lossless, i.e. a relation satisfying
the given dependencies can be perfectly reconstructed from the relations
given by the decomposition. This is done by adding an additional
relation, containing a key for all the original attributes, if one is
not already present.

As an additional optional step, schemas are checked for "avoidable"
attributes, that can be removed without loss of information.

Constant attributes, i.e. those whose only determinant set is empty, get
assigned to a relation with no keys.

Output is independent of the order of the input dependencies: schemas
are sorted according to their simplest keys.

Schemas are sorted before ensuring for losslessness, or removing
avoidable attributes. As a result, neither optional step changes the
order of the schemas, and ensuring losslessness can only add an extra
schema to the end of the output vector.

## References

3NF synthesis algorithm: Bernstein P. A. (1976) Synthesizing third
normal form relations from functional dependencies. *ACM Trans. Database
Syst.*, **1, 4**, 277–298.

Removal of avoidable attributes: Ling T., Tompa F. W., Kameda T. (1981)
An improved third normal form for relational databases. *ACM Trans.
Database Syst.*, **6, 2**, 329–346.

## Examples

``` r
# example 6.24 from The Theory of Relational Databases by David Maier
# A <-> B, AC -> D, AC -> E, BD -> C
deps <- functional_dependency(
  list(
    list("A", "B"),
    list("B", "A"),
    list(c("A", "C"), "D"),
    list(c("A", "C"), "E"),
    list(c("B", "D"), "C")
  ),
  attrs_order = c("A", "B", "C", "D", "E")
)
synthesise(deps, remove_avoidable = FALSE)
#> 2 relation schemas
#> 5 attributes: A, B, C, D, E
#> schema A: A, B
#>   key 1: A
#>   key 2: B
#> schema A_C: A, C, B, D, E
#>   key 1: A, C
#>   key 2: B, D
synthesise(deps, remove_avoidable = TRUE)
#> 2 relation schemas
#> 5 attributes: A, B, C, D, E
#> schema A: A, B
#>   key 1: A
#>   key 2: B
#> schema A_C: A, C, D, E
#>   key 1: A, C
#>   key 2: A, D
```
