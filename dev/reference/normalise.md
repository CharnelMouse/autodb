# Create normalised database schemas from functional dependencies

Creates a database schema from given functional dependencies, satisfying
at least third normal form, using Bernstein's synthesis.

## Usage

``` r
normalise(
  dependencies,
  single_ref = FALSE,
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
  [`functional_dependency`](https://charnelmouse.github.io/autodb/dev/reference/functional_dependency.md)
  object, as given by
  [`discover`](https://charnelmouse.github.io/autodb/dev/reference/discover.md).

- single_ref:

  a logical, FALSE by default. If TRUE, then only one reference between
  each relation pair is kept when generating foreign key references. If
  a pair has multiple references, the kept reference refers to the
  earliest key for the child relation, as sorted by priority order.

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
  [`discover`](https://charnelmouse.github.io/autodb/dev/reference/discover.md).

- progress_file:

  a scalar character or a connection. If `progress` is non-zero,
  determines where the progress is written to, in the same way as the
  `file` argument for [`cat`](https://rdrr.io/r/base/cat.html).

## Value

A
[`database_schema`](https://charnelmouse.github.io/autodb/dev/reference/database_schema.md)
object, containing the synthesis relation schemas and the created
foreign key references.

## Details

This is a wrapper function for applying
[`synthesise`](https://charnelmouse.github.io/autodb/dev/reference/synthesise.md)
and
[`autoref`](https://charnelmouse.github.io/autodb/dev/reference/autoref.md),
in order. For creating relation schemas and foreign key references
separately, use these functions directly. See both functions for
examples.

For details on the synthesis algorithm used, see
[`synthesise`](https://charnelmouse.github.io/autodb/dev/reference/synthesise.md).
