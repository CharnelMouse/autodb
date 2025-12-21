# Remove extraneous components from functional dependencies

Remove extraneous components from functional dependencies

## Usage

``` r
remove_extraneous(x, reduce_attributes = TRUE)
```

## Arguments

- x:

  a
  [`functional_dependency`](https://charnelmouse.github.io/autodb/dev/reference/functional_dependency.md)
  object.

- reduce_attributes:

  a logical, TRUE by default. If TRUE, `dependencies` are checked for
  determinant attributes that are made redundant by the other
  dependencies. This is redundant if `dependencies` is output from
  `discover`, since there will be no such redundant attributes.

## Value

a minimal cover of the original dependencies, i.e. a subset where those
remaining are not implied by the others. If `remove_attributes` is TRUE,
dependencies in the determinant are removed if the other dependencies
show them to be redundant.

## Examples

``` r
# a -> b and b -> c imply a -> c
x <- functional_dependency(
  list(list("a", "b"), list("b", "c"), list("a", "c")),
  c("a", "b", "c")
)
remove_extraneous(x)
#> 2 functional dependencies
#> 3 attributes: a, b, c
#> a -> b
#> b -> c

# a -> b lets us remove b from {a, b} -> c
x <- functional_dependency(
  list(list("a", "b"), list(c("a", "b"), "c")),
  c("a", "b", "c")
)
remove_extraneous(x)
#> 2 functional dependencies
#> 3 attributes: a, b, c
#> a -> b
#> a -> c
```
