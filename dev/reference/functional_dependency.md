# Functional dependency vectors

Creates a set of functional dependencies with length-one dependants.

## Usage

``` r
functional_dependency(FDs, attrs_order, unique = TRUE)
```

## Arguments

- FDs:

  a list of functional dependencies, in the form of two-elements lists:
  the first element contains a character vector of all attributes in the
  determinant set, and the second element contains the single dependent
  attribute (dependant).

- attrs_order:

  a character vector, giving the names of all attributes. These need not
  be present in `FDs`, but all attributes in `FDs` must be present in
  `attrs`.

- unique:

  a logical, TRUE by default, for whether to remove duplicate
  dependencies.

## Value

A `functional_dependency` object, containing the list given in `FDs`,
with `attrs_order` an attribute of the same name. Functional
dependencies are returned with their determinant sets sorted according
to the attribute order in `attrs`. Any duplicates found after sorting
are removed.

## Details

When several sets of functional dependencies are concatenated, their
`attrs_order` attributes are merged, so as to preserve all of the
original attribute orders, if possible. If this is not possible, because
the orderings disagree, then the returned value of the `attrs_order`
attribute is their union instead.

## See also

[`detset`](https://charnelmouse.github.io/autodb/dev/reference/detset.md),
[`dependant`](https://charnelmouse.github.io/autodb/dev/reference/dependant.md),
and
[`attrs_order`](https://charnelmouse.github.io/autodb/dev/reference/attrs_order.md)
for extracting parts of the information in a `functional_dependency`;
[`rename_attrs`](https://charnelmouse.github.io/autodb/dev/reference/rename_attrs.md)
for renaming the attributes in `attrs_order`.

## Examples

``` r
fds <- functional_dependency(
  list(list(c("a", "b"), "c"), list(character(), "d")),
  attrs_order = c("a", "b", "c", "d")
)
print(fds)
#> 2 functional dependencies
#> 4 attributes: a, b, c, d
#> a, b -> c
#>      -> d
detset(fds)
#> [[1]]
#> [1] "a" "b"
#> 
#> [[2]]
#> character(0)
#> 
dependant(fds)
#> [1] "c" "d"
attrs_order(fds)
#> [1] "a" "b" "c" "d"

# vector operations
fds2 <- functional_dependency(list(list("e", "a")), c("a", "e"))
c(fds, fds2) # attrs_order attributes are merged
#> 3 functional dependencies
#> 5 attributes: a, b, c, d, e
#> a, b -> c
#>      -> d
#>    e -> a
unique(c(fds, fds))
#> 2 functional dependencies
#> 4 attributes: a, b, c, d
#> a, b -> c
#>      -> d

# subsetting
fds[1]
#> 1 functional dependency
#> 4 attributes: a, b, c, d
#> a, b -> c
fds[c(1, 2, 1)]
#> 3 functional dependencies
#> 4 attributes: a, b, c, d
#> a, b -> c
#>      -> d
#> a, b -> c
stopifnot(identical(fds[[2]], fds[2]))

# reassignment
fds3 <- fds
fds3[2] <- functional_dependency(list(list("a", "c")), attrs_order(fds3))
print(fds3)
#> 2 functional dependencies
#> 4 attributes: a, b, c, d
#> a, b -> c
#>    a -> c
detset(fds3)[[2]] <- character()
dependant(fds3)[[2]] <- "d"
stopifnot(identical(fds3, fds))
# changing appearance priority for attributes
attrs_order(fds3) <- rev(attrs_order(fds3))
fds3
#> 2 functional dependencies
#> 4 attributes: d, c, b, a
#> b, a -> c
#>      -> d

# reconstructing from components
fds_recon <- functional_dependency(
 Map(list, detset(fds), dependant(fds)),
 attrs_order(fds)
)
stopifnot(identical(fds_recon, fds))

# can be a data frame column
data.frame(id = 1:2, fd = fds)
#>   id        fd
#> 1  1 a, b -> c
#> 2  2      -> d

# (in)equality ignores header
stopifnot(all(fds3 == fds))
stopifnot(!any(fds != fds))
stopifnot(all(fds <= fds))
```
