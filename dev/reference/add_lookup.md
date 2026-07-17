# Add attribute lookup relations

Create single-attribute relations for each of a given set of attributes.
If the original object also contains references, then the new lookup
relations are connected to other appearances of their attribute by
chains of references.

## Usage

``` r
add_lookup(x, as, ...)
```

## Arguments

- x:

  a relational schema object, such as a
  [`relation_schema`](https://charnelmouse.github.io/autodb/dev/reference/relation_schema.md)
  or
  [`database_schema`](https://charnelmouse.github.io/autodb/dev/reference/database_schema.md)
  object, or a relational data object, such as a
  [`relation`](https://charnelmouse.github.io/autodb/dev/reference/relation.md)
  or
  [`database`](https://charnelmouse.github.io/autodb/dev/reference/database.md)
  object.

- as:

  a character vector of elements from
  [`attrs_order`](https://charnelmouse.github.io/autodb/dev/reference/attrs_order.md)`(x)`,
  indicating which attributes to create lookup tables for.

- ...:

  further arguments pass on to methods.

## Value

an object of the same class as `x`.

## Details

Whether an attribute in `as` gets a new key relation depends on the
relations already present. An existing relation is considered to be a
lookup if the following hold:

- The attribute is a simple key for the relation;

- If `x` is a relation data object, then the relation contains all given
  values for that attribute.

If several relations could be a lookup for the attribute, then
`add_lookup` fails.

## Examples

``` r
db <- autodb(ChickWeight)
db
#> database with 2 relations
#> 4 attributes: weight, Time, Chick, Diet
#> relation Chick: Chick, Diet; 50 records
#>   key 1: Chick
#> relation Time_Chick: Time, Chick, weight; 578 records
#>   key 1: Time, Chick
#> references:
#> Time_Chick.{Chick} -> Chick.{Chick}
add_lookup(db, "Time")
#> database with 3 relations
#> 4 attributes: weight, Time, Chick, Diet
#> relation Chick: Chick, Diet; 50 records
#>   key 1: Chick
#> relation Time_Chick: Time, Chick, weight; 578 records
#>   key 1: Time, Chick
#> relation Time: Time; 12 records
#>   key 1: Time
#> references:
#> Time_Chick.{Chick} -> Chick.{Chick}
#> Time_Chick.{Time} -> Time.{Time}
add_lookup(db, "Chick") # Chick is already a key, so this does nothing
#> database with 2 relations
#> 4 attributes: weight, Time, Chick, Diet
#> relation Chick: Chick, Diet; 50 records
#>   key 1: Chick
#> relation Time_Chick: Time, Chick, weight; 578 records
#>   key 1: Time, Chick
#> references:
#> Time_Chick.{Chick} -> Chick.{Chick}
# data objects round numeric and complex values before checking given values
add_lookup(db, "weight", digits = 3)
#> database with 3 relations
#> 4 attributes: weight, Time, Chick, Diet
#> relation Chick: Chick, Diet; 50 records
#>   key 1: Chick
#> relation Time_Chick: Time, Chick, weight; 578 records
#>   key 1: Time, Chick
#> relation weight: weight; 212 records
#>   key 1: weight
#> references:
#> Time_Chick.{Chick} -> Chick.{Chick}
#> Time_Chick.{weight} -> weight.{weight}
if (FALSE) add_lookup(c(db, db), "Chick") # fails: two lookup candidates # \dontrun{}
```
