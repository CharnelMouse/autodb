# autodb (development version)

Breaking changes:

* Renamed `dfd` to `discover`, to reflect the generalisation to allow the use of other methods. At the moment, this includes DFD and Tane.
* Removed `flatten` from exported functions, in favour of flattening the functional dependencies in `dfd`/`discover` instead. Since `flatten` was usually called anyway, and its output is more readable since adding a `print` method for it, the old `dfd`/`discover` output format had little reason to be kept.
* Renamed `normalise` to `synthesise`, to reflect its only creating relation schemas, not foreign key references. `normalise` now calls a wrapper for both `synthesise` and `cross_reference`, since in most cases we don't need to do these steps separately. Additionally, `ensure_lossless` is now an argument for `synthesise` rather than `cross_reference`: this is a more nature place to put it, since `synthesise` creates relations, and `cross_reference` adds foreign key references.
* As noted in improvements, functional dependency objects now have their own subsetting methods. In particular, they have a `[[` method, so code that used `[[` to extract determinant sets or dependents from functional dependencies will no longer work. These should be extracted with the new `detset` and `dependent` functions instead.

Improvements:

* Added a `functional_dependency` class for flattened functional dependency sets. The attributes vector is now stored as an attribute, so that the dependencies can be accessed as a simple list without list subsetting operators. There are `[`, `[[`, and `unique` methods to facilitate this vector-like access. There is also a method for `c`, which attempts to merge attributes vectors while conserving the initial orderings, and `detset`, `dependent`, and `attrs_order` generic functions for extracting the relevant parts. `detset` and `dependent`, in particular, should be useful for the purposes of filtering predicates.
* Added a `relation_schema` class for relational schema sets, as returned by `synthesise`. The attributes and keys are now stored together in a named list, with the `attrs_order` vector attribute order stored as an attribute. As with the `functional_dependency`, this lets the schemas be accessed like a vector. There are `[`, `[[`, and `unique` methods for vector-like access. There is also a method for `c`, which attempts to merge attributes vectors while conserving the initial orderings, `merge_empty_keys` for combining schemas with an empty key, and `attrs`, `keys`, and `attrs_order` generic functions for extracting the relevant parts.
* Added a `database_schema` class for database schemas, as returned by `normalise`. This inherits from `relation_schema`, and has foreign key references as an additional `relationships` attribute. There are `[`, `[[`, `unique`, `c`, and `merge_empty_keys` methods that conserve validity of the foreign key references.
* The `database` class no longer assigns a `parents` attribute to each relation, since this duplicates the foreign key reference information given in `relationships`.
* Adjusted `normalise` to prefer to remove dependencies with dependents and determinant sets later in table order, and with larger dependent sets. This brings it more in line with similar decisions made in other package functions.
* `relationships` in `database_schema` and `database` objects are now stored in a format that better reflects their being foreign key constraints: instead of an element for each pair of attributes in a foreign key, there is one element for the whole foreign key, containing all of the involved attributes. Similarly, they are now printed in the format "child.{c1, c2, ...} -> parent.{p1, p2, ...}" instead of "child.c1 -> parent.p1; child.c2 -> parent.p2; ...".
* Simplified some internals of `dfd`/`discover` to improve computation time.
* Added a `skip_bijections` option to `dfd`/`discover`, to speed up functional dependency searches where there are pairwise-equivalent attributes present.
* Added an option to use Tane instead of DFD for functional dependency search.

Fixes:

* Corrected vignette re: when to remove spurious dependencies before.
* Corrected `autodb` documentation link to page with database format information.
* Corrected `df_equiv` to work with `data.frame` columns that are lists.
* Fixed `normalise`'s return output to be invariant to the given order of the functional_dependency input.
* Fixed `normalise` returning relations with attributes in the wrong order in certain cases where `remove_avoidable = TRUE`.

# autodb 1.1.0

* Added a `NEWS.md` file to track changes to the package.

Improvements:

* Added examples for `autodb`, `dfd`, `gv`, and `rejoin`.
* Added a reference for removing avoidable attributes for enhanced third normal
form.
* Changed `decompose` to return an error if the data.frame doesn't satisfy the functional dependencies implied by the schema. This will return an error when using `decompose` with a schema derived from the same data.frame if any approximate dependencies were included. Previously, using `decompose` or `dfd` with approximate dependencies would result in constructing a database with duplicate key values, since there's currently no handling of approximate dependencies during database construction, and records ignored in approximate dependencies were being kept. This is incorrect behaviour; `decompose` will be added back for approximate dependencies once the package can properly handle them.
* Made `reduce` generic, and added a method for database schemas. Currently this method requires explicitly naming the main relations, rather than inferring them.
* Removed incorrect comment in vignette about needing foreign keys to reconstruct the original data frame from a database.
* Tidied up `nudge` data documentation, improved commentary on publication references in vignette.
* Removed accuracy argument from `autodb`, due to approximate dependencies now returning an error in `decompose`.

Fixes:

* Fixed `print.database` to refer to records instead of rows.
* Fixed existing reference formatting.
* Fixed references for missing-value implementation mockup in vignette.
* Fixed a bug in `normalise` that resulted in relations having duplicate keys.
* Fixed a bug in `normalise`, that resulted in schemas that didn't reproduce the given functional dependencies.
* Fixed `dfd`'s data simplification step for POSIXct datetimes, in case where two times only differ by standard/daylight-savings time (e.g. 1:00:00 EST vs. 1:00:00 EDT on the same day).
* Fixed a bug in `dfd` with cache = TRUE, where data frame column names being argument names for `paste` can result in an error.
* Fixed decomposition for tables with zero columns (TABLE_DUM and TABLE_DEE) to allow lossless rejoin.
* Fixed a bug where the output of `gv` methods included Graphviz syntax errors when given relations with zero-length names. `gv.data.frame` now requires `name` to be non-empty; `gv.database_schema` and `gv.database` replace zero-length names.

# autodb 1.0.0

* Initial version
