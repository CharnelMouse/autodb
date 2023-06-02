# autodb (development version)

Breaking changes:

* Removed `flatten` from exported functions, in favour of flattening the functional dependencies in `dfd` instead. Since `flatten` was usually called anyway, and its output is more readable since adding a `print` method for it, the old `dfd` output format had little reason to be kept.

Improvements:

* Added a `functional_dependency` class for flattened functional dependency sets. The attributes vector is now stored as an attribute, so that the dependencies can be accessed as a simple list without list subsetting operators. There is also a method for `c`, which attempts to merge attributes vectors while conserving the initial orderings.
* Adjusted `normalise` to prefer to remove dependencies with dependents and determinant sets later in table order, and with larger dependent sets. This brings it more in line with similar decisions made in other package functions.
* Simplified some internals of `dfd` to improve computation time.
* Added a `skip_bijections` option to `dfd`, to speed up functional dependency searches where there are pairwise-equivalent attributes present.
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
