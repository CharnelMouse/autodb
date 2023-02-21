# autodb 1.0.0.9000

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

# autodb 1.0.0

* Initial version
