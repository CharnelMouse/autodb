# autodb (development version)

## Breaking changes

* The argument list for `discover` has been rearranged:
  * The `accuracy` argument is now optional, defaulting to one for exact dependency search.  This reflects the reduced focus on approximate dependencies: the main `autodb` function doesn't allow for them anyway, and the new FDHits search algorithms can only search for exact dependencies.
  * Arguments specific to the DFD algorithm have been moved to the back of the list, since they are of lesser priority. This includes the `accuracy` parameter.
  * The `skip_bijections` argument is now first in the DFD-specific arguments, since setting it to `TRUE` speeds up the search, where the other non-`accuracy` parameters slow it down.

## Improvements

* The package has no remaining package dependencies. There is still an implicit dependency on GraphViz, if you use `gv` to export plotting code.
* Added the new FDHits search algorithm as an alternative option to DFD in `discover`.
* Performance improvements:
  * `relation` and `database` methods for `rename_attrs` and `gv` now run significantly more quickly for large data sets, due to not re-running all the key validity checks.
  * Removed the unnecessary validity checks from the `database` method for `reduce`, so that it runs more quickly, especially for databases with a large number of records.
  * Removed unnecessary validity checks from the `database_schema` and `database` methods for `[`.
  * Removed unnecessary closure calculations in `normalise`.
  * `normalise`, `synthesise`, and `autoref` have improved performance, due to more efficient closure checks.
* Added a `check` parameter to `decompose`, to allow skipping some checks if the data frame to decompose is the one used to create the schema.
* Handing for numerical/complex variables is more consistent:
    * Values are now rounded by significant digits, as intended, rather than by decimal places.
    * In addition to `autodb`, `discover`, and `df_equiv`, values are now also rounded for `insert` and `decompose`.
* Some `relation` error messages are more informative.
* Added a `main` argument for the `database` method of `reduce`.

## Fixes

* Fixed the `format` method for `relation` to describe elements as relations, rather than schemas.
* Fixed `autodb` to pass its `progress_file` argument on to `discover`.

# autodb 2.3.1

Some minor changes to tests, as part of re-submission to CRAN.

# autodb 2.3.0

Some performance improvements, as part of re-submission to CRAN.

## Improvements

* Added a convenience function called `df_records`, for converting a data frame into a list of row values. These are sometimes more useful than data frames, e.g. for checking which rows of a data frame are present in another one.
* Improved the run time of `database` for larger data sets, specifically the validity checks for the data satisfying the foreign key references.
* Improved the run time of `create` for `relation_schema` and `database_schema`, by removing validity checks. If the input is valid, these are redundant.
* Improved the run time of `autodb`, by skipping removal of extraneous attributes. This is done on the results of `discover`, so there won't be any.

## Fixes

* CRAN fixes to DESCRIPTION.
* Fixes to documentation.

# autodb 2.2.1

## Fixes

* CRAN fixes to DESCRIPTION.
* Fixes to documentation: the PDF version of the manual is now created successfully.

# autodb 2.2.0

Continuing efforts to prepare for submission to CRAN.

## Improvements

* Changed handling for numerical/complex variables in `autodb`, `discover`, and `df_equiv`, to round to a number of significant digits. Due to the nature of floating-point, and the definition of a functional dependency, floating-point values can't be compared using equality (`==`), or by `all.equal` for the purposes of functional dependency discovery / validation, and have the result be consistent between different machines. Because of this, floating-point variables are now rounded to a small level of precision by default before processing. If the data frame is being loaded from a file, we recommend reading any numerical/complex variables as character values (strings), if it's appropriate, to avoid loss of precision.
* `df_equiv` now checks rows for exact matches, outside of the rounding mentioned above. Previously, it compared rows using `match`, which gave no control over float precision.
* `relation_schema`, `relation`, `database_schema`, and `database` now only return a name-based subset successfully if all of the given names exist in the object.

# autodb 2.1.1

Some minor changes to documentation and tests, to allow for package updates and submission to CRAN.

# autodb 2.1.0

## Improvements

* Added `format` and `as.data.frame` methods for `functional_dependency`, `relation_schema`, `database_schema`, `relation`, and `database`. This allows them to be columns in a data frame at initial construction. I'm not sure *why* you'd want to put them a a data frame column, but it's consistent with the idea that the objects from these classes should mostly be treatable as vectors. Be warned: they don't currently work in tibbles.
* Added an `as.character` method for `functional_dependency`. The optional `align_arrows` argument can add padding to one side, in order to make the arrows align when they're printed to different lines. These options are used to align arrows in its `print` method, and its `format` method for when printed as a data frame column.
* Added `==` and `!=` implementations for `functional_dependency`. These ignore differences in `attrs_order`: differently-ordered determinant sets are considered equal.
* Added a `rename_attrs` method for `functional_dependency`.
* Added a `dependants` argument to `discover`, which limit the functional dependency search to those with a dependant in the given set of column names, defaulting to all of them. This should significantly speed up searches where only some dependants are of interest.
* Added a `detset_limit` argument for `discover`/`autodb`, which limits the FD search to only look for dependencies with the determinant set size under a given limit. For DFD, this usually doesn't significantly reduce the search time, but it won't make it worse. It will be useful once other search algorithms are implemented.
* Added an `all` argument to `insert`, `FALSE` by default. If `TRUE`, then `insert` returns an error if the data to insert doesn't include all attribute for the elements being inserted into, rather than skipping those elements. This helps to prevent accidental no-ops.
* Running discover() or autodb() with `progress = TRUE` now keeps the output display up to date when using a console-based version of R.
* DFD now checks for single-attribute keys, and excludes them as determinants in the main search, potentially reducing the search time.

## Fixes

* Fixed `gv` to account for Graphviz HTML-like labels requiring certain characters, namely the set "<>&, to be escaped in Graphviz HTML-like labels, and removed completely in attribute values.
* Fixed `df_equiv` to properly handle data frames with zero columns or duplicate rows.
* Fixed `database_schema` and `database`, and reference re-assignments, to allow references to be given with the referee's key not in attribute order.

# autodb 2.0.0

The general theme for this version is classes for intermediate results: functional dependencies, schemas, and databases now have fleshed-out classes, with methods to keep them self-consistent. They all have their own constructors, for users to create their own, instead of having to generate them from a given data frame.

## Breaking changes

* Renamed `dfd` to `discover`, to reflect the generalisation to allow the use of other methods. At the moment, this just includes DFD.
* Removed `flatten` from exported functions, in favour of flattening the functional dependencies in `dfd`/`discover` instead. Since `flatten` was usually called anyway, and its output is more readable since adding a `print` method for it, there was little reason to keep the old `dfd`/`discover` output format, where functional dependencies were grouped by dependant.
* Renamed `cross_reference` to `autoref`, to better reflect its purpose as generating foreign key references.
* Renamed `normalise` to `synthesise`, to reflect its only creating relation schemas, not foreign key references. The new function named `normalise` now calls a wrapper for both `synthesise` and `autoref`, since in most cases we don't need to do these steps separately. Additionally, `ensure_lossless` is now an argument for `synthesise` rather than `autoref`: this is a more nature place to put it, since `synthesise` creates relations, and `autoref` adds foreign key references.
* As noted in improvements, functional dependency objects now have their own subsetting methods. In particular, they have a `[[` method, so code that used `[[` to extract determinant sets or dependants from functional dependencies will no longer work. These should be extracted with the new `detset` and `dependant` functions instead.
* Similarly, the `database` class has its own subsetting methods, so components must be extracted with `records`, `keys`, and so on.
* The `database` class no longer assigns a `parents` attribute to each relation, since this duplicates the foreign key reference information given in `references`.
* The `database` class no longer has a `name` attribute. This was only used to name the graph when using the `gv` function, so is now an argument for the `database` method of `gv` instead, bringing its arguments into line with those of the other methods.
* `relationships` in `database_schema` and `database` objects are now called `references`, to better reflect their being foreign key constraints, and they are stored in a format that better reflects this: instead of an element for each pair of attributes in a foreign key, there is one element for the whole foreign key, containing all of the involved attributes. Similarly, they are now printed in the format "child.{c1, c2, ...} -> parent.{p1, p2, ...}" instead of "child.c1 -> parent.p1; child.c2 -> parent.p2; ...".
* `cross_reference`/`autoref` now defaults to generating more than one foreign key reference per parent-child relation pair, rather than keeping only the one with the first child key by priority order. This can result in some confusion on plots, since references are still plotted one attribute pair at a time.

## Improvements

* Added classes and methods for important data structures:
    * Added a `functional_dependency` class for flattened functional dependency sets. The attributes vector is now stored as an attribute, so that the dependencies can be accessed as a simple list without list subsetting operators. There are also `detset`, `dependant`, and `attrs_order` generic functions for extracting the relevant parts. `detset` and `dependant`, in particular, should be useful for the purposes of filtering predicates.
    * Added a `relation_schema` class for relational schema sets, as returned by `synthesise`. The attributes and keys are now stored together in a named list, with the `attrs_order` vector attribute order stored as an attribute. As with the `functional_dependency`, this lets the schemas be accessed like a vector. There is also `merge_empty_keys` for combining schemas with an empty key, and `attrs`, `keys`, and `attrs_order` generic functions for extracting the relevant parts.
    * Added a `database_schema` class for database schemas, as returned by `normalise`. This inherits from `relation_schema`, and has foreign key references as an additional `references` attribute. There is a `merge_empty_keys` method that conserves validity of the foreign key references. Additionally, when the names of the contained relation schemas are changed using `names<-`, the references are changed to use the new names.
    * Added a `relation` class for vectors of relations containing data. Since a `database_schema` is just a `relation_schema` vector with foreign key references added, the `relation` class was added as the equivalent underlying vector for the `database` class. A user of the package probably won't need to use it.
    * `database` is now a wrapper class around `relation`, that adds foreign key references, and handles them separately in its methods.
    * All of the above have their own methods for the `[`, `[[`, and -- except for `functional_dependency` -- `$` subsetting operators, along with their replacement equivalents, `[<-` etc., to allow treating them as vectors of relation schemas or relations. Subsetting also removes any foreign key references in `database_schema` and `database` objects that are no longer relevant. These methods prevent the subsetting operators from being used to access the object's internal components, so many of the generic functions mentioned above were written to allow access in a more principled manner, not requiring knowledge of how the structure is implemented.
    * All of the above have a `c` method for vector-like concatenation. There are two non-trivial aspects to this. Firstly, when concatenating objects with different `attrs_order` attributes, `c` merges the orders to keep them consistent, if possible. Secondly, for `database_schema` and `database`, foreign key references are changed to reflect any changes made to relation names to keep them unique.
    * All of the above have a `unique` method for vector-like removal of duplicate schemas / relations. This conserves validity of foreign key references for `database_schema` and `database` objects. For `relation` and `database` objects, duplication doesn't require records to be kept in the same order.
* Added some basic database creation/manipulation generic functions for the above data structures:
    * All of the above have a `names<-` method for consistently changing relation (schema) names. In particular, for databases and database schemas, this ensures the names are also changed in references.
    * All of the above, except `functional_dependency`, have a `rename_attrs` method for renaming the attributes across the whole object. This renames them in all schemas, relations, references, and so on.
    * Added a `create` generic function, for creating `relation` and `database` objects from `relation_schema` and `database_schema` objects, respectively. The created objects contain no data. This function is roughly the equivalent to `CREATE TABLE` in SQL, but the vectorised nature of the relation classes means that several tables are created at once.
    * Added an `insert` generic function for `relation` and `database` objects, which takes a data frame of new data, and inserts it into any relation in the object whose attributes are all present in the new data. This is roughly equivalent to SQL's `INSERT`, but works over multiple relations at once, and means there's now a way to put data into a `database` outside of `decompose`. Indeed, `decompose` is now equivalent to calling `create`, then calling `insert` with all the relations.
* Adjusted `normalise` to prefer to remove dependencies with dependants and determinant sets later in table order, and with larger dependant sets. This brings it more in line with similar decisions made in other package functions.
* Simplified some internals of `dfd`/`discover` to improve computation time.
* Added a `skip_bijections` option to `dfd`/`discover`, to speed up functional dependency searches where there are pairwise-equivalent attributes present.

## Fixes

* Corrected vignette re: when to remove spurious dependencies before.
* Corrected `autodb` documentation link to page with database format information.
* Corrected `df_equiv` to work with `data.frame` columns that are lists.
* Fixed several issues related to doubles / floating-point:
  * Fixed `dfd`/`discover` treating similar numeric values as equal, resulting in data frames not being insertable into their own schema.
  * Fixed `database` checks not handling doubles correctly. Specifically, foreign key reference checks involve merging tables together, and merge operates on doubles with a tolerance that's set within an internal method, so merges can create duplicates that need to be removed afterwards.
  * Similarly, fixed `rejoin` in the case where merges are based on doubles, sometimes resulting in duplicates.
* Fixed `normalise`'s return output to be invariant to the given order of the `functional_dependency` input.
* Fixed `normalise` returning relations with attributes in the wrong order in certain cases where `remove_avoidable = TRUE`.
* Fixed `gv` giving Graphviz code that could result in incorrect diagrams: relation and attribute names were converted to lower case, and not checked for uniqueness afterwards. This could result in incorrect foreign key references being drawn. The fix also accounts for a current bug in Graphviz, where edges between HTML-style node ports ignore case for the port labels.

# autodb 1.1.0

* Added a `NEWS.md` file to track changes to the package.

## Improvements

* Added examples for `autodb`, `dfd`, `gv`, and `rejoin`.
* Added a reference for removing avoidable attributes for enhanced third normal
form.
* Changed `decompose` to return an error if the data.frame doesn't satisfy the functional dependencies implied by the schema. This will return an error when using `decompose` with a schema derived from the same data.frame if any approximate dependencies were included. Previously, using `decompose` or `dfd` with approximate dependencies would result in constructing a database with duplicate key values, since there's currently no handling of approximate dependencies during database construction, and records ignored in approximate dependencies were being kept. This is incorrect behaviour; `decompose` will be added back for approximate dependencies once the package can properly handle them.
* Made `reduce` generic, and added a method for database schemas. Currently this method requires explicitly naming the main relations, rather than inferring them.
* Removed incorrect comment in vignette about needing foreign keys to reconstruct the original data frame from a database.
* Tidied up `nudge` data documentation, improved commentary on publication references in vignette.
* Removed accuracy argument from `autodb`, due to approximate dependencies now returning an error in `decompose`.

## Fixes

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
