# autodb (development version)

## Improvements

* Added `format` and `as.data.frame` methods for `functional_dependency`, `relation_schema`, `database_schema`, `relation`, and `database`. This allows them to be columns in a data frame at initial construction. I'm not sure *why* you'd want to put them a a data frame column, but it's consistent with the idea that the objects from these classes should mostly be treatable as vectors. Be warned: they don't currently work in tibbles.
* Added an `as.character` method for `functional_dependency`. The optional `align_arrows` argument can add padding to one side, in order to make the arrows align when they're printed to different lines. These options are used to align arrows in its `print` method, and its `format` method for when printed as a data frame column.
* Added `==` and `!=` implementations for `functional_dependency`. These ignore differences in `attrs_order`: differently-ordered determinant sets are considered equal.
* Added a `dependants` argument to `discover`, which limit the functional dependency search to those with a dependant in the given set of column names, defaulting to all of them. This should significantly speed up searches where only some dependants are of interest.
* Added a `detset_limit` argument for `discover`/`autodb`, which limits the FD search to only look for dependencies with the determinant set size under a given limit. For DFD, this usually doesn't significantly reduce the search time, but it won't make it worse. It will be useful once other search algorithms are implemented.

## Fixes

* Fixed `gv` to account for Graphviz HTML-like labels requiring certain characters, namely the set "<>&, to be escaped in Graphviz HTML-like labels, and removed completely in attribute values.
* Fixed `df_equiv` to properly handle data frames with zero columns or duplicate rows.

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
