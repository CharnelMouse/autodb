#' Determinant sets
#'
#' Generic function, with the only given method fetching determinant sets for
#' functional dependencies.
#'
#' @param x an R object. For the given method, a
#'   \code{\link{functional_dependency}}.
#' @param ... further arguments passed on to methods.
#'
#' @return A list containing determinant sets, each consisting of a character
#'   vector with unique elements.
#' @export
detset <- function(x, ...) {
  UseMethod("detset")
}

#' @rdname detset
#'
#' @param value A character vector of the same length as \code{detset(x, ...)}.
#'
#' @export
`detset<-` <- function(x, ..., value) {
  UseMethod("detset<-")
}

#' Dependants
#'
#' Generic function, with the only given method fetching dependants for
#' functional dependencies.
#'
#' @param x an R object. For the given method, a
#'   \code{\link{functional_dependency}}.
#' @param ... further arguments passed on to methods.
#'
#' @return A character vector containing dependants.
#' @export
dependant <- function(x, ...) {
  UseMethod("dependant")
}

#' @rdname dependant
#'
#' @param value A character vector of the same length as \code{dependant(x, ...)}.
#'
#' @export
`dependant<-` <- function(x, ..., value) {
  UseMethod("dependant<-")
}

#' Relational data attributes
#'
#' Generic function, for fetching attribute sets for elements of a relational
#' object.
#'
#' @param x a relational schema object, such as a \code{\link{relation_schema}}
#'   or \code{\link{database_schema}} object, or a relational data object, such
#'   as a \code{\link{relation}} or \code{\link{database}} object.
#' @param ... further arguments passed on to methods.
#'
#' @return A list, containing a character vector for each element of \code{x}.
#' @export
attrs <- function(x, ...) {
  UseMethod("attrs")
}

#' @rdname attrs
#'
#' @param value A character vector of the same length as \code{attrs(x, ...)}.
#'
#' @export
`attrs<-` <- function(x, ..., value) {
  UseMethod("attrs<-")
}

#' Rename relational data attributes
#'
#' Generic function, for renaming attributes present in a database-like
#' structure.
#'
#' This function has a different intended use to re-assigning
#' \code{\link{attrs_order}}: that is intended only for rearranging the order of
#' the attributes, without renaming them. This is intended for renaming the
#' attributes without re-ordering them.
#'
#' @param x an object with an \code{attrs_order} attribute. This includes
#'   relational schema objects, such as a \code{\link{relation_schema}} or
#'   \code{\link{database_schema}} object, relational data objects, such as a
#'   \code{\link{relation}} or \code{\link{database}} object, and
#'   \code{\link{functional_dependency}} objects.
#' @param names a character vector of the same length as \code{attrs_order(x)},
#'   with no duplicated elements, to be used as the new attribute names.
#' @param ... further arguments passed on to methods.
#'
#' @return A relational object of the same type as \code{x}, with attributes
#'   renamed consistently across the whole object.
#' @export
rename_attrs <- function(x, names, ...) {
  UseMethod("rename_attrs")
}

#' Relational data keys
#'
#' Generic function, with the only given method fetching candidate key lists for
#' relation schemas.
#'
#' @param x a relational schema object, such as a \code{\link{relation_schema}}
#'   or \code{\link{database_schema}} object, or a relational data object, such
#'   as a \code{\link{relation}} or \code{\link{database}} object.
#' @param ... further arguments passed on to methods.
#'
#' @return A list containing lists of unique character vectors, representing
#'   candidate keys for each element of \code{x}.
#' @export
keys <- function(x, ...) {
  UseMethod("keys")
}

#' @rdname keys
#'
#' @param value A list of lists of character vectors, of the same length as
#'   \code{keys(x, ...)}. The number of keys for an element of \code{x} can be
#'   changed.
#'
#' @export
`keys<-` <- function(x, ..., value) {
  UseMethod("keys<-")
}

#' Relational data records
#'
#' Generic function, for retrieving data contained in a database-like structure.
#' In particular, this is intended for such structures where the individual
#' relations can't be accessed with subsetting.
#'
#' Since the relational data objects in \code{autodb}, \code{\link{relation}}
#' and \code{\link{database}}, have subsetting methods that return relational
#' data objects, the data contained within them can't be accessed by subsetting.
#' This function is intended for accessing it instead.
#'
#' It's recommended to call \code{records} before doing any subsetting, since
#' subsetting on a relation data object does more work that will be thrown away,
#' such as subsetting on a \code{\link{database}} checking whether foreign key
#' references should be removed.
#'
#' @param x a relational data object, such as a \code{\link{relation}} or
#'   \code{\link{database}} object.
#' @param ... further arguments passed on to methods.
#'
#' @return A list containing data frames, with elements named for their
#'   respective relations.
#' @export
#' @examples
#' db <- autodb(ChickWeight)
#' records(db) # data for Chick and Time_Chick relations
#'
#' # ways to get data for subsets
#' records(db)[c(1, 2)]
#' records(db)[[1]]
#' records(db)$Chick
#'
#' # subsetting first isn't recommended: removes foreign key
#' # reference as mentions, and you need to subset again anyway
#' records(db[[1]])[[1]]
records <- function(x, ...) {
  UseMethod("records")
}

#' @rdname records
#'
#' @param value A list of data frames of the same length as \code{records(x,
#'   ...)}, where each data frame has the same column names as that which it
#'   will replace, in the same order.
#'
#' @export
`records<-` <- function(x, ..., value) {
  UseMethod("records<-")
}

#' Relational data attribute order
#'
#' Generic function, fetching attribute order for relational objects.
#'
#' All classes in \code{autodb} contain an \code{attrs_order} attribute. It
#' gives an easy way to find a list of all attributes/variables involved in an
#' object, but its main purpose is to also assign those attributes a consistent
#' order when printing or plotting the object.
#'
#' @param x an R object, such as a \code{\link{functional_dependency}},
#'   \code{\link{relation_schema}}, \code{\link{relation}},
#'   \code{\link{database_schema}}, or \code{\link{database}} object.
#' @param ... further arguments passed on to methods.
#'
#' @return A character vector, giving attributes in the order in which they're
#'   prioritised for sorting within \code{x}.
#' @export
attrs_order <- function(x, ...) {
  UseMethod("attrs_order")
}

#' @rdname attrs_order
#'
#' @param value A character vector of the same length as \code{attrs_order(x, ...)}.
#'
#' @export
`attrs_order<-` <- function(x, ..., value) {
  UseMethod("attrs_order<-")
}

#' Schema references
#'
#' Generic function, returning present (foreign key) references.
#'
#' @param x an R object with references, such as a \code{\link{database_schema}}
#'   or \code{\link{database}} object.
#' @param ... further arguments passed on to methods.
#'
#' @return A list, giving references.
#' @export
references <- function(x, ...) {
  UseMethod("references")
}

#' @rdname references
#'
#' @param value A list, of the same length as \code{references}(x, ...).
#'
#' @export
`references<-` <- function(x, value) {
  UseMethod("references<-")
}

#' Schema subschemas
#'
#' Generic function, returning subschemas for \code{x}.
#'
#' @param x an R object, intended to be some sort of schema that contains other
#'   schemas, such as a \code{\link{database_schema}} object.
#' @param ... further arguments passed on to methods.
#'
#' @return A schema-type object, or a list of schema-type objects if the
#'   subschema isn't vectorised. For example, if \code{x} is a
#'   \code{\link{database_schema}}, the result is the contained
#'   \code{\link{relation_schema}}.
#' @export
subschemas <- function(x, ...) {
  UseMethod("subschemas")
}

#' Database subrelations
#'
#' Generic function, returning subrelations for \code{x}.
#'
#' @param x an R object, intended to be some sort of database-like object that
#'   contains relations, such as a \code{\link{database}} object.
#' @param ... further arguments passed on to methods.
#'
#' @return A relation-type object, or a list of relation-type objects if the
#'   subrelation isn't vectorised. For example, if \code{x} is a
#'   \code{\link{database}}, the result is the contained \code{\link{relation}}.
#' @export
subrelations <- function(x, ...) {
  UseMethod("subrelations")
}

merge_attribute_orderings <- function(...) {
  ordered_sets <- list(...)
  # Combining attributes pairwise can't ensure preservation of consistency, so
  # we only add an attribute to the joined list when it's the next one in all
  # lists containing it.
  attrs_order <- unique(unlist(ordered_sets))
  indices <- outer(attrs_order, ordered_sets, Vectorize(match))
  merged <- character()
  while (any(!is.na(indices))) {
    maxs <- apply(indices, 1, max, na.rm = TRUE)
    top <- which(maxs == 1)
    if (length(top) == 0L) {
      warning(paste(
        "inconsistent attribute orderings,",
        "returning remaining attributes in order of listing"
      ))
      return(union(merged, attrs_order))
    }
    nxt <- top[[1L]]
    merged <- c(merged, attrs_order[[nxt]])
    nxt_sets <- !is.na(indices[nxt, ])
    indices[, nxt_sets] <- indices[, nxt_sets] - 1L
    indices <- indices[-nxt, , drop = FALSE]
    attrs_order <- attrs_order[-nxt]
  }
  merged
}

#' Merge relation schemas with empty keys
#'
#' Merges an object's schemas with empty keys. The remaining such schema
#' contains all attributes contained in such schemas.
#'
#' This function is not itself generic, but makes use of the generic functions
#' \code{\link{keys}} and \code{\link{merge_schemas}}. Any input class with
#' valid methods for these generic functions can be passed into this function.
#'
#' For \code{\link{database_schema}} objects, references involving the
#' schemas with empty keys are updated to refer to the merged schema.
#'
#' @param x a relational schema object, such as a \code{\link{relation_schema}}
#'   or \code{\link{database_schema}} object.
#'
#' @return An R object of the same class as \code{x}, where relations with an
#'   empty key have been merged into a single relation.
#' @export
#' @seealso \code{\link{merge_schemas}}, on which this function is based.
merge_empty_keys <- function(x) {
  empty_keys <- which(vapply(
    keys(x),
    identical,
    logical(1),
    list(character())
  ))
  if (length(empty_keys) < 2L)
    return(x)
  merge_schemas(
    x,
    empty_keys[-1],
    rep(empty_keys[[1]], length(empty_keys) - 1L)
  )
}

#' Merge relation schemas in given pairs
#'
#' Generic function that merges pairs of an object's schemas with matching sets
#' of keys. The remaining schemas contain all the attributes from the schemas
#' merged into them.
#'
#' @param x a relational schema object, such as a \code{\link{relation_schema}}
#'   or \code{\link{database_schema}} object.
#' @param to_remove an integer vector, giving the indices for schemas to be
#'   merged into other schemas, then removed.
#' @param merge_into an integer vector of the same length as \code{to_remove},
#'   giving the indices for the schemas into which to merge.
#' @param ... further arguments passed on to methods.
#'
#' @return An R object of the same class as \code{x}, where the relations have
#'   been merged as indicated.
#' @export
#' @seealso \code{\link{merge_empty_keys}}, which is based on this function.
#' @examples
#' rs <- relation_schema(
#'   list(
#'     a = list(c("a", "b"), list("a")),
#'     b = list(c("b", "c"), list("b")),
#'     b.1 = list(c("b", "d"), list("b")),
#'     d = list(c("d", "e"), list("d", "e"))
#'   ),
#'   letters[1:5]
#' )
#' ds <- database_schema(
#'   rs,
#'   list(
#'     list("a", "b", "b", "b"),
#'     list("b.1", "d", "d", "d")
#'    )
#' )
#' merge_schemas(rs, 3, 2) # merging b and b.1
#' merge_schemas(ds, 3, 2) # also merging their references
#'
#' # merging a schema into itself just removes it
#' merge_schemas(rs, 3, 3)
#' merge_schemas(ds, 3, 3)
merge_schemas <- function(x, to_remove, merge_into, ...) {
  UseMethod("merge_schemas")
}

#' Create instance of a schema
#'
#' Create a relation data object, using the given relational schema object, with
#' the resulting relations empty and ready for data insertion using
#' \code{\link{insert}}.
#'
#' @param x a relational schema object, representing the schema to create an
#'   instance of, such as a \code{\link{relation_schema}} or
#'   \code{\link{database_schema}} object.
#' @param ... further arguments passed on to methods.
#'
#' @return An instance of the schema. For example, calling \code{create} on a
#'   \code{\link{database_schema}} creates a \code{\link{database}}, where all
#'   the relations contain zero records.
#' @export
create <- function(x, ...) {
  UseMethod("create")
}

#' Insert data
#'
#' Generic function for inserting a data frame of data into an object.
#'
#' This function is intended for inserting into an object that is itself
#' comprised of data frames, such as a \code{\link{relation}} or a
#' \code{\link{database}}. The given methods have the following behaviour:
#' \itemize{
#'   \item If an empty set of data is inserted, into a non-empty object element,
#'   nothing happens.
#'   \item If an empty set of data is inserted into an empty object element, the
#'   resulting element is also empty, but takes on the attribute/column classes
#'   of the inserted data. This is done to prevent having to know attribute
#'   classes during object creation.
#'   \item Insertion can fail if inserting would violate object constraints. For
#'   example, databases cannot have data inserted that would violate
#'   candidate/foreign key constraints.
#'   \item For other cases, the data is inserted in an object element in the
#'   same way as using \code{\link{rbind}}, followed by \code{\link{unique}}.
#' }
#'
#' While key violations prevent insertion, re-insertion of existing records in
#' an object element does not. This makes insertion equivalent to an \code{INSERT OR
#' IGNORE} expression in SQL. In particular, it is somewhat like using this
#' expression in SQLite, since that implementation uses dynamic typing.
#'
#' If \code{vals} contains attributes not included in
#' \code{\link{attrs_order}(x)}, \code{insert} throws an error, since those
#' attributes can't be inserted.
#'
#' If a partial set of attributes is inserted, and \code{all} is \code{FALSE},
#' then data is only inserted into components of \code{x[relations]} whose
#' required attributes are all present in \code{vals}. If \code{all} is
#' \code{TRUE}, \code{insert} returns an error instead. This is useful when
#' specifying \code{relations}: in that case, you often intend to insert
#' into all of the specified elements, so not including all the required
#' attributes is a mistake, and \code{all = TRUE} prevents it.
#'
#' If \code{all} is \code{TRUE}, \code{insert}
#' throws an error in this case: This ensures you insert into all members of a
#' specified value of \code{relations}.
#'
#' @param x a relational data object, into which to insert data, such as a
#'   \code{\link{relation}} or \code{\link{database}} object.
#' @param vals a data frame, containing data to insert. Column names must be
#'   unique.
#' @param relations a character vector, containing names of elements of \code{x}
#'   into which to insert data. By default, \code{insert} attempts to insert
#'   data into every element.
#' @param all a logical, indicating whether \code{vals} is required to contain
#'   all attributes of all elements of \code{x[relations]}. By default, it is
#'   not, and data is only inserted into elements of \code{x[relations]} whose
#'   attributes are all present in \code{vals}.
#' @param keep_rownames a logical or a string, indicating whether to include the
#'   row names as a column. If a string is given, it is used as the name for the
#'   column, otherwise the column is named "row". Set to FALSE by default.
#' @param digits a positive integer, indicating how many significant digits are
#'   to be used for numeric and complex variables. A value of \code{NA} results
#'   in no rounding. By default, this uses \code{getOption("digits")}, similarly
#'   to \code{\link{format}}. See the "Floating-point variables" section for
#'   \code{\link{discover}} for why this rounding is necessary for consistent
#'   results across different machines. See the note in
#'   \code{\link{print.default}} about \code{digits >= 16}.
#' @param ... further arguments pass on to methods.
#'
#' @return An R object of the same class as \code{x}, containing the additional
#'   new data.
#' @export
insert <- function(
  x,
  vals,
  relations = names(x),
  all = FALSE,
  keep_rownames = FALSE,
  digits = getOption("digits"),
  ...
) {
  UseMethod("insert")
}

# for checking subset assignment (`[<-` etc.) takes an object of the same class
check_reassignment_same_class <- function(value, x) {
  if (!identical(class(value), class(x)))
    stop("value must also be a ", class(x)[[1]], " object")
}
