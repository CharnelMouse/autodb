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

#' Dependents
#'
#' Generic function, with the only given method fetching dependents for
#' functional dependencies.
#'
#' @param x an R object. For the given method, a
#'   \code{\link{functional_dependency}}.
#' @param ... further arguments passed on to methods.
#'
#' @return A character vector containing dependents.
#' @export
dependent <- function(x, ...) {
  UseMethod("dependent")
}

#' Relational data attributes
#'
#' Generic function, with the only given method fetching attribute sets for
#' functional dependencies.
#'
#' @param x an R object. For the given method, a
#'   \code{\link{functional_dependency}}.
#' @param ... further arguments passed on to methods.
#'
#' @return A character vector containing an attribute set, with unique elements.
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

#' Relational data keys
#'
#' Generic function, with the only given method fetching candidate key lists for
#' relation schemas.
#'
#' @param x an R object. For the given method, a \code{\link{relation_schema}}.
#' @param ... further arguments passed on to methods.
#'
#' @return A list containing lists of unique character vectors, representing
#'   candidate keys for each schema.
#' @export
keys <- function(x, ...) {
  UseMethod("keys")
}

#' Relational data attribute order
#'
#' Generic function, with the only given method fetching attribute order for
#' relation schemas.
#'
#' @param x an R object. For the given method, a \code{\link{relation_schema}}.
#' @param ... further arguments passed on to methods.
#'
#' @return A character vector, giving attributes in the order in which they're
#'   prioritised for sorting attributes and keys in the schema.
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

#' Name
#'
#' @param x an R object.
#' @param ... further arguments passed on to methods.
#'
#' @export
name <- function(x, ...) {
  UseMethod("name")
}

#' Schema relationships
#'
#' Generic function, returning present relationships.
#'
#' @param x an R object.
#' @param ... further arguments passed on to methods.
#'
#' @return a list, giving relationships.
#' @export
relationships <- function(x, ...) {
  UseMethod("relationships")
}

#' @rdname relationships
#'
#' @param value A list, of the same length as \code{relationships}(x, ...).
#'
#' @export
`relationships<-` <- function(x, value) {
  UseMethod("relationships<-")
}

#' Schema subschemas
#'
#' Generic function, returning subschemas for \code{x}.
#'
#' @param x an R object, intended to be some sort of schema that contains other
#'   schemas.
#' @param ... further arguments passed on to methods.
#'
#' @return a schema-type object, or a list of schema-type objects if the
#'   subschema isn't vectorised.
#' @export
subschemas <- function(x, ...) {
  UseMethod("subschemas")
}

#' Database subrelations
#'
#' Generic function, returning subrelations for \code{x}.
#'
#' @param x an R object, intended to be some sort of database-like object that
#'   contains relations.
#' @param ... further arguments passed on to methods.
#'
#' @return a relation-type object, or a list of relation-type objects if the
#'   subrelation isn't vectorised.
#' @export
subrelations <- function(x, ...) {
  UseMethod("subrelations")
}

merge_attribute_orderings <- function(...) {
  ordered_sets <- list(...)
  # Combining attributes pairwise can't ensure preservation of consistency, so
  # we only add an attribute to the joined list when it's the next one in all
  # lists containing it.
  n_sets <- length(ordered_sets)

  if (n_sets >= 2L) {
    pairs <- utils::combn(seq_len(n_sets), 2)
    if (Position(
      \(n) {
        as1 <- pairs[[1, n]]
        as2 <- pairs[[2, n]]
        is.unsorted(match(as1, as2), na.rm = TRUE) ||
          is.unsorted(match(as2, as1), na.rm = TRUE)
      },
      seq_len(ncol(pairs)),
      nomatch = 0L
    )) {
      warning(paste(
        "pairwise-inconsistent attribute orderings,",
        "returning attributes in order of listing"
      ))
      return(Reduce(union, ordered_sets))
    }
  }

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
#' Generic function, merging relation schemas with empty keys. The remaining
#' such schema contains all attributes contained in such schemas.
#'
#' For \code{\link{database_schema}} objects, relationships involving the
#' schemas with empty keys are updated to refer to the merged schema.
#'
#' @param x an R object.
#'
#' @return an R object of the same class as \code{x}, where relations with an
#'   empty key have been merged into a single relation.
#' @export
merge_empty_keys <- function(x) {
  UseMethod("merge_empty_keys")
}

#' @exportS3Method
merge_empty_keys.database_schema <- function(x) {
  schemas <- subschemas(x)
  rels <- relationships(x)
  empty_keys <- which(vapply(
    keys(schemas),
    identical,
    logical(1),
    list(character())
  ))
  if (length(empty_keys) >= 2L) {
    as <- unique(unlist(attrs(schemas[empty_keys])))
    to_keep <- empty_keys[[1]]
    to_remove <- empty_keys[-1]
    result_lst <- remove_schemas(
      schemas,
      rels,
      to_remove,
      rep(to_keep, length(to_remove))
    )
    schemas <- result_lst[[1]]
    rels <- result_lst[[2]]
    attrs(schemas)[[to_keep]] <- as
  }
  database_schema(schemas, rels)
}

#' @exportS3Method
merge_empty_keys.relation_schema <- function(x) {
  empty_keys <- which(vapply(
    keys(x),
    identical,
    logical(1),
    list(character())
  ))
  if (length(empty_keys) >= 2L) {
    as <- unique(unlist(attrs(x[empty_keys])))
    to_keep <- empty_keys[[1]]
    to_remove <- empty_keys[-1]
    attrs(x)[[to_keep]] <- as
    x <- x[-to_remove]
  }
  x
}

#' Create instance of a schema
#'
#' @param x an R object, representing the schema to create an instance of.
#' @param ... further arguments passed on to methods.
#'
#' @return an instance of the schema. For example, calling \code{create} on a
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
#' an object element do not. This makes insertion equivalent to an `INSERT OR
#' IGNORE` expression in SQL. In particular, it is somewhat like using this
#' expression in SQLite, since that implementation uses dynamic typing.
#'
#' @param x an R object, into which to insert data.
#' @param vals a data frame, containing data to insert.
#' @param ... further arguments pass on to methods.
#'
#' @return an R object of the same class as \code{x}, containing the additional
#'   new data.
#' @export
insert <- function(x, vals, ...) {
  UseMethod("insert")
}
