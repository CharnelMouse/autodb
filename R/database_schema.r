#' Database schemas
#'
#' Enhances a \code{\link{relation_schema}} object with foreign key reference
#' information.
#'
#' Unlike \code{\link{functional_dependency}} and \code{\link{relation_schema}},
#' \code{database_schema} is not designed to be vector-like: it only holds a
#' single database schema. This adheres to the usual package use case, where a
#' single data frame is being analysed at a time. However, it inherits from
#' \code{\link{relation_schema}}, so is vectorised with respect to its relation
#' schemas.
#'
#' As with \code{\link{relation_schema}}, duplicate relation schemas, after
#' ordering by attribute, are allowed, and can be removed with
#' \code{\link{unique}}.
#'
#' References, i.e. foreign key references, are allowed to have different
#' attribute names in the child and parent relations; this can't occur in the
#' output for \code{\link{autoref}} and \code{\link{normalise}}.
#'
#' Subsetting removes any references that involve removed relation schemas.
#' Removing duplicates with \code{\link{unique}} changes references involving
#' duplicates to involve the kept equivalent schemas instead.
#'
#' @param relation_schemas a \code{\link{relation_schema}} object, as returned
#'   by \code{\link{synthesise}} or \code{\link{relation_schema}}.
#' @param references a list of references, each
#'  represented by a list containing four character elements. In order, the
#'  elements are a scalar giving the name of the child schema, a vector giving
#'  the child attribute names, a scalar giving the name of the parent schema,
#'  and a vector giving the parent attribute names. The vectors must be of the
#'  same length and contain names for attributes present in their respective
#'  schemas, and the parent attributes must form a key, in order.
#'
#' @return A \code{database_schema} object, containing \code{relation_schemas}
#'   with \code{references} stored in an attribute of the same name.
#'   References are stored with their attributes in the order they appear in
#'   their respective relation schemas.
#' @export
database_schema <- function(relation_schemas, references) {
  # should FKs be made unique?
  if (!inherits(relation_schemas, "relation_schema"))
    stop("relations must be a relation_schema")
  check_valid_reference(references, relation_schemas, "relation schema")

  structure(
    relation_schemas,
    references = references,
    class = c("database_schema", "relation_schema")
  )
}

#' @exportS3Method
references.database_schema <- function(x, ...) {
  attr(x, "references")
}

#' @export
`references<-.database_schema` <- function(x, value) {
  database_schema(subschemas(x), value)
}

#' @export
`attrs_order<-.database_schema` <- function(x, ..., value) {
  rels <- subschemas(x)
  attrs_order(rels) <- value
  database_schema(rels, references(x))
}

#' @exportS3Method
subschemas.database_schema <- function(x, ...) {
  y <- unclass(x)
  attributes(y) <- NULL
  relation_schema(stats::setNames(y, names(x)), attrs_order(x))
}

#' @exportS3Method
create.database_schema <- function(x, ...) {
  database(
    create(subschemas(x)),
    references(x)
  )
}

#' @exportS3Method
unique.database_schema <- function(x, ...) {
  schemas <- subschemas(x)
  dups <- which(duplicated(schemas))
  if (length(dups) == 0L)
    return(x)
  dups_prec <- vapply(
    dups,
    \(n) Position(
      \(rs) {
        identical(unname(attrs(rs)), unname(attrs(schemas[[n]]))) &&
          identical(unname(keys(rs)), unname(keys(schemas[[n]])))
      },
      schemas
    ),
    integer(1)
  )
  merge_schemas(x, dups, dups_prec)
}

#' @exportS3Method
c.database_schema <- function(...) {
  lst <- list(...)
  joined_schemas <- do.call(c, lapply(lst, subschemas))
  names(joined_schemas) <- if (is.null(names(joined_schemas)))
    character(length(joined_schemas))
  else
    make.unique(names(joined_schemas))

  references_list <- lapply(lst, references)
  new_references <- Map(
    rename_reference_referands,
    references_list,
    lapply(lst, names),
    unname(split(
      names(joined_schemas),
      rep(factor(seq_along(lst)), lengths(lst))
    ))
  )
  joined_references <- do.call(c, new_references)

  result_lst <- list(joined_schemas, joined_references)
  do.call(database_schema, result_lst)
}

#' @exportS3Method
merge_schemas.database_schema <- function(x, to_remove, merge_into, ...) {
  stopifnot(length(to_remove) == length(merge_into))
  schemas <- merge_schemas.relation_schema(subschemas(x), to_remove, merge_into)
  rels <- merge_reference_referands(
    references(x),
    to_remove,
    merge_into,
    names(x),
    names(schemas)
  )
  database_schema(schemas, rels)
}

#' @export
`[.database_schema` <- function(x, i) {
  rels <- references(x)
  kept_relation_names <- names(stats::setNames(seq_along(x), names(x))[i])
  kept_rels <- rels[reference_names_element(rels, kept_relation_names)]

  new_schemas <- subschemas(x)[i]
  database_schema(new_schemas, kept_rels)
}

#' @exportS3Method
print.database_schema <- function(x, max = 10, ...) {
  cat("database schema with ")
  print(subschemas(x), max = max, ...)
  print_references(references(x), max)
}
