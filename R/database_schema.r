#' Database schemas
#'
#' Enhances a \code{relation_schema} object with foreign key reference
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
#' Relationships, i.e. foreign key references, are allowed to have different
#' attribute names in the child and parent relations; this can't occur in the
#' output for \code{\link{cross_reference}} and \code{\link{normalise}}.
#'
#' Subsetting removes any relationships that involve removed relation schemas.
#' Removing duplicates with \code{\link{unique}} changes relationships involving
#' duplicates to involve the kept equivalent schemas instead.
#'
#' @param relations a \code{relation_schema} object, as returned by
#'   \code{\link{synthesise}} or \code{\link{relation_schema}}.
#' @param relationships a list, whose elements are length-four character
#'   vectors, giving the names of the referencing relation, the referencing
#'   attribute, the referenced relation, and the reference attribute,
#'   respectively.
#'
#' @return A database schema with relationships, represented by a named list of
#'   three lists and two character vectors, with the first four having equal
#'   length and representing relation schemas:
#'   \itemize{
#'     \item \code{attrs} elements contain the attributes present in the
#'     relation schemas, with attributes in keys given first.
#'     \item \code{keys} elements contain a list of the candidate keys for the
#'     relation schemas.
#'     \item \code{relationships} contains a list of relationships, each
#'     represented by a list containing two elements. In order, the elements
#'     are a two-length integer vector, giving the positions of the child and
#'     parent relation schemas, and a scalar character, giving the name of the
#'     linked attribute in both relation schemas.
#'     \item \code{relation_names} is a character vector, containing the names
#'     of the relation schemas
#'     \item \code{attrs_order} is a character vector, containing all attribute
#'     names in priority order for placement and key ordering, i.e. as ordered
#'     in the original data frame.
#'  }
#' @export
database_schema <- function(relations, relationships) {
  # should FKs be made unique?
  if (!inherits(relations, "relation_schema"))
    stop("relations must be a relation_schema")
  if (!is.list(relationships))
    stop("relationships must be a list")
  if (any(
    lengths(relationships) != 4L |
    !vapply(relationships, is.character, logical(1))
  ))
    stop("relationship elements must be length-four characters")
  if (any(!vapply(
    relationships,
    \(r) all(r[c(1L, 3L)] %in% names(relations)),
    logical(1)
  ))) {
    stop("relationship relation names must be within relation schema names")
  }
  if (any(!vapply(
    relationships,
    \(r) {
      r[[2]] %in% attrs(relations)[[r[[1]]]] &&
        r[[4]] %in% unlist(keys(relations)[[r[[3]]]])
    },
    logical(1)
  )))
    stop("relationship attributes must be within referer's attributes and referee's keys")
  if (any(vapply(
    relationships,
    \(r) r[[1]] == r[[3]],
    logical(1)
  )))
    stop("relationship cannot be from a relation's attribute to itself")

  structure(
    relations,
    relationships = relationships,
    class = c("database_schema", "relation_schema")
  )
}

#' @exportS3Method
print.database_schema <- function(x, max = 10, ...) {
  n_relations <- length(attrs(x))
  cat(paste0(
    "database schema with ",
    n_relations,
    " relation schema",
    if (n_relations != 1) "s",
    "\n"
  ))

  cat(with_number(length(attrs_order(x)), "attribute", "", "s"))
  if (length(attrs_order(x)) > 0L)
    cat(":", toString(attrs_order(x)))
  cat("\n")

  for (n in seq_len(min(n_relations, max))) {
    cat(paste0("schema ", names(x)[n], ": ", toString(attrs(x)[[n]]), "\n"))
    keys <- keys(x)[[n]]
    n_keys <- length(keys)
    for (k in seq_len(min(n_keys, max))) {
      cat(paste0("  key ", k, ": ", toString(keys[[k]]), "\n"))
    }
    if (max < n_keys)
      cat("  ... and", n_keys - max, "other keys\n")
  }
  if (max < n_relations) {
    cat("... and", n_relations - max, "other schemas\n")
  }
  if (length(relationships(x)) == 0)
    cat("no relationships\n")
  else {
    cat(paste("relationships:\n"))
    n_relationships <- length(relationships(x))
    for (r in seq_len(n_relationships)) {
      rel <- relationships(x)[[r]]
      cat(paste0(
        rel[[1]], ".", rel[[2]],
        " -> ",
        rel[[3]], ".", rel[[4]], "\n"
      ))
    }
    if (max < n_relationships)
      cat("... and", n_relationships - max, "other relationships\n")
  }
}

#' @exportS3Method
relationships.database_schema <- function(x, ...) {
  attr(x, "relationships")
}

#' @export
`relationships<-.database_schema` <- function(x, value) {
  database_schema(subschemas(x), value)
}

#' @exportS3Method
subschemas.database_schema <- function(x, ...) {
  relation_schema(unclass(x), attrs_order(x))
}

#' @export
`[.database_schema` <- function(x, i) {
  rels <- relationships(x)
  kept_indices <- seq_along(x)[i]
  kept_rels <- rels[vapply(rels, \(r) all(r[[1]] %in% kept_indices), logical(1))]
  new_rels <- lapply(kept_rels, \(r) list(match(r[[1]], kept_indices), r[[2]]))
  new_schemas <- subschemas(x)[i]
  database_schema(new_schemas, new_rels)
}

#' @exportS3Method
unique.database_schema <- function(x, ...) {
  schemas <- subschemas(x)
  rels <- relationships(x)
  dups <- which(duplicated(schemas))
  if (length(dups) > 0L) {
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
    result_lst <- remove_schemas(schemas, rels, dups, dups_prec)
    schemas <- result_lst[[1]]
    rels <- unique(result_lst[[2]])
  }
  rels <- rels[vapply(rels, \(r) r[[1]] != r[[3]], logical(1))]
  database_schema(schemas, rels)
}

#' @exportS3Method
c.database_schema <- function(...) {
  lst <- list(...)
  joined_schemas <- do.call(c, lapply(lst, subschemas))
  names(joined_schemas) <- if (is.null(names(joined_schemas)))
    character(length(joined_schemas))
  else
    make.unique(names(joined_schemas))

  relationships_list <- lapply(lst, relationships)
  new_relationships <- Map(
    \(rls, old, new) lapply(
      rls,
      \(rl) c(new[match(rl[[1]], old)], rl[[2]], new[match(rl[[3]], old)], rl[[4]])
    ),
    relationships_list,
    lapply(lst, names),
    unname(split(
      names(joined_schemas),
      rep(factor(seq_along(lst)), lengths(lst))
    ))
  )
  joined_relationships <- do.call(c, new_relationships)

  result_lst <- list(joined_schemas, joined_relationships)
  do.call(database_schema, result_lst)
}

remove_schemas <- function(schemas, rels, to_remove, replace_with) {
  remaining_inds <- setdiff(seq_along(schemas), to_remove)
  ind_map <- seq_along(schemas)
  ind_map[to_remove] <- replace_with
  ind_map <- seq_along(remaining_inds)[match(ind_map, remaining_inds)]

  old_names <- names(schemas)
  schemas <- schemas[-to_remove]
  rels <- lapply(
    rels,
    \(rel) c(
      names(schemas)[ind_map[match(rel[[1]], old_names)]],
      rel[[2]],
      names(schemas)[ind_map[match(rel[[3]], old_names)]],
      rel[[4]]
    )
  )
  list(schemas, rels)
}

#' @exportS3Method
create.database_schema <- function(x, ...) {
  database(
    create(subschemas(x)),
    relationships(x)
  )
}