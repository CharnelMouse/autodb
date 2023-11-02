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
#' Relationships, i.e. foreign key references, are allowed to have different
#' attribute names in the child and parent relations; this can't occur in the
#' output for \code{\link{cross_reference}} and \code{\link{normalise}}.
#'
#' Subsetting removes any relationships that involve removed relation schemas.
#' Removing duplicates with \code{\link{unique}} changes relationships involving
#' duplicates to involve the kept equivalent schemas instead.
#'
#' @param relation_schemas a \code{\link{relation_schema}} object, as returned
#'   by \code{\link{synthesise}} or \code{\link{relation_schema}}.
#' @param relationships a list of relationships, each
#'  represented by a list containing four character elements. In order, the
#'  elements are a scalar giving the name of the child schema, a vector giving
#'  the child attribute names, a scalar giving the name of the parent schema,
#'  and a vector giving the parent attribute names. The vectors must be of the
#'  same length and contain names for attributes present in their respective
#'  schemas, and the parent attributes must form a key, in order.
#'
#' @return A \code{database_schema} object, containing \code{relation_schemas}
#'   with \code{relationships} stored in an attribute of the same name.
#'   Relationships are stored with their attributes in the order they appear in
#'   their respective relation schemas.
#' @export
database_schema <- function(relation_schemas, relationships) {
  # should FKs be made unique?
  if (!inherits(relation_schemas, "relation_schema"))
    stop("relations must be a relation_schema")
  if (!is.list(relationships))
    stop("relationships must be a list")
  if (any(
    lengths(relationships) != 4L |
    !vapply(relationships, is.list, logical(1))
  ))
    stop("relationship elements must be length-four lists")
  if (any(!vapply(
    relationships,
    \(r) all(r[c(1L, 3L)] %in% names(relation_schemas)),
    logical(1)
  ))) {
    stop("relationship relation names must be within relation schema names")
  }
  if (any(!vapply(
    relationships,
    \(r) {
      all(r[[2]] %in% attrs(relation_schemas)[[r[[1]]]]) &&
        all(r[[4]] %in% unlist(keys(relation_schemas)[[r[[3]]]]))
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
    relation_schemas,
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
        rel[[1]], ".{", toString(rel[[2]]),
        "} -> ",
        rel[[3]], ".{", toString(rel[[4]]), "}\n"
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

#' @export
`attrs_order<-.database_schema` <- function(x, ..., value) {
  rels <- subschemas(x)
  attrs_order(rels) <- value
  database_schema(rels, relationships(x))
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

  relationships_list <- lapply(lst, relationships)
  new_relationships <- Map(
    \(rls, old, new) lapply(
      rls,
      \(rl) list(new[match(rl[[1]], old)], rl[[2]], new[match(rl[[3]], old)], rl[[4]])
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

#' @exportS3Method
merge_schemas.database_schema <- function(x, to_remove, merge_into, ...) {
  stopifnot(length(to_remove) == length(merge_into))

  schemas <- merge_schemas.relation_schema(subschemas(x), to_remove, merge_into)

  ind_map <- seq_along(x)
  remaining_inds <- setdiff(ind_map, to_remove)
  ind_map[to_remove] <- merge_into
  ind_map <- seq_along(remaining_inds)[match(ind_map, remaining_inds)]

  old_names <- names(x)
  rels <- unique(lapply(
    relationships(x),
    \(rel) list(
      names(schemas)[ind_map[match(rel[[1]], old_names)]],
      rel[[2]],
      names(schemas)[ind_map[match(rel[[3]], old_names)]],
      rel[[4]]
    )
  ))
  rels <- rels[vapply(
    rels,
    \(r) r[[1]] != r[[3]],
    logical(1)
  )]
  database_schema(schemas, rels)
}

#' @exportS3Method
create.database_schema <- function(x, ...) {
  database(
    create(subschemas(x)),
    relationships(x)
  )
}
