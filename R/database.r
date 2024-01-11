#' Databases
#'
#' Enhances a \code{\link{relation}} object with foreign key reference
#' information.
#'
#' Unlike \code{\link{relation_schema}} and \code{link{relation}}, and like
#' \code{\link{database_schema}}, \code{database} is not designed to be
#' vector-like: it only holds a single database. This adheres to the usual
#' package use case, where a single data frame is being analysed at a time.
#' However, it inherits from \code{\link{relation}}, so is vectorised with
#' respect to its relations.
#'
#' As with \code{\link{relation}}, duplicate relations, after ordering by
#' attribute, are allowed, and can be removed with \code{\link{unique}}.
#'
#' Relationships, i.e. foreign key references, are allowed to have different
#' attribute names in the child and parent relations; this can't occur in the
#' output for \code{\link{cross_reference}} and \code{\link{normalise}}.
#'
#' Subsetting removes any relationships that involve removed relations.
#' Removing duplicates with \code{\link{unique}} changes relationships involving
#' duplicates to involve the kept equivalent relations instead.
#'
#' @inheritParams database_schema
#' @inheritParams autodb
#' @param relations a \code{\link{relation}} object.
#'
#' @return A \code{database} object, containing \code{relations} with
#'   \code{relationships} stored in an attribute of the same name. Relationships
#'   are stored with their attributes in the order they appear in their
#'   respective relations.
#' @export
database <- function(relations, relationships, name = NA_character_) {
  if (!inherits(relations, "relation"))
    stop("relations must be a relation")
  if (!is.character(name) || length(name) != 1L)
    stop("name must be a scalar character")
  check_valid_reference(relationships, relations, "relation")

  relat_errors <- Filter(
    \(relat) {
      referrer <- unique(records(relations)[[relat[[1]]]][, relat[[2]], drop = FALSE])
      referee <- unique(records(relations)[[relat[[3]]]][, relat[[4]], drop = FALSE])
      !identical(
        unname(lapply(referrer, class)),
        unname(lapply(referee, class))
      ) ||
        !identical(
        nrow(merge(referrer, referee, by.x = relat[[2]], by.y = relat[[4]])),
        nrow(referrer)
      )
    },
    relationships
  )
  if (length(relat_errors) > 0)
    stop(paste0(
      "relations must satisfy relationships in schema:\n",
      paste(
        vapply(
          relat_errors,
          \(relat) paste0(
            relat[[1]], ".{", toString(relat[[2]]),
            "} -> ",
            relat[[3]], ".{", toString(relat[[4]]), "}"
          ),
          character(1)
        ),
        collapse = "\n"
      )
    ))

  structure(
    relations,
    name = name,
    relationships = relationships,
    class = c("database", "relation", "list")
  )
}

#' @export
`attrs_order<-.database` <- function(x, ..., value) {
  rels <- subrelations(x)
  attrs_order(rels) <- value
  database(
    rels,
    relationships = relationships(x),
    name = name(x)
  )
}

#' @exportS3Method
name.database <- function(x, ...) {
  attr(x, "name")
}

#' @exportS3Method
relationships.database <- function(x, ...) {
  attr(x, "relationships")
}

#' @export
`relationships<-.database` <- function(x, value) {
  database(subrelations(x), value)
}

#' @exportS3Method
subrelations.database <- function(x, ...) {
  y <- unclass(x)
  attributes(y) <- NULL
  relation(stats::setNames(y, names(x)), attrs_order(x))
}

#' @exportS3Method
unique.database <- function(x, ...) {
  relations <- subrelations(x)
  dups <- which(duplicated(relations))
  if (length(dups) == 0L)
    return(x)
  y <- sort_records(relations)
  dups_prec <- vapply(
    dups,
    \(n) Position(
      \(rs) {
        identical(unname(records(rs)), unname(records(y[[n]]))) &&
          identical(unname(keys(rs)), unname(keys(y[[n]])))
      },
      y
    ),
    integer(1)
  )
  merge_database_relations(x, dups, dups_prec)
}

merge_database_relations <- function(x, to_remove, merge_into, ...) {
  stopifnot(length(to_remove) == length(merge_into))
  relations <- merge_relations(subrelations(x), to_remove, merge_into)
  rels <- merge_reference_referands(
    relationships(x),
    to_remove,
    merge_into,
    names(x),
    names(relations)
  )
  database(relations, rels)
}

merge_relations <- function(x, to_remove, merge_into, ...) {
  stopifnot(length(to_remove) == length(merge_into))

  for (n in seq_along(to_remove)[to_remove != merge_into]) {
    stopifnot(identical(
      unname(keys(x[[to_remove[[n]]]])),
      unname(keys(x[[merge_into[[n]]]]))
    ))
    stopifnot(identical(
      unname(attrs(x[[to_remove[[n]]]])),
      unname(attrs(x[[merge_into[[n]]]]))
    ))
  }

  x[-to_remove]
}

#' @exportS3Method
c.database <- function(...) {
  lst <- list(...)
  joined_rels <- do.call(c, lapply(lst, subrelations))
  names(joined_rels) <- if (is.null(names(joined_rels)))
    character(length(joined_rels))
  else
    make.unique(names(joined_rels))

  relationships_list <- lapply(lst, relationships)
  new_relationships <- Map(
    rename_reference_referands,
    relationships_list,
    lapply(lst, names),
    unname(split(
      names(joined_rels),
      rep(factor(seq_along(lst)), lengths(lst))
    ))
  )
  joined_relationships <- do.call(c, new_relationships)

  result_lst <- list(joined_rels, joined_relationships)
  do.call(database, result_lst)
}

#' @exportS3Method
insert.database <- function(x, vals, ...) {
  res <- insert.relation(x, vals, ...)
  dfs <- records(res)
  relationship_checks <- relationships(res)[vapply(
    relationships(res),
    \(relat) {
      referrer <- unique(dfs[[relat[[1]]]][, relat[[2]], drop = FALSE])
      referee <- unique(dfs[[relat[[3]]]][, relat[[4]], drop = FALSE])
      !identical(
        nrow(merge(referrer, referee, by.x = relat[[2]], by.y = relat[[4]])),
        nrow(referrer)
      )
    },
    logical(1)
  )]
  if (length(relationship_checks)) {
    error_strings <- vapply(
      relationship_checks,
      \(relat) paste0(
        relat[[1]], ".{", toString(relat[[2]]),
        "} -> ",
        relat[[3]], ".{", toString(relat[[4]]), "}"
      ),
      character(1)
    )
    stop(
      "insertion violates ",
      with_number(length(error_strings), "relationship", "", "s"),
      ":\n",
      paste(error_strings, collapse = "\n")
    )
  }
  res
}

#' @export
`[.database` <- function(x, i) {
  rels <- relationships(x)
  kept_relation_names <- names(stats::setNames(seq_along(x), names(x))[i])
  kept_rels <- rels[reference_names_element(rels, kept_relation_names)]

  new_relations <- subrelations(x)[i]
  database(new_relations, kept_rels)
}

#' @exportS3Method
print.database <- function(x, max = 10, ...) {
  cat(paste0("database ", name(x), " with "))
  print(subrelations(x), max = max, ...)
  print_references(relationships(x), max)
}
