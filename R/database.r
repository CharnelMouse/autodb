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
  if (!is.list(relationships))
    stop("relationships must be a list")
  if (!is.character(name) || length(name) != 1L)
    stop("name must be a scalar character")
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
`[.database` <- function(x, i) {
  rels <- relationships(x)
  kept_relation_names <- names(stats::setNames(seq_along(x), names(x))[i])
  kept_rels <- rels[vapply(rels, \(r) all(c(r[[1]], r[[3]]) %in% kept_relation_names), logical(1))]

  relations <- relation(unclass(x), attrs_order(x))
  new_relations <- relations[i]
  database(new_relations, kept_rels)
}

#' @export
`attrs_order<-.database` <- function(x, ..., value) {
  database(
    relation(unclass(x), attrs_order = value),
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

#' @exportS3Method
print.database <- function(x, max = 10, ...) {
  n_relations <- length(x)
  cat(paste0(
    "database ",
    name(x),
    " with ",
    n_relations,
    " relation",
    if (n_relations != 1) "s",
    "\n"
  ))
  dfs <- records(x)
  as <- attrs(x)
  ks <- keys(x)
  for (n in seq_len(min(n_relations, max))) {
    rows <- nrow(dfs[[n]])
    cat(paste0(
      "relation ",
      names(x)[n],
      ": ",
      toString(as[[n]]),
      "; ",
      rows,
      " record", if (rows != 1) "s", "\n"
    ))
    keys <- ks[[n]]
    n_keys <- length(keys)
    for (k in seq_len(min(n_keys, max))) {
      cat(paste0("  key ", k, ": ", toString(keys[[k]]), "\n"))
    }
    if (max < n_keys)
      cat("  ... and", n_keys - max, "other keys\n")
  }
  if (max < n_relations) {
    cat("... and", n_relations - max, "other relations\n")
  }
  if (length(relationships(x)) == 0)
    cat("no relationships\n")
  else {
    cat(paste("relationships:\n"))
    n_relationships <- length(relationships(x))
    for (r in seq_len(min(n_relationships, max))) {
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
subrelations.database <- function(x, ...) {
  y <- unclass(x)
  attributes(y) <- NULL
  relation(stats::setNames(y, names(x)), attrs_order(x))
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
