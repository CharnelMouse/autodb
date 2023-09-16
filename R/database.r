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

  structure(
    relations,
    name = name,
    relationships = relationships,
    class = c("database", "relation", "list")
  )
}

#' @export
`attrs_order<-.database` <- function(x, ..., value) {
  database(
    relation(x, attrs_order = value),
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
  for (n in seq_len(min(n_relations, max))) {
    rows <- nrow(x[[n]]$df)
    cat(paste0(
      "relation ",
      names(x)[n],
      ": ",
      toString(names(x[[n]]$df)),
      "; ",
      rows,
      " record", if (rows != 1) "s", "\n"
    ))
    keys <- x[[n]]$keys
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
