#' Remove relations not linked to the main relations
#'
#' Filters an object's relations, keeping only the main relations, and those
#' considered ancestors via foreign key references. Foreign key
#' relationships involving removed relations are also removed.
#'
#' Details on how the main tables are chosen are given in individual methods.
#'
#' This function is mostly intended for simplifying a database, or a database
#' schema, for the purposes of exploration, particularly by examining plots.
#' While the filtering might remove important auxiliary relations, it's also
#' likely to remove any based on spurious dependencies, of which some databases
#' can contain many.
#'
#' @param x An object whose relations are to be filtered.
#' @param ... further arguments passed to or from other methods.
#'
#' @return An object of the same class as \code{x}, with the auxiliary relations
#'   and foreign key relationships removed.
#' @export
reduce <- function(x, ...) {
  UseMethod("reduce", x)
}

#' Remove database relations not linked to the main relations
#'
#' Filters a database's relations, keeping only the main relations, and those
#' considered ancestors via foreign key references. Foreign
#' key relationships involving removed relations are also removed.
#'
#' The main relations are considered to be the relations with the largest number
#' of records.
#'
#' Using \code{\link{rejoin}} on the database resulting from \code{reduce} is
#' likely to fail or return incomplete results.
#'
#' @param x A database, whose relations are to be filtered.
#' @inheritParams reduce
#'
#' @return A database, with the auxiliary relations and foreign key
#'   relationships removed.
#' @exportS3Method
reduce.database <- function(x, ...) {
  relation_nrows <- vapply(x$relations, \(x) nrow(x$df), integer(1))
  queue <- names(relation_nrows)[relation_nrows == max(relation_nrows)]
  kept <- character()
  while (length(queue) > 0) {
    current <- queue[1]
    queue <- queue[-1]
    kept <- union(kept, current)
    current_parents <- x$relations[[current]]$parents
    queue <- union(queue, setdiff(current_parents, kept))
  }
  sorted_kept <- kept[order(match(kept, names(x$relations)))]
  x$relations <- x$relations[sorted_kept]
  x$relationships <- Filter(
    \(r) all(is.element(r[c(1, 3)], sorted_kept)),
    x$relationships
  )
  x
}

#' Remove database schema relations not linked to the given relations
#'
#' Filters a database schema's relations, keeping only the given relations, and
#' those considered ancestors via foreign key references. Foreign key
#' relationships involving removed relations are also removed.
#'
#' This method takes a given set of main relations, rather than inferring them.
#'
#' Using \code{\link{rejoin}} on the database resulting from decomposing a data
#' frame with the reduced schema is likely to fail or return incomplete results.
#'
#' @param x A database schema, whose relations are to be filtered.
#' @param main A character vector, containing names of relations to be
#'   considered as the "main" relations.
#' @inheritParams reduce
#'
#' @return A database schema, with the auxiliary relations and foreign key
#'   relationships removed.
#' @exportS3Method
reduce.database_schema <- function(x, main, ...) {
  main_indices <- match(main, x$relation_names)
  if (anyNA(main_indices))
    stop(
      "main contains names for relations not present: ",
      toString(main[is.na(main_indices)])
    )
  queue <- main_indices
  kept <- integer()
  while (length(queue) > 0) {
    current <- queue[1]
    queue <- queue[-1]
    kept <- union(kept, current)
    current_parents <- x$parents[[current]]
    queue <- union(queue, setdiff(current_parents, kept))
  }
  sorted_kept <- sort(kept)
  x$attrs <- x$attrs[sorted_kept]
  x$keys <- x$keys[sorted_kept]
  x$parents <- lapply(x$parents[sorted_kept], \(p) match(p, sorted_kept))
  x$relation_names <- x$relation_names[sorted_kept]
  x$relationships <- Filter(
    \(r) all(is.element(r[[1]], sorted_kept)),
    x$relationships
  )
  x$relationships <- Filter(
    \(r) is.element(r[[2]], x$relation_names),
    x$relationships
  )
  x$relationships <- lapply(
    x$relationships,
    \(r) {
      r[[1]] <- match(r[[1]], sorted_kept)
      r
    }
  )
  x
}
