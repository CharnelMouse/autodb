#' Remove database relations not linked to the main relation
#'
#' Filters a database's relations, keeping only those linked to the main
#' relation by foreign key references. The main relations is considered to be
#' the relation with the largest number of records. Foreign key relationships
#' involving removed relations are also removed.
#'
#' This function is mostly intended for simplifying a database for the purposes
#' of exploration, particularly by examining plots. While the filtering might
#' remove important auxiliary relations, it's also likely to remove any based on
#' spurious dependencies, of which some databases can contain many.
#'
#' Using \code{\link{rejoin}} on the database resulting from \code{reduce} is
#' likely to fail or return incomplete results.
#'
#' @param database A database, whose relations are to be filtered.
#'
#' @return A database, with the auxiliary relations and foreign key
#'   relationships removed.
#' @export
reduce <- function(database) {
  relation_nrows <- vapply(database$relations, \(x) nrow(x$df), integer(1))
  queue <- names(relation_nrows)[relation_nrows == max(relation_nrows)]
  kept <- character()
  while (length(queue) > 0) {
    current <- queue[1]
    queue <- queue[-1]
    kept <- union(kept, current)
    current_parents <- database$relations[[current]]$parents
    queue <- union(queue, setdiff(current_parents, kept))
  }
  sorted_kept <- kept[order(match(kept, names(database$relations)))]
  database$relations <- database$relations[sorted_kept]
  database$relationships <- Filter(
    \(r) all(is.element(r[c(1, 3)], sorted_kept)),
    database$relationships
  )
  database
}
