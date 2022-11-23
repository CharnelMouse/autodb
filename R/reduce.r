#' Remove database tables not linked to the main table
#'
#' Filters a database's tables, keeping only those linked to the main table by
#' foreign key references. The main table is considered to be the table with the
#' largest number of rows. Foreign key relationships involving removed tables
#' are also removed.
#'
#' This function is mostly intended for simplifying a database for the purposes
#' of exploration, particularly by examining plots. While the filtering might
#' remove important auxiliary tables, it's also likely to remove any tables
#' based on spurious dependencies, of which some databases can contain many.
#'
#' @param database A database, whose tables are to be filtered.
#'
#' @return A database, with the auxiliary tables and foreign key relationships
#'   removed.
#' @export
reduce <- function(database) {
  table_nrows <- vapply(database$tables, \(x) nrow(x$df), integer(1))
  queue <- names(table_nrows)[table_nrows == max(table_nrows)]
  kept <- character()
  while (length(queue) > 0) {
    current <- queue[1]
    queue <- queue[-1]
    kept <- union(kept, current)
    current_parents <- database$tables[[current]]$parents
    queue <- union(queue, setdiff(current_parents, kept))
  }
  sorted_kept <- kept[order(match(kept, names(database$tables)))]
  database$tables <- database$tables[sorted_kept]
  database$relationships <- Filter(
    \(r) all(is.element(r[c(1, 3)], sorted_kept)),
    database$relationships
  )
  database
}
