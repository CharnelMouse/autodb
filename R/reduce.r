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
