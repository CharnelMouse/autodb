reduce <- function(es) {
  table_nrows <- vapply(es$tables, \(x) nrow(x$df), integer(1))
  queue <- names(table_nrows)[table_nrows == max(table_nrows)]
  kept <- character()
  while (length(queue) > 0) {
    current <- queue[1]
    queue <- queue[-1]
    kept <- union(kept, current)
    current_parents <- es$tables[[current]]$parents
    queue <- union(queue, setdiff(current_parents, kept))
  }
  sorted_kept <- kept[order(match(kept, names(es$tables)))]
  es$tables <- es$tables[sorted_kept]
  es$relationships <- Filter(
    \(r) all(is.element(r[c(1, 3)], sorted_kept)),
    es$relationships
  )
  es
}
