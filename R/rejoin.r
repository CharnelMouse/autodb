rejoin <- function(database) {
  tables <- database$tables
  if (length(tables) == 0)
    return(data.frame())
  if (length(tables) == 1)
    return(tables[[1]]$df)
  relationships <- database$relationships
  while (length(tables) > 1) {
    children <- vapply(relationships, `[`, character(1), 1)
    parents <- vapply(relationships, `[`, character(1), 3)
    non_children <- setdiff(parents, children)
    if (length(non_children) == 0)
      stop("no non-children!")
    non_child <- non_children[1]
    non_child_relationships <- which(vapply(
      relationships,
      \(rel) rel[3] == non_child,
      logical(1)
    ))
    rels <- relationships[non_child_relationships]
    for (r in rels) {
      # remove non-key attributes in parent already present in child,
      # to avoid duplicates
      # remove: parent not-key && child
      # keep: parent key || !child
      parent_attrs <- names(tables[[r[3]]]$df)
      columns_to_include <- parent_attrs[
        parent_attrs %in% unlist(tables[[r[3]]]$keys) |
          !is.element(parent_attrs, names(tables[[r[1]]]$df))
      ]
      tables[[r[1]]]$df <- merge(
        tables[[r[1]]]$df,
        tables[[r[3]]]$df[, columns_to_include, drop = FALSE],
        by.x = r[2],
        by.y = r[4]
      )
      tables[[r[1]]]$parents <- setdiff(tables[[r[1]]]$parents, r[3])
    }
    parent_index <- match(r[3], names(tables))
    tables <- tables[-parent_index]
    relationships <- relationships[-non_child_relationships]
  }
  tables[[1]]$df
}
