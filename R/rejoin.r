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
    # group rels from same child together for simultaneous merging
    non_child_children <- vapply(rels, `[`, character(1), 1)
    for (child in unique(non_child_children)) {
      # remove non-linking attributes in parent already present in child,
      # to avoid duplicates
      rs <- rels[non_child_children == child]
      ref_attrs <- vapply(rs, `[`, character(1), 2)
      parent_attrs <- names(tables[[non_child]]$df)
      columns_to_include <- parent_attrs[
        parent_attrs %in% ref_attrs |
          !is.element(parent_attrs, names(tables[[child]]$df))
      ]
      tables[[child]]$df <- merge(
        tables[[child]]$df,
        tables[[non_child]]$df[, columns_to_include, drop = FALSE],
        by = ref_attrs,
        sort = FALSE
      )
      tables[[child]]$parents <- setdiff(tables[[child]]$parents, non_child)
    }
    parent_index <- match(non_child, names(tables))
    tables <- tables[-parent_index]
    relationships <- relationships[-non_child_relationships]
  }
  tables[[1]]$df
}
