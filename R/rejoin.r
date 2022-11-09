rejoin <- function(database) {
  tables <- database$tables
  if (length(tables) == 0)
    return(data.frame())
  if (length(tables) == 1)
    return(tables[[1]]$df)
  relationships <- database$relationships
  attrs <- lapply(tables, \(tb) names(tb$df))
  all_attrs <- unique(unlist(attrs))
  keys <- lapply(tables, \(tb) tb$keys)
  G <- synthesised_fds(attrs, keys)
  G_det_sets <- lapply(unlist(G, recursive = FALSE), `[[`, 1)
  G_deps <- vapply(unlist(G, recursive = FALSE), `[[`, character(1), 2)
  G_relations <- rep(seq_along(attrs), lengths(G))
  closures <- lapply(
    attrs,
    find_closure_with_used,
    G_det_sets,
    G_deps
  )
  closure_attrs <- lapply(closures, `[[`, 1)
  closure_usedlists <- lapply(closures, `[[`, 2)
  is_main <- vapply(closure_attrs, setequal, logical(1), all_attrs)
  if (!any(is_main))
    stop("database is not lossless")
  to_merge <- unique(G_relations[closure_usedlists[[which(is_main)[1]]]])
  stopifnot(!is.null(names(is_main)))
  main_table <- tables[[which(is_main)]]$df
  while (length(to_merge) > 0) {
    mergee <- to_merge[1]
    to_merge <- to_merge[-1]
    mergee_relation <- tables[[mergee]]
    current_attrs <- names(main_table)
    mergee_attrs <- names(mergee_relation$df)
    key <- Find(\(k) all(is.element(k, current_attrs)), mergee_relation$keys)
    new_attrs <- setdiff(mergee_attrs, current_attrs)
    old_nrow <- nrow(main_table)
    main_table <- merge(
      main_table,
      mergee_relation$df[, c(key, new_attrs), drop = FALSE],
      by = key,
      sort = FALSE
    )
    stopifnot(identical(nrow(main_table), old_nrow))
  }
  main_table
}
