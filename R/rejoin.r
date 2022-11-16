#' Join a database into a single flat table
#'
#' Rejoins the tables in a database. This is the inverse of calling
#' \code{\link{autonorm}} with \code{accuracy} set to 1, except that the rows
#' and columns might be returned in a different order.
#'
#' The rejoining algorithm might not use all of the given tables: it begins with
#' the table with the largest number of rows, then joins it with enough tables
#' to contain all of the present attributes. This is not limited to tables that
#' the starting table is linked to by foreign keys, and is not limited to them
#' either, since in some cases this constraint would make it impossible to
#' rejoin with all of the present attributes.
#'
#' If the database is inconsistent, where the unused tables contain additional
#' information, then the rejoining will be lossy. The algorithm does not check
#' for this consistency violation.
#'
#' @param database A database containing the data to be rejoined, as returned by
#'   \code{\link{decompose}}.
#'
#' @return A data frame, containing all information contained \code{database} if
#'   it is lossless and self-consistent.
#' @export
rejoin <- function(database) {
  tables <- database$tables
  if (length(tables) == 0)
    return(data.frame())
  if (length(tables) == 1)
    return(tables[[1]]$df)
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
