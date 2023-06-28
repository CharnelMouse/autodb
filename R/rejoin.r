#' Join a database into a data frame
#'
#' Rejoins the relations in a database. This is the inverse of calling
#' \code{\link{autodb}} with \code{accuracy} set to 1, except that the rows
#' might be returned in a different order.
#'
#' The rejoining algorithm might not use all of the given relations: it begins
#' with the relation with the largest number of records, then joins it with enough
#' relations to contain all of the present attributes. This is not limited to
#' relations that the starting relation is linked to by foreign keys, and is not
#' limited to them either, since in some cases this constraint would make it
#' impossible to rejoin with all of the present attributes.
#'
#' If the database is inconsistent, where the unused relations contain
#' additional information, then the rejoining will be lossy. The algorithm does
#' not check for this consistency violation. This is also the case for rejoining
#' the results of \code{\link{reduce}}.
#'
#' @param database A database containing the data to be rejoined, as returned by
#'   \code{\link{decompose}}.
#'
#' @return A data frame, containing all information contained \code{database} if
#'   it is lossless and self-consistent.
#' @examples
#' # simple example
#' db <- autodb(ChickWeight, "chick")
#' rj <- rejoin(db)
#' rj <- rj[order(as.integer(rownames(rj))), ]
#' mapply(identical, rj, as.data.frame(ChickWeight))
#' @export
rejoin <- function(database) {
  relations <- database$relations
  if (length(relations) == 0)
    return(data.frame())
  if (length(relations) == 1)
    return(relations[[1]]$df[, database$attributes, drop = FALSE])
  attrs <- lapply(relations, \(r) names(r$df))
  attrs_order <- unique(unlist(attrs))
  keys <- lapply(relations, \(r) r$keys)
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
  is_main <- vapply(closure_attrs, setequal, logical(1), attrs_order)
  if (!any(is_main))
    stop("database is not lossless")
  to_merge <- unique(G_relations[closure_usedlists[[which(is_main)[1]]]])
  stopifnot(!is.null(names(is_main)))
  main_relation <- relations[[which(is_main)]]$df
  while (length(to_merge) > 0) {
    mergee <- to_merge[1]
    to_merge <- to_merge[-1]
    mergee_relation <- relations[[mergee]]
    current_attrs <- names(main_relation)
    mergee_attrs <- names(mergee_relation$df)
    key <- Find(\(k) all(is.element(k, current_attrs)), mergee_relation$keys)
    new_attrs <- setdiff(mergee_attrs, current_attrs)
    old_nrow <- nrow(main_relation)
    main_relation <- merge(
      main_relation,
      mergee_relation$df[, c(key, new_attrs), drop = FALSE],
      by = key,
      sort = FALSE
    )
    stopifnot(identical(nrow(main_relation), old_nrow))
  }
  main_relation[, database$attributes, drop = FALSE]
}
