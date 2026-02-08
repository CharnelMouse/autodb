#' Join a database into a data frame
#'
#' Rejoins the relations in a database into a single data frame, if possible.
#' This is the inverse of calling \code{\link{autodb}}, except that the rows
#' might be returned in a different order.
#'
#' The rejoining algorithm might not use all of the given relations: it begins
#' with the relation with the largest number of records, then joins it with enough
#' relations to contain all of the present attributes. This is not limited to
#' relations that the starting relation is linked to by foreign keys, and is not
#' limited to them either, since in some cases this constraint would make it
#' impossible to rejoin with all of the present attributes.
#'
#' Since the algorithm may not use all of the given relations, the algorithm may
#' ignore some types of database inconsistency, where different relations hold
#' data inconsistent with each other. In this case, the rejoining will be lossy.
#' Rejoining the results of \code{\link{reduce}} can also be lossy.
#'
#' Due to the above issues, the algorithm will be changed to use all of the
#' relations in the future.
#'
#' Not all databases can be represented as a single data frame. A simple example
#' is any database where the same attribute name is used for several difference
#' sources of data, since rejoining results in inappropriate merges.
#'
#' @param database A database containing the data to be rejoined, as returned by
#'   \code{\link{decompose}}.
#'
#' @return A data frame, containing all information contained \code{database} if
#'   it is lossless and self-consistent.
#' @examples
#' # simple example
#' db <- autodb(ChickWeight)
#' rj <- rejoin(db)
#' rj <- rj[order(as.integer(rownames(rj))), ]
#' all(rj == ChickWeight) # TRUE
#'
#' # showing rejoin() doesn't check for inconsistency:
#' # add another Chick table with the diets swapped
#' db2 <- db[c(1, 2, 1)]
#' records(db2)[[3]]$Diet <- rev(records(db2)[[3]]$Diet)
#' rj2 <- rejoin(db2)
#' rj2 <- rj2[order(as.integer(rownames(rj2))), ]
#' all(rj2 == ChickWeight) # TRUE
#' @export
rejoin <- function(database) {
  keys <- keys(database)
  attrs_order <- attrs_order(database)
  attrs <- attrs(database)
  missing <- setdiff(attrs_order, unlist(attrs))
  if (length(missing) > 0)
    stop(
      "database is not lossless: ",
      "attributes in attrs_order not present in relations\n",
      toString(missing)
    )
  if (length(database) == 0)
    return(data.frame())
  if (length(database) == 1)
    return(records(database)[[1]][, attrs_order, drop = FALSE])

  G <- synthesised_fds(attrs, keys)

  # calculate closures for relations if we merge in other relations where a key
  # can be determined (not just via foreign key references), and order in which
  # to merge things in
  G_flattened <- unlist(G, recursive = FALSE, use.names = FALSE)
  G_det_sets <- lapply(G_flattened, `[[`, 1)
  G_deps <- vapply(G_flattened, `[[`, character(1), 2)
  closures <- lapply(
    lapply(attrs, match, attrs_order),
    find_closure_with_used,
    detset_matrix(lapply(G_det_sets, match, attrs_order), length(attrs_order)),
    match(G_deps, attrs_order)
  )

  # find relations whose closure contains everything, i.e. can merge in
  # "children" to contain all attributes
  closure_attrs <- lapply(closures, \(x) attrs_order[x[[1]]])
  is_main <- vapply(closure_attrs, setequal, logical(1), attrs_order)
  if (!any(is_main)) {
    sorted_closure_attrs <- unique(lapply(
      closure_attrs,
      \(x) x[order(match(x, attrs_order))]
    ))
    subsets <- outer(
      sorted_closure_attrs,
      sorted_closure_attrs,
      Vectorize(\(x, y) !setequal(x, y) && setequal(union(x, y), y))
    )
    best_merges <- sorted_closure_attrs[apply(subsets, 1, Negate(any))]
    stop(
      "database can not be fully rejoined\nbest joined sets:\n",
      paste(vapply(best_merges, toString, character(1)), collapse = "\n")
    )
  }

  # merge everything into a "main" relation, in closure order
  G_relations <- rep(seq_along(attrs), lengths(G))
  closure_usedlists <- lapply(closures, `[[`, 2)
  to_merge <- unique(G_relations[closure_usedlists[[which(is_main)[[1]]]]])
  stopifnot(!is.null(names(is_main)))
  main_relation <- records(database)[[which(is_main)[[1]]]]
  r_dfs <- records(database)
  while (length(to_merge) > 0) {
    mergee <- to_merge[1]
    to_merge <- to_merge[-1]
    mergee_df <- r_dfs[[mergee]]
    mergee_keys <- keys[[mergee]]
    current_attrs <- names(main_relation)
    mergee_attrs <- names(mergee_df)
    key <- Find(\(k) all(is.element(k, current_attrs)), mergee_keys)
    new_attrs <- setdiff(mergee_attrs, current_attrs)
    old_nrow <- nrow(main_relation)
    # unique() needed here in case floating-point values cause duplicates in
    # merge
    main_relation <- df_unique(df_join(
      main_relation,
      mergee_df[, c(key, new_attrs), drop = FALSE],
      by = key
    ))
    stopifnot(identical(nrow(main_relation), old_nrow))
  }

  main_relation[, attrs_order, drop = FALSE]
}
