#' Add foreign key relationships to a normalised database
#'
#' @param scheme a database scheme without relationships, as given by
#'   \code{\link{normalise}}.
#' @param ensure_lossless a logical, TRUE by default. If TRUE, and the
#'   decomposition isn't lossless, an extra relation is added to make the
#'   decomposition lossless. This relation becomes the ultimate child table.
#'
#' @return A database scheme with relationships, represented by a named list of
#'   four lists, with the first three having equal length:
#'   \itemize{
#'     \item \code{attrs} elements contain the attributes present in the
#'     relation schemes, with attributes in keys given first.
#'     \item \code{keys} elements contain a list of the candidate keys for the
#'     relation schemes.
#'     \item \code{parents} elements contain integers, representing a relation
#'     scheme's parent relation schemes by their position in the paired lists.
#'     \item \code{relationships} contains a list of relationships, each
#'     represented by a list containing two elements. In order, the elements
#'     are a two-length integer vector, giving the positions of the child and
#'     parent relation schemes, and a scalar character, giving the name of the
#'     linked attribute in both relation schemes.
#'  }
#' @export
cross_reference <- function(scheme, ensure_lossless = TRUE) {
  all_attrs <- scheme$all_attrs
  attrs <- scheme$attrs
  keys <- scheme$keys
  relation_names <- scheme$relation_names

  if (ensure_lossless) {
    G <- synthesised_fds(attrs, keys)
    G_det_sets <- lapply(unlist(G, recursive = FALSE), `[[`, 1)
    G_deps <- vapply(unlist(G, recursive = FALSE), `[[`, character(1), 2)
    primaries <- lapply(keys, `[[`, 1)
    closures <- lapply(primaries, find_closure, G_det_sets, G_deps)
    if (!any(vapply(closures, setequal, logical(1), all_attrs))) {
      new_key <- minimal_subset(all_attrs, all_attrs, G_det_sets, G_deps)
      attrs <- c(attrs, list(new_key))
      keys <- c(keys, list(list(new_key)))
      relation_names <- c(relation_names, paste(new_key, collapse = "_"))
    }
  }

  references <- calculate_references(keys, attrs)

  n_relations <- length(attrs)
  parents <- replicate(n_relations, integer())
  for (n in seq_len(n_relations)) {
    parents[[n]] <- sort(unique(references$parent[references$child == n]))
  }

  relationships <- Map(
    \(child, parent, attr) list(c(child, parent), attr),
    references$child,
    references$parent,
    references$attr
  )

  structure(
    list(
      attrs = attrs,
      keys = keys,
      parents = parents,
      relationships = relationships,
      relation_names = relation_names,
      all_attrs = all_attrs
    ),
    class = c("database_scheme", "list")
  )
}

calculate_references <- function(keys, attrs) {
  # find all links for indexes (should be any candidate key instead)
  child_ref_attrs <- integer()
  parent_ref_attrs <- integer()
  ref_attrs <- list()
  seq_rel <- seq_along(keys)
  for (parent in seq_rel) {
    for (child in seq_rel[-parent]) {
      for (key in seq_along(keys[[parent]])) {
        parent_key <- keys[[parent]][[key]]
        if (all(parent_key %in% attrs[[child]])) {
          child_ref_attrs <- c(child_ref_attrs, child)
          parent_ref_attrs <- c(parent_ref_attrs, parent)
          ref_attrs <- c(ref_attrs, list(parent_key))
          break
        }
      }
    }
  }

  # remove extraneous relationships, i.e. those that skip tables in the
  # hierarchy, and duplicates
  vecs <- list(
    determinant_sets = child_ref_attrs,
    dependents = parent_ref_attrs
  )
  filtered_vecs <- remove_extraneous_dependencies(vecs)

  table_pairs <- do.call(Map, c(list(c), vecs))
  filtered_table_pairs <- do.call(Map, c(list(c), filtered_vecs))
  kept <- match(filtered_table_pairs, table_pairs)
  stopifnot(!anyNA(kept))
  filtered_attrs <- ref_attrs[kept]

  list(
    child = rep(unlist(filtered_vecs$determinant_sets), lengths(filtered_attrs)),
    parent = rep(filtered_vecs$dependents, lengths(filtered_attrs)),
    attr = unlist(filtered_attrs)
  )
}
