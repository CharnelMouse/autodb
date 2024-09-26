#' Add foreign key references to a normalised database
#'
#' @param schema a \code{\link{relation_schema}} object, as given by
#'   \code{\link{synthesise}}.
#' @inheritParams autodb
#'
#' @return A \code{\link{database_schema}} object, containing the given relation
#'   schemas and the created foreign key references.
#' @export
#' @examples
#' rs <- relation_schema(
#'   list(
#'     a_b_c = list(c("a", "b", "c", "d"), list(c("a", "b", "c"))),
#'     a_b = list(c("a", "b", "d"), list(c("a", "b"), c("b", "d")))
#'   ),
#'   letters[1:4]
#' )
#' autoref(rs, single_ref = FALSE)
#' autoref(rs, single_ref = TRUE)
autoref <- function(schema, single_ref = FALSE) {
  references <- calculate_references(schema, single_ref = single_ref)
  references <- Map(
    \(child, parent, attr) list(
      child,
      attr,
      parent,
      attr
    ),
    references$child,
    references$parent,
    references$attr
  )
  database_schema(schema, references)
}

calculate_references <- function(schema, single_ref = FALSE) {
  keys <- keys(schema)
  attrs <- attrs(schema)
  # find all links for indexes (should be any candidate key instead)
  child_ref_attrs <- integer()
  parent_ref_attrs <- integer()
  ref_attrs <- list()
  seq_rel <- seq_along(keys)
  for (parent in seq_rel) {
    for (child in seq_rel[-parent]) {
      for (key in seq_along(keys[[parent]])[lengths(keys[[parent]]) > 0L]) {
        parent_key <- keys[[parent]][[key]]
        if (all(parent_key %in% attrs[[child]])) {
          child_ref_attrs <- c(child_ref_attrs, child)
          parent_ref_attrs <- c(parent_ref_attrs, parent)
          ref_attrs <- c(ref_attrs, list(parent_key))
          if (single_ref)
            break
        }
      }
    }
  }

  # remove extraneous references, i.e. those that skip relations in the
  # hierarchy, and duplicates
  # we do this by abusing the remove_extraneous_dependencies function
  # for functional dependencies
  fds <- functional_dependency(
    Map(list, names(schema)[child_ref_attrs], names(schema)[parent_ref_attrs]) |>
      unname(),
    names(schema),
    unique = FALSE
  )
  unique_fds <- unique(fds)
  unique_fd_indices <- match(fds, unique_fds)
  filtered_fds <- remove_extraneous_dependencies(unique_fds)
  filtered_vecs <- list(
    determinant_sets = detset(filtered_fds),
    dependants = dependant(filtered_fds)
  )

  kept <- is.element(unique_fds, filtered_fds)
  fds_kept <- kept[unique_fd_indices]
  kept_indices <- match(fds[fds_kept], filtered_fds)
  stopifnot(!anyNA(fds_kept))
  filtered_attrs <- ref_attrs[fds_kept]

  list(
    child = filtered_vecs$determinant_sets[kept_indices],
    parent = filtered_vecs$dependants[kept_indices],
    attr = filtered_attrs
  )
}
