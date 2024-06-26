#' Add foreign key references to a normalised database
#'
#' @param schema a \code{\link{relation_schema}} object, as given by
#'   \code{\link{synthesise}}.
#'
#' @return A \code{\link{database_schema}} object, containing the given relation
#'   schemas and the created foreign key references.
#' @export
autoref <- function(schema) {
  references <- calculate_references(schema)
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

calculate_references <- function(schema) {
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
    Map(list, names(schema)[child_ref_attrs], names(schema)[parent_ref_attrs]),
    names(schema)
  )
  filtered_fds <- remove_extraneous_dependencies(fds)
  filtered_vecs <- list(
    determinant_sets = detset(filtered_fds),
    dependants = dependant(filtered_fds)
  )

  relation_pairs <- fds
  filtered_relation_pairs <- filtered_fds
  kept <- match(filtered_relation_pairs, relation_pairs)
  stopifnot(!anyNA(kept))
  filtered_attrs <- ref_attrs[kept]

  list(
    child = filtered_vecs$determinant_sets,
    parent = filtered_vecs$dependants,
    attr = filtered_attrs
  )
}
