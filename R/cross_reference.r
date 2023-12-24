#' Add foreign key relationships to a normalised database
#'
#' @param schema a \code{\link{relation_schema}} object, as given by
#'   \code{\link{synthesise}}.
#'
#' @return A \code{\link{database_schema}} object, containing the given relation
#'   schemas and the created foreign key references.
#' @export
cross_reference <- function(schema) {
  references <- calculate_references(keys(schema), attrs(schema))
  relationships <- Map(
    \(child, parent, attr) list(
      names(schema)[[child]],
      attr,
      names(schema)[[parent]],
      attr
    ),
    references$child,
    references$parent,
    references$attr
  )
  database_schema(schema, relationships)
}

calculate_references <- function(keys, attrs) {
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

  # remove extraneous relationships, i.e. those that skip relations in the
  # hierarchy, and duplicates
  vecs <- list(
    determinant_sets = child_ref_attrs,
    dependents = parent_ref_attrs
  )
  filtered_vecs <- remove_extraneous_dependencies(vecs)

  relation_pairs <- do.call(Map, unname(c(list(c), vecs)))
  filtered_relation_pairs <- do.call(Map, unname(c(list(c), filtered_vecs)))
  kept <- match(filtered_relation_pairs, relation_pairs)
  stopifnot(!anyNA(kept))
  filtered_attrs <- ref_attrs[kept]

  list(
    child = filtered_vecs$determinant_sets,
    parent = filtered_vecs$dependents,
    attr = filtered_attrs
  )
}
