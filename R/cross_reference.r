#' Add foreign key relationships to a normalised database
#'
#' @param schema a \code{\link{relation_schema}} object, as given by
#'   \code{\link{synthesise}}.
#' @inheritParams normalise
#'
#' @return A \code{\link{database_schema}} object, containing the given relation
#'   schemas and the created foreign key references.
#' @export
cross_reference <- function(schema, ensure_lossless = TRUE) {
  attrs_order <- attrs_order(schema)
  attrs <- attrs(schema)
  keys <- keys(schema)
  relation_names <- names(schema)

  if (ensure_lossless) {
    G <- synthesised_fds(attrs, keys)
    G_det_sets <- lapply(unlist(G, recursive = FALSE), `[[`, 1)
    G_deps <- vapply(unlist(G, recursive = FALSE), `[[`, character(1), 2)
    primaries <- lapply(keys, `[[`, 1)
    closures <- lapply(primaries, find_closure, G_det_sets, G_deps)
    if (!any(vapply(closures, setequal, logical(1), attrs_order))) {
      new_key <- minimal_subset(attrs_order, attrs_order, G_det_sets, G_deps)
      attrs <- c(attrs, list(new_key))
      keys <- c(keys, list(list(new_key)))
      new_name <- paste(new_key, collapse = "_")
      if (nchar(new_name) == 0L)
        new_name <- "constants"
      relation_names <- c(relation_names, new_name)
      stopifnot(sum(nchar(relation_names) == 0L) <= 1L)
      relation_names[nchar(relation_names) == 0L] <- "empty"
      relation_names <- make.names(relation_names, unique = TRUE)
      schema <- c(
        schema,
        relation_schema(
          stats::setNames(
            list(list(new_key, list(new_key))),
            relation_names[length(relation_names)]
          ),
          attrs_order
        )
      )
    }
  }

  references <- calculate_references(keys, attrs)

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
