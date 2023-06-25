#' Add foreign key relationships to a normalised database
#'
#' @param schema a database schema without relationships, as given by
#'   \code{\link{synthesise}}.
#' @inheritParams normalise
#'
#' @return A database schema with relationships, represented by a named list of
#'   three lists and two character vectors, with the first four having equal
#'   length and representing relation schemas:
#'   \itemize{
#'     \item \code{attrs} elements contain the attributes present in the
#'     relation schemas, with attributes in keys given first.
#'     \item \code{keys} elements contain a list of the candidate keys for the
#'     relation schemas.
#'     \item \code{parents} elements contain integers, representing a relation
#'     schema's parent relation schemas by their position in the paired lists.
#'     \item \code{relationships} contains a list of relationships, each
#'     represented by a list containing two elements. In order, the elements
#'     are a two-length integer vector, giving the positions of the child and
#'     parent relation schemas, and a scalar character, giving the name of the
#'     linked attribute in both relation schemas.
#'     \item \code{relation_names} is a character vector, containing the names
#'     of the relation schemas
#'     \item \code{all_attrs} is a character vector, containing all attribute
#'     names in priority order for placement and key ordering, i.e. as ordered
#'     in the original data frame.
#'  }
#' @export
cross_reference <- function(schema, ensure_lossless = TRUE) {
  all_attrs <- all_attrs(schema)
  attrs <- attrs(schema)
  keys <- keys(schema)
  relation_names <- names(schema)

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
      stopifnot(sum(nchar(relation_names) == 0L) <= 1L)
      relation_names[nchar(relation_names) == 0L] <- "empty"
      relation_names <- make.names(relation_names, unique = TRUE)
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
    class = c("database_schema", "list")
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
    child = rep(unlist(filtered_vecs$determinant_sets), lengths(filtered_attrs)),
    parent = rep(filtered_vecs$dependents, lengths(filtered_attrs)),
    attr = unlist(filtered_attrs)
  )
}
