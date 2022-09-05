#' Add foreign key relationships to a normalised database
#'
#' @param scheme a database scheme without relationships, as given by
#'   \code{\link{normalise}}.
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
cross_reference <- function(scheme) {
  n_relations <- length(scheme$attrs)

  indexes <- lapply(scheme$keys, `[[`, 1)
  relation_indices <- seq_len(n_relations)

  reference_mat <- calculate_reference_matrix(indexes, scheme$attrs)
  parents <- replicate(n_relations, integer())
  for (n in seq_len(n_relations)) {
    refs <- reference_mat[n, ]
    ref_indices <- relation_indices[setdiff(which(refs), n)]
    parents[[n]] <- ref_indices
  }

  stack <- seq_len(n_relations)
  relationships <- list()
  while (length(stack) > 0) {
    current_relation <- stack[1]
    child_attrs <- scheme$attrs[[current_relation]]
    stack <- stack[-1]

    for (parent_index in parents[[current_relation]]) {
      parent_keys <- scheme$keys[[parent_index]]
      relationships <- c(
        relationships,
        lapply(
          intersect(child_attrs, unlist(parent_keys)),
          \(attr) list(c(current_relation, parent_index), attr)
        )
      )
    }
  }

  structure(
    list(
      attrs = scheme$attrs,
      keys = scheme$keys,
      parents = parents,
      relationships = relationships
    ),
    class = c("database_scheme", "list")
  )
}

calculate_reference_matrix <- function(indexes, attrs) {
  ref_detsets <- list()
  ref_deps <- character()
  seq_rel <- seq_along(indexes)
  for (n in seq_rel) {
    for (m in seq_rel[-n]) {
      ind <- indexes[[n]]
      if (all(ind %in% attrs[[m]])) {
        ref_detsets <- c(ref_detsets, paste(m, ind, sep = "."))
        ref_deps <- c(ref_deps, paste(n, ind, sep = "."))
      }
    }
  }

  ref_attrs <- unique(c(unlist(ref_detsets), unlist(ref_deps)))
  ref_vecs <- list(determinant_sets = ref_detsets, dependents = ref_deps) |>
    convert_to_integer_attributes(ref_attrs) |>
    remove_extraneous_dependencies()
  # convert back to characters
  convert_back <- function(x) {
    ref_attrs[x] |>
      as.character() |>
      strsplit(".", fixed = TRUE)
  }
  filtered_determinant_sets <- convert_back(unlist(ref_vecs$determinant_sets))
  filtered_dependents <- convert_back(ref_vecs$dependents)
  rel_inds <- function(x) strtoi(vapply(x, `[[`, character(1), 1))
  rel_attrs <- function(x) vapply(x, `[[`, character(1), 2)
  filtered_children <- rel_inds(filtered_determinant_sets)
  filtered_parents <- rel_inds(filtered_dependents)
  filtered_attrs <- rel_attrs(filtered_determinant_sets)
  stopifnot(identical(filtered_attrs, rel_attrs(filtered_dependents)))

  unique_ref_splits <- !duplicated(Map(c, filtered_children, filtered_parents))
  unique_ref_children <- filtered_children[unique_ref_splits]
  unique_ref_parents <- filtered_parents[unique_ref_splits]
  ref_order <- order(unique_ref_children, unique_ref_parents)
  children <- unique_ref_children[ref_order]
  parents <- unique_ref_parents[ref_order]

  n_tables <- length(indexes)
  reference_mat <- matrix(FALSE, nrow = n_tables, ncol = n_tables)
  for (ref in seq_along(ref_order)) {
    reference_mat[children[ref], parents[ref]] <- TRUE
  }

  reference_mat
}
