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

  list(
    attrs = scheme$attrs,
    keys = scheme$keys,
    parents = parents,
    relationships = relationships
  )
}
