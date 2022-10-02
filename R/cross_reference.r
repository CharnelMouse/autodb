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
  references <- calculate_references(indexes, scheme$attrs)

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
      attrs = scheme$attrs,
      keys = scheme$keys,
      parents = parents,
      relationships = relationships
    ),
    class = c("database_scheme", "list")
  )
}

calculate_references <- function(indexes, attrs) {
  # find all links for indexes (should be any candidate key instead)
  child_ref_attrs <- character()
  parent_ref_attrs <- character()
  seq_rel <- seq_along(indexes)
  for (parent in seq_rel) {
    for (child in seq_rel[-parent]) {
      parent_index <- indexes[[parent]]
      if (all(parent_index %in% attrs[[child]])) {
        child_ref_attrs <- c(child_ref_attrs, paste(child, parent_index, sep = "."))
        parent_ref_attrs <- c(parent_ref_attrs, paste(parent, parent_index, sep = "."))
      }
    }
  }

  # remove extraneous relationships, i.e. those that skip tables in the hierarchy
  ref_attrs <- unique(c(child_ref_attrs, parent_ref_attrs))
  ref_vecs <- list(
    determinant_sets = child_ref_attrs,
    dependents = parent_ref_attrs
  ) |>
    convert_to_integer_attributes(ref_attrs) |>
    remove_extraneous_dependencies()
  # convert back to characters
  convert_back <- function(x) {
    ref_attrs[x] |>
      as.character() |>
      strsplit(".", fixed = TRUE)
  }
  filtered_child_ref_attrs <- convert_back(unlist(ref_vecs$determinant_sets))
  filtered_parent_ref_attrs <- convert_back(ref_vecs$dependents)
  rel_inds <- function(x) strtoi(vapply(x, `[[`, character(1), 1))
  rel_attrs <- function(x) vapply(x, `[[`, character(1), 2)
  filtered_attrs <- rel_attrs(filtered_child_ref_attrs)
  stopifnot(identical(filtered_attrs, rel_attrs(filtered_parent_ref_attrs)))

  list(
    child = rel_inds(filtered_child_ref_attrs),
    parent = rel_inds(filtered_parent_ref_attrs),
    attr = filtered_attrs
  )
}
