#' Add foreign key references to a normalised database
#'
#' Adds foreign key references to a \code{\link{relation_schema}} object
#' automatically, replacing any existing references.
#'
#' The method for generating references is simple. First, it finds every link
#' between two relation schemas, where the parent contains all the attributes in
#' one of the child's keys. This can be done separately for all of the child's
#' keys, so there can be multiple links with the same parent and child if
#' \code{single_ref} is \code{TRUE}.
#'
#' Second, any transitive references are removed: if there are link relation
#' pairs a -> b, b -> c, and a -> c, then the latter is transitive, and so is
#' removed. If there is a cyclic reference, e.g. where c -> a, then the choice
#' of which link to remove is arbitrary. Cycles cannot occur in sets of relation
#' schemas resulting from decomposing a single table.
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
  remove_extraneous_references(
    child_ref_attrs,
    parent_ref_attrs,
    ref_attrs,
    schema
  )
}

remove_extraneous_references <- function(
  child_ref_attrs,
  parent_ref_attrs,
  ref_attrs,
  schema
) {
  # Easy approach is to plug the references into remove_extraneous_dependencies
  # as FDs, but that's slow, because it handles a more general case. Here,
  # all "determinants" have size one, so we can more simply represent the
  # problem with a reference matrix, with empty diagonal elements. We can
  # repeatedly remove any one pairing that also appears in the square of the
  # matrix.
  # Re-calculating the square, and which pairs are redundant, is also
  # somewhat expensive, so instead we modify only the parts affected by the
  # removed pair.
  pairs <- Map(c, child_ref_attrs, parent_ref_attrs)
  unique_pairs <- unique(pairs)
  unique_pair_indices <- match(pairs, unique_pairs)
  omat <- matrix(0L, nrow = length(schema), ncol = length(schema))
  for (pair in unique_pairs) {
    omat[[pair[[1]], pair[[2]]]] <- 1L
  }
  mat <- omat
  sq <- mat %*% mat
  redundant <- which(mat & sq, arr.ind = TRUE)
  while ((nr <- nrow(redundant)) > 0) {
    # last element in redundant is last by column (parent), then by row (child),
    # so try to remove last-ordered pair first
    child <- redundant[[nr, "row"]]
    parent <- redundant[[nr, "col"]]

    sq[child, ] <- sq[child, ] - mat[parent, ]
    sq[, parent] <- sq[, parent] - mat[, child]
    # no need to re-increment sq[c, p],
    # since mat[p, p] and mat[c, c] are always zero (no self-references)

    redundant <- redundant[-nr, , drop = FALSE]
    removed_children <- mat[parent, ] == 1 & sq[child, ] == 0
    removed_parents <- mat[, child] == 1 & sq[, parent] == 0
    redundant <- redundant[
      !removed_children[redundant[, "row"]] | redundant[, "col"] != parent,
      ,
      drop = FALSE
    ]
    redundant <- redundant[
      redundant[, "row"] != child | !removed_parents[redundant[, "col"]],
      ,
      drop = FALSE
    ]

    mat[[child, parent]] <- 0L
  }
  rem <- which(mat == 1, arr.ind = TRUE)
  filtered_pairs <- Map(c, rem[, "row"], rem[, "col"])
  kept_alt <- is.element(unique_pairs, filtered_pairs)

  pairs_kept <- kept_alt[unique_pair_indices]
  kept_indices_alt <- match(pairs[pairs_kept], filtered_pairs)
  stopifnot(!anyNA(pairs_kept))
  filtered_attrs_alt <- ref_attrs[pairs_kept]

  list(
    child = as.list(names(schema)[rem[, "row"]][kept_indices_alt]),
    parent = names(schema)[rem[, "col"]][kept_indices_alt],
    attr = filtered_attrs_alt
  )
}
