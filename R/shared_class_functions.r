#' Determinant sets
#'
#' Generic function, with the only given method fetching determinant sets for
#' functional dependencies.
#'
#' @param x an R object. For the given method, a
#'   \code{\link{functional_dependency}}.
#' @param ... further arguments passed on to methods.
#'
#' @return A list containing determinant sets, each consisting of a character
#'   vector with unique elements.
#' @export
detset <- function(x, ...) {
  UseMethod("detset")
}

#' Dependents
#'
#' Generic function, with the only given method fetching dependents for
#' functional dependencies.
#'
#' @param x an R object. For the given method, a
#'   \code{\link{functional_dependency}}.
#' @param ... further arguments passed on to methods.
#'
#' @return A character vector containing dependents.
#' @export
dependent <- function(x, ...) {
  UseMethod("dependent")
}

#' Relational data attributes
#'
#' Generic function, with the only given method fetching attribute sets for
#' functional dependencies.
#'
#' @param x an R object. For the given method, a
#'   \code{\link{functional_dependency}}.
#' @param ... further arguments passed on to methods.
#'
#' @return A character vector containing an attribute set, with unique elements.
#' @export
attrs <- function(x, ...) {
  UseMethod("attrs")
}

#' Relational data keys
#'
#' Generic function, with the only given method fetching candidate key lists for
#' relation schemas.
#'
#' @param x an R object. For the given method, a \code{\link{relation_schema}}.
#' @param ... further arguments passed on to methods.
#'
#' @return A list containing lists of unique character vectors, representing
#'   candidate keys for each schema.
#' @export
keys <- function(x, ...) {
  UseMethod("keys")
}

#' Relational data attribute order
#'
#' Generic function, with the only given method fetching attribute order for
#' relation schemas.
#'
#' @param x an R object. For the given method, a \code{\link{relation_schema}}.
#' @param ... further arguments passed on to methods.
#'
#' @return A character vector, giving attributes in the order in which they're
#'   prioritised for sorting attributes and keys in the schema.
#' @export
attrs_order <- function(x, ...) {
  UseMethod("attrs_order")
}

merge_attribute_orderings <- function(...) {
  ordered_sets <- list(...)
  # Combining attributes pairwise can't ensure preservation of consistency, so
  # we only add an attribute to the joined list when it's the next one in all
  # lists containing it.
  n_sets <- length(ordered_sets)

  if (n_sets >= 2L) {
    pairs <- utils::combn(seq_len(n_sets), 2)
    if (Position(
      \(n) {
        as1 <- pairs[[1, n]]
        as2 <- pairs[[2, n]]
        is.unsorted(match(as1, as2), na.rm = TRUE) ||
          is.unsorted(match(as2, as1), na.rm = TRUE)
      },
      seq_len(ncol(pairs)),
      nomatch = 0L
    )) {
      warning(paste(
        "pairwise-inconsistent attribute orderings,",
        "returning attributes in order of listing"
      ))
      return(Reduce(union, ordered_sets))
    }
  }

  attrs_order <- unique(unlist(ordered_sets))
  indices <- outer(attrs_order, ordered_sets, Vectorize(match))
  merged <- character()
  while (any(!is.na(indices))) {
    maxs <- apply(indices, 1, max, na.rm = TRUE)
    top <- which(maxs == 1)
    if (length(top) == 0L) {
      warning(paste(
        "inconsistent attribute orderings,",
        "returning remaining attributes in order of listing"
      ))
      return(union(merged, attrs_order))
    }
    nxt <- top[[1L]]
    merged <- c(merged, attrs_order[[nxt]])
    nxt_sets <- !is.na(indices[nxt, ])
    indices[, nxt_sets] <- indices[, nxt_sets] - 1L
    indices <- indices[-nxt, , drop = FALSE]
    attrs_order <- attrs_order[-nxt]
  }
  merged
}
