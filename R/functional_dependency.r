#' Functional dependency vectors
#'
#' Creates a set of functional dependencies with length-one dependents.
#'
#' When several sets of functional dependencies are concatenated, their
#' \code{attrs} attributes are merged, so as to preserve all of the original
#' attribute orders, if possible. If this is not possible, because the orderings
#' disagree, then the returned value of the \code{attrs} attribute is their
#' union instead.
#'
#' @param FDs a list of functional dependencies, in the form of two-elements
#'   lists: the first element contains character vector of all attributes in the
#'   determinant set, and the second element contains the single dependent
#'   attribute.
#' @param attrs a character vector, giving the names of all attributes. These
#'   need not be present in \code{FDs}, but all attributes in \code{FDs} must be
#'   present in \code{attrs}.
#' @param unique a logical, TRUE by default, for whether to remove duplicate
#'   dependencies.
#'
#' @return a \code{functional_dependency} object, containing the list given in
#'   \code{FDs}, with \code{attrs} stored in an attribute of the same name.
#'   Functional dependencies are returned with their determinant sets sorted
#'   according to the attribute order in \code{attrs}. Any duplicates found
#'   after sorting are removed.
#' @seealso \code{\link{detset}}, \code{\link{dependent}}, and
#'   \code{\link{attrs}} for extracting parts of the information in a
#'   \code{functional_dependency}.
#' @examples
#' fds <- functional_dependency(
#'   list(list(c("a", "b"), "c"), list(character(), "d")),
#'   attrs = c("a", "b", "c", "d")
#' )
#' detset(fds)
#' dependent(fds)
#' attrs(fds)
#' @export
functional_dependency <- function(FDs, attrs, unique = TRUE) {
  if (any(lengths(FDs) != 2))
    stop("FDs elements must have length two")
  det_sets <- lapply(FDs, `[[`, 1L)
  if (any(vapply(det_sets, Negate(is.character), logical(1))))
    stop("FD determinant sets must be characters")
  if (any(!vapply(det_sets, Negate(anyDuplicated), logical(1))))
    stop("attributes in determinant sets must be unique")
  deps <- lapply(FDs, `[[`, 2L)
  if (
    any(vapply(deps, Negate(is.character), logical(1))) ||
    any(lengths(deps) != 1L)
  )
    stop("FDs dependents must be length-one characters")
  if (any(!is.element(unlist(FDs), attrs)))
    stop("attributes in FDs must be present in attrs")
  sorted_FDs <- lapply(
    FDs,
    \(FD) list(FD[[1]][order(match(FD[[1]], attrs))], FD[[2]])
  )
  structure(
    if (unique) unique(sorted_FDs) else sorted_FDs,
    attrs = attrs,
    class = c("functional_dependency", "list")
  )
}

#' @export
`[.functional_dependency` <- function(x, i) {
  attrs <- attributes(x)
  res <- unclass(x)[i]
  attributes(res) <- attrs
  res
}

#' @export
`[[.functional_dependency` <- function(x, i) {
  if (length(i) == 0L)
    stop("attempt to select less than one element")
  if (length(i) > 1L)
    stop("attempt to select more than one element")
  x[i]
}

#' @exportS3Method
print.functional_dependency <- function(x, ...) {
  det_txt <- vapply(detset(x), toString, character(1))
  if (length(x) == 0L)
    padding <- character()
  else{
    det_nchar <- nchar(det_txt)
    det_len <- max(det_nchar)
    padding <- vapply(
      det_len - det_nchar,
      \(n) paste(rep(" ", n), collapse = ""),
      character(1)
    )
  }
  dep_txt <- dependent(x)
  txt <- paste0(padding, det_txt, " -> ", dep_txt, recycle0 = TRUE)
  cat(with_number(length(x), "functional dependenc", "y", "ies"))
  cat(paste0("\n", with_number(length(attrs(x)), "attribute", "", "s")))
  if (length(attrs(x)) > 0)
    cat(paste0(": ", toString(attrs(x))))
  cat("\n")
  if (length(txt) > 0L) {
    cat(txt, sep = "\n")
  }
}

#' @exportS3Method
c.functional_dependency <- function(...) {
  lst <- list(...)
  joined_dependencies <- unique(Reduce(c, lapply(lst, unclass)))

  attrs_list <- lapply(lst, attrs)
  joined_attrs <- do.call(merge_attribute_orderings, attrs_list)

  functional_dependency(joined_dependencies, joined_attrs)
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

  all_attrs <- unique(unlist(ordered_sets))
  indices <- outer(all_attrs, ordered_sets, Vectorize(match))
  merged <- character()
  while (any(!is.na(indices))) {
    maxs <- apply(indices, 1, max, na.rm = TRUE)
    top <- which(maxs == 1)
    if (length(top) == 0L) {
      warning(paste(
        "inconsistent attribute orderings,",
        "returning remaining attributes in order of listing"
      ))
      return(union(merged, all_attrs))
    }
    nxt <- top[[1L]]
    merged <- c(merged, all_attrs[[nxt]])
    nxt_sets <- !is.na(indices[nxt, ])
    indices[, nxt_sets] <- indices[, nxt_sets] - 1L
    indices <- indices[-nxt, , drop = FALSE]
    all_attrs <- all_attrs[-nxt]
  }
  merged
}

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

#' @exportS3Method
detset.functional_dependency <- function(x, ...) {
  lapply(unclass(x), `[[`, 1L)
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

#' @exportS3Method
dependent.functional_dependency <- function(x, ...) {
  vapply(unclass(x), `[[`, character(1L), 2L)
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

#' @exportS3Method
attrs.functional_dependency <- function(x, ...) {
  attr(x, "attrs", exact = TRUE)
}
