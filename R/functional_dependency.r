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
#' @param attrs_order a character vector, giving the names of all attributes. These
#'   need not be present in \code{FDs}, but all attributes in \code{FDs} must be
#'   present in \code{attrs}.
#' @param attrs_class a named list of character vectors, giving the default
#'   class vectors for all attributes. By default, every attribute is assumed to
#'   be logical. These are mostly for use in \code{\link{relation_schema}}
#'   objects, but are included in \code{functional_dependency} objects to let
#'   class information to be extracted when using \code{\link{discover}}.
#' @param unique a logical, TRUE by default, for whether to remove duplicate
#'   dependencies.
#'
#' @return a \code{functional_dependency} object, containing the list given in
#'   \code{FDs}, with \code{attrs_order} and \code{attrs_class} stored in an
#'   \code{attrs_class} attribute. Functional dependencies are returned with
#'   their determinant sets sorted according to the attribute order in
#'   \code{attrs}. Any duplicates found after sorting are removed. Elements of
#'   \code{attrs_class} are sorted to be in the same order as in
#'   \code{attrs_order}.
#' @seealso \code{\link{detset}}, \code{\link{dependent}}, and
#'   \code{\link{attrs_order}} for extracting parts of the information in a
#'   \code{functional_dependency}.
#' @examples
#' fds <- functional_dependency(
#'   list(list(c("a", "b"), "c"), list(character(), "d")),
#'   attrs_order = c("a", "b", "c", "d")
#' )
#' print(fds)
#' detset(fds)
#' dependent(fds)
#' attrs_order(fds)
#' @export
functional_dependency <- function(
  FDs,
  attrs_order,
  attrs_class = stats::setNames(
    rep(list("logical"), length(attrs_order)),
    attrs_order
  ),
  unique = TRUE
) {
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
  if (any(!is.element(unlist(FDs), attrs_order)))
    stop("attributes in FDs must be present in attrs_order")
  if (length(attrs_class) != length(attrs_order))
    stop("attrs_class must have same length as attrs_order")
  if (!setequal(names(attrs_class), attrs_order))
    stop("attrs_class must have attrs_order as (unordered) names")
  sorted_FDs <- lapply(
    FDs,
    \(FD) list(FD[[1]][order(match(FD[[1]], attrs_order))], FD[[2]])
  )
  structure(
    if (unique) unique(sorted_FDs) else sorted_FDs,
    attrs_class = attrs_class,
    class = "functional_dependency"
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
  cat(paste0("\n", with_number(length(attrs_order(x)), "attribute", "", "s")))
  if (length(attrs_order(x)) > 0)
    cat(paste0(": ", toString(attrs_order(x))))
  cat("\n")
  if (length(txt) > 0L) {
    cat(txt, sep = "\n")
  }
}

#' @exportS3Method
unique.functional_dependency <- function(x, ...) {
  x[!duplicated(x)]
}

#' @exportS3Method
c.functional_dependency <- function(..., unique = TRUE) {
  lst <- list(...)
  joined_dependencies <- Reduce(c, lapply(lst, unclass))

  attrs_list <- lapply(lst, attrs_order)
  joined_attrs <- do.call(merge_attribute_orderings, attrs_list)

  all_classes <- Reduce(c, lapply(lst, attrs_class))
  all_classes <- all_classes[!duplicated(Map(
    list,
    unname(all_classes),
    names(all_classes)
  ))]
  if (length(all_classes) != length(joined_attrs))
    stop("attrs_class values are not consistent")
  joined_classes <- all_classes[joined_attrs]

  functional_dependency(
    joined_dependencies,
    joined_attrs,
    joined_classes,
    unique = unique
  )
}

#' @exportS3Method
detset.functional_dependency <- function(x, ...) {
  lapply(unclass(x), `[[`, 1L)
}

#' @exportS3Method
dependent.functional_dependency <- function(x, ...) {
  vapply(unclass(x), `[[`, character(1L), 2L)
}

#' @exportS3Method
attrs_order.functional_dependency <- function(x, ...) {
  names(attrs_class(x))
}

#' @export
`attrs_order<-.functional_dependency` <- function(x, ..., value) {
  functional_dependency(
    Map(list, detset(x), dependent(x)),
    attrs_order = value,
    attrs_class = attrs_class(x)[value]
  )
}

#' @exportS3Method
attrs_class.functional_dependency <- function(x, ...) {
  attr(x, "attrs_class", exact = TRUE)
}

#' @export
`attrs_class<-.functional_dependency` <- function(x, ..., value) {
  functional_dependency(
    Map(list, detset(x), dependent(x)),
    attrs_order = names(value),
    attrs_class = value
  )
}
