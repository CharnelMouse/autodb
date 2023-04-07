#' Functional dependency vectors
#'
#' Creates functional dependencies with length-one dependents.
#'
#' @param FDs a list of functional dependencies, in the form of two-elements
#'   lists: the first element contains character vector of all attributes in the
#'   determinant set, and the second element contains the single dependent
#'   attribute.
#' @param attrs a character vector, giving the names of all attributes. These
#'   need not be present in \code{FDs}, but all attributes in \code{FDs} must be
#'   present in \code{attrs}.
#'
#' @return a \code{functional_dependency} object, containing the list given in
#'   \code{FDs}, with \code{attrs} stored in an attribute of the same name.
#'   Functional dependencies are returned with their determinant sets sorted
#'   according to the attribute order in \code{attrs}.
#' @export
functional_dependency <- function(FDs, attrs) {
  if (any(lengths(FDs) != 2))
    stop("FDs elements must have length two")
  det_sets <- lapply(FDs, `[[`, 1L)
  if (any(vapply(det_sets, Negate(is.character), logical(1))))
    stop("FDs determinant sets must be characters")
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
  structure(sorted_FDs, attrs = attrs, class = c("functional_dependency", "list"))
}

#' @export
`[.functional_dependency` <- function(x, i) {
  attrs <- attributes(x)
  res <- unclass(x)[i]
  attributes(res) <- attrs
  res
}

#' @exportS3Method
print.functional_dependency <- function(x, ...) {
  det_txt <- vapply(
    x,
    \(fd) toString(fd[[1]]),
    character(1)
  )
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
  dep_txt <- vapply(x, `[[`, character(1), 2L)
  txt <- paste0(padding, det_txt, " -> ", dep_txt, recycle0 = TRUE)
  cat(paste0(
    length(x),
    " functional dependenc",
    if (length(x) == 1) "y" else "ies"
  ))
  cat(paste0("\n", length(attr(x, "attrs")), " attributes"))
  if (length(attr(x, "attrs")) > 0)
    cat(paste0(": ", toString(attr(x, "attrs"))))
  if (length(txt) > 0L) {
    cat("\n")
    cat(txt, sep = "\n")
  }
}

#' Flatten functional dependency list for normalisation
#'
#' @param dependencies a list, containing functional dependencies as returned by
#'   \code{\link{dfd}}.
#'
#' @return A list of \code{function_dependency} objects, i.e. two-element lists,
#'   with the dependency's determinants in the first element, and its dependent
#'   in the second. Attributes are stored, in the original ordering, in the
#'   \code{attrs} attributes. This is the format required by
#'   \code{\link{normalise}}.
flatten <- function(dependencies) {
  result <- list()
  for (i in seq_along(dependencies$dependencies)) {
    rhs <- names(dependencies$dependencies)[i]
    result <- c(
      result,
      lapply(dependencies$dependencies[[i]], \(lhs) list(lhs, rhs))
    )
  }
  functional_dependency(result, dependencies$attrs)
}
