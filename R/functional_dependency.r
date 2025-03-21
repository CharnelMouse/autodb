#' Functional dependency vectors
#'
#' Creates a set of functional dependencies with length-one dependants.
#'
#' When several sets of functional dependencies are concatenated, their
#' \code{attrs_order} attributes are merged, so as to preserve all of the
#' original attribute orders, if possible. If this is not possible, because the
#' orderings disagree, then the returned value of the \code{attrs_order}
#' attribute is their union instead.
#'
#' @param FDs a list of functional dependencies, in the form of two-elements
#'   lists: the first element contains a character vector of all attributes in
#'   the determinant set, and the second element contains the single dependent
#'   attribute (dependant).
#' @param attrs_order a character vector, giving the names of all attributes.
#'   These need not be present in \code{FDs}, but all attributes in \code{FDs}
#'   must be present in \code{attrs}.
#' @param unique a logical, TRUE by default, for whether to remove duplicate
#'   dependencies.
#'
#' @return A \code{functional_dependency} object, containing the list given in
#'   \code{FDs}, with \code{attrs_order} an attribute of the same name.
#'   Functional dependencies are returned with their determinant sets sorted
#'   according to the attribute order in \code{attrs}. Any duplicates found
#'   after sorting are removed.
#' @seealso \code{\link{detset}}, \code{\link{dependant}}, and
#'   \code{\link{attrs_order}} for extracting parts of the information in a
#'   \code{functional_dependency}; \code{\link{rename_attrs}}
#'   for renaming the attributes in \code{attrs_order}.
#' @examples
#' fds <- functional_dependency(
#'   list(list(c("a", "b"), "c"), list(character(), "d")),
#'   attrs_order = c("a", "b", "c", "d")
#' )
#' print(fds)
#' detset(fds)
#' dependant(fds)
#' attrs_order(fds)
#'
#' # vector operations
#' fds2 <- functional_dependency(list(list("e", "a")), c("a", "e"))
#' c(fds, fds2) # attrs_order attributes are merged
#' unique(c(fds, fds))
#'
#' # subsetting
#' fds[1]
#' fds[c(1, 2, 1)]
#' stopifnot(identical(fds[[2]], fds[2]))
#'
#' # reassignment
#' fds3 <- fds
#' fds3[2] <- functional_dependency(list(list("a", "c")), attrs_order(fds3))
#' print(fds3)
#' detset(fds3)[[2]] <- character()
#' dependant(fds3)[[2]] <- "d"
#' stopifnot(identical(fds3, fds))

#' # changing appearance priority for attributes
#' attrs_order(fds3) <- rev(attrs_order(fds3))
#' fds3
#'
#' # reconstructing from components
#' fds_recon <- functional_dependency(
#'  Map(list, detset(fds), dependant(fds)),
#'  attrs_order(fds)
#' )
#' stopifnot(identical(fds_recon, fds))
#'
#' # can be a data frame column
#' data.frame(id = 1:2, fd = fds)
#'
#' # (in)equality ignores header
#' stopifnot(all(fds3 == fds))
#' stopifnot(!any(fds != fds))
#' @export
functional_dependency <- function(
  FDs,
  attrs_order,
  unique = TRUE
) {
  stop_with_elements_if(
    lengths(FDs) != 2,
    "FDs elements must have length two"
  )
  det_sets <- lapply(FDs, `[[`, 1L)
  stop_with_elements_if(
    vapply(det_sets, Negate(is.character), logical(1)),
    "FD determinant sets must be characters"
  )
  stop_with_elements_if(
    !vapply(det_sets, Negate(anyDuplicated), logical(1)),
    "attributes in determinant sets must be unique"
  )
  deps <- lapply(FDs, `[[`, 2L)
  stop_with_elements_if(
    vapply(deps, Negate(is.character), logical(1)) |
      lengths(deps) != 1L,
    "FD dependants must be length-one characters"
  )
  stop_with_elements_if(
    !is.element(unlist(FDs), attrs_order),
    "attributes in FDs must be present in attrs_order"
  )
  stop_with_values_if(
    attrs_order,
    duplicated(attrs_order),
    "attrs_order must be unique",
    prefix = "duplicated",
    suffix_else = "",
    unique = TRUE
  )
  sorted_FDs <- lapply(
    FDs,
    \(FD) list(FD[[1]][order(match(FD[[1]], attrs_order))], FD[[2]])
  )
  structure(
    if (unique) unique(sorted_FDs) else sorted_FDs,
    attrs_order = attrs_order,
    class = "functional_dependency"
  )
}

#' @exportS3Method
detset.functional_dependency <- function(x, ...) {
  lapply(unclass(x), `[[`, 1L)
}

#' @export
`detset<-.functional_dependency` <- function(x, ..., value) {
  functional_dependency(
    Map(list, value, dependant(x)),
    attrs_order(x)
  )
}

#' @exportS3Method
dependant.functional_dependency <- function(x, ...) {
  vapply(unclass(x), `[[`, character(1L), 2L)
}

#' @export
`dependant<-.functional_dependency` <- function(x, ..., value) {
  functional_dependency(
    Map(list, detset(x), value),
    attrs_order(x)
  )
}

#' @exportS3Method
attrs_order.functional_dependency <- function(x, ...) {
  attr(x, "attrs_order")
}

#' @export
`attrs_order<-.functional_dependency` <- function(x, ..., value) {
  functional_dependency(
    Map(list, detset(x), dependant(x)),
    attrs_order = value
  )
}

#' @exportS3Method
rename_attrs.functional_dependency <- function(x, names, ...) {
  old <- attrs_order(x)
  new_detsets <- lapply(detset(x), \(as) names[match(as, old)])
  new_dependants <- names[match(dependant(x), old)]
  functional_dependency(
    Map(list, new_detsets, new_dependants),
    names
  )
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

  functional_dependency(
    joined_dependencies,
    joined_attrs,
    unique = unique
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
`[<-.functional_dependency` <- function(x, i, value) {
  check_reassignment_same_class(value, x)
  dets <- detset(x)
  deps <- dependant(x)
  dets[i] <- detset(value)
  deps[i] <- dependant(value)
  ao <- merge_attribute_orderings(attrs_order(x), attrs_order(value))
  functional_dependency(Map(list, dets, deps), ao)
}

#' @export
`[[.functional_dependency` <- function(x, i) {
  indices <- stats::setNames(seq_along(x), names(x))
  taken <- try(indices[[i]], silent = TRUE)
  if (class(taken)[[1]] == "try-error")
    stop(attr(taken, "condition")$message)
  x[taken]
}

#' @export
`[[<-.functional_dependency` <- function(x, i, value) {
  indices <- stats::setNames(seq_along(x), names(x))
  taken <- try(indices[[i]], silent = TRUE)
  if (class(taken)[[1]] == "try-error")
    stop(attr(taken, "condition")$message)
  x[taken] <- value
  x
}

#' @exportS3Method
as.character.functional_dependency <- function(
  x,
  align_arrows = c("no", "left", "right"),
  ...
) {
  align_arrows <- match.arg(align_arrows)
  det_txt <- vapply(detset(x), toString, character(1))
  dep_txt <- dependant(x)
  switch(
    align_arrows,
    no = lpadding <- rpadding <- rep("", length(x)),
    left = if (length(x) == 0) {
      lpadding <- rpadding <- rep("", length(x))
    }else{
      det_nchar <- nchar(det_txt)
      lpadding <- vapply(
        max(det_nchar) - det_nchar,
        \(n) paste(rep(" ", n), collapse = ""),
        character(1)
      )
      rpadding <- rep("", length(x))
    },
    right = if (length(x) == 0) {
      lpadding <- rpadding <- rep("", length(x))
    }else{
      dep_nchar <- nchar(dep_txt)
      rpadding <- vapply(
        max(dep_nchar) - dep_nchar,
        \(n) paste(rep(" ", n), collapse = ""),
        character(1)
      )
      lpadding <- rep("", length(x))
    }
  )
  paste0(lpadding, det_txt, " -> ", dep_txt, rpadding, recycle0 = TRUE)
}

#' @exportS3Method
print.functional_dependency <- function(x, ...) {
  txt <- as.character(x, align_arrows = "left")
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
format.functional_dependency <- function(x, ...) {
  as.character(x, align_arrows = "right")
}

#' @exportS3Method
as.data.frame.functional_dependency <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  nm = deparse1(substitute(x))[[1L]]
) {
  res <- data.frame(a = seq_along(x))[, FALSE, drop = FALSE]
  res$x <- x
  names(res) <- NULL
  if (!optional)
    names(res) <- nm
  res
}

#' @exportS3Method
Ops.functional_dependency <- function(e1, e2) {
  ok <- switch(.Generic, `==` = , `!=` = TRUE, FALSE)
  if (!ok) {
    stop(gettextf(
      "%s not meaningful for functional_dependency objects",
      sQuote(.Generic)
    ))
  }
  switch(
    .Generic,
    `==` = mapply(setequal, detset(e1), detset(e2)) &
      dependant(e1) == dependant(e2),
    `!=` = !mapply(setequal, detset(e1), detset(e2)) |
      dependant(e1) != dependant(e2)
  )
}
