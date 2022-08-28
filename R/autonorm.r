#' Create a normalised entity set from a dataframe
#'
#' This is a wrapper function for applying \code{\link{dfd}},
#' \code{\link{flatten}}, \code{\link{normalise}}, \code{\link{decompose}}, and
#' \code{\link{cross_reference}}, in order.
#'
#' @param df a data.frame, containing the data to be normalised.
#' @param accuracy a numeric in (0, 1], giving the accuracy threshold threshold
#'   required in order to conclude a dependency.
#' @param ... further arguments passed on to \code{\link{dfd}}.
#'
#' @return A "database", a list of two elements. See
#'   \code{\link{cross_reference}} for details.
#' @export
autonorm <- function(
  df,
  accuracy,
  ...
) {
  deps <- dfd(df, accuracy, ...)
  deps <- flatten(deps)
  norm_deps <- normalise(deps)
  tables <- decompose(df, norm_deps)
  cross_reference(tables)
}

#' Flatten functional dependency list for normalisation
#'
#' @param dependencies a list, containing functional dependencies as returned by
#'   \code{\link{dfd}}.
#'
#' @return A copy of \code{dependencies}, where the \code{dependencies} element
#'   has been transformed to a list of two-element lists, with the dependency's
#'   determinants in the first element, its dependent in the second. This is the
#'   format required by \code{\link{normalise}}.
#' @export
flatten <- function(dependencies) {
  # Takes dependencies grouped by dependent attribute, and returns the
  # dependencies in a flat list with (parent table, parent attr, child table,
  # child attr) format.
  result <- list()
  for (i in seq_along(dependencies$dependencies)) {
    rhs <- names(dependencies$dependencies)[i]
    result <- c(
      result,
      lapply(dependencies$dependencies[[i]], \(lhs) list(lhs, rhs))
    )
  }
  dependencies$dependencies <- result
  dependencies
}
