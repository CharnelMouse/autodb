#' Finds dependencies within dataframe df with the DFD search algorithm
#'
#' @inheritParams auto_entityset
#'
#' @return A list of dependencies.
#' @export
find_dependencies <- function(
  df,
  accuracy,
  ...
) {
  dfd(df, accuracy, ...)
}

#' Creates a normalised entity set from a dataframe
#'
#' @param df a data.frame, containing the data to be normalised.
#' @param accuracy a numeric in (0, 1], giving the accuracy threshold threshold
#'   required in order to conclude a dependency.
#' @param name a character scalar, giving the name of the created entity set.
#' @param ... further arguments passed on to \code{\link{dfd}}.
#'
#' @return An entity set, containing the data normalised into the required
#'   number of tables.
#' @export
auto_entityset <- function(
  df,
  accuracy,
  name = NA_character_,
  ...
) {
  deps <- find_dependencies(
    df,
    accuracy,
    ...
  )
  deps <- flatten(deps)
  norm_deps <- normalise(deps)
  tables <- decompose(df, norm_deps)
  EntitySet(tables, name)
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
