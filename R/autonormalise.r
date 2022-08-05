#' Finds dependencies within dataframe df with the DFD search algorithm
#'
#' @inheritParams auto_entityset
#'
#' @return A list of dependencies.
#' @export
find_dependencies <- function(
  df,
  accuracy,
  exclude = character(),
  exclude_class = character()
) {
  dfd(df, accuracy, exclude = exclude, exclude_class = exclude_class)
}

#' Creates a normalised entity set from a dataframe
#'
#' @param df a data.frame, containing the data to be normalised.
#' @param accuracy a numeric in (0, 1], giving the accuracy threshold threshold
#'   required in order to conclude a dependency.
#' @param name a character scalar, giving the name of the created entity set.
#' @param exclude a character vector, containing names of attributes to not
#'   consider as members of keys. If names are given that aren't present in
#'   \code{df}, the user is given a warning.
#' @param exclude_class a character vector, indicating classes of attributes to
#'   not consider as members of keys. Attributes are excluded if they inherit
#'   from any given class.
#'
#' @return An entity set, containing the data normalised into the required
#'   number of tables.
#' @export
auto_entityset <- function(
  df,
  accuracy,
  name = NA_character_,
  exclude = character(),
  exclude_class = character()
) {
  deps <- find_dependencies(
    df,
    accuracy,
    exclude = exclude,
    exclude_class = exclude_class
  )
  deps$dependencies <- flatten(deps$dependencies)
  norm_deps <- normalise(deps)
  tables <- decompose(df, norm_deps)
  EntitySet(tables, norm_deps, name)
}

flatten <- function(dependencies) {
  # Takes dependencies grouped by dependent attribute, and returns the
  # dependencies in a flat list with (parent table, parent attr, child table,
  # child attr) format.
  result <- list()
  for (i in seq_along(dependencies)) {
    rhs <- names(dependencies)[i]
    result <- c(
      result,
      lapply(dependencies[[i]], \(lhs) list(lhs, rhs))
    )
  }
  result
}
