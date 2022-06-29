#' Finds dependencies within dataframe df with the DFD search algorithm
#'
#' @inheritParams auto_entityset
#'
#' @return A list of dependencies.
#' @export
find_dependencies <- function(df, accuracy) {
  dfd(df, accuracy)
}

#' Creates a normalized entity set from a dataframe
#'
#' @param df a data.frame, containing the data to be normalised.
#' @param accuracy a numeric in (0, 1], giving the accuracy threshold threshold
#'   required in order to conclude a dependency.
#' @param name a character scalar, giving the name of the created entity set.
#' @param filter a logical, indicating whether to filter the discovered
#'   functional dependencies before normalising the data.frame. Dependencies
#'   with any determinant attributes that aren't characters, integers, factors,
#'   or logicals are removed.
#'
#' @return An entity set, containing the data normalised into the required
#'   number of tables.
#' @export
auto_entityset <- function(
  df,
  accuracy,
  name = NA_character_,
  filter = FALSE
) {
  deps <- find_dependencies(df, accuracy)
  deps$dependencies <- flatten(deps$dependencies)
  if (filter)
    deps <- filter(deps, df)
  EntitySet(df, deps, name)
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

filter <- function(relations, df) {
  # Removes functional dependencies where any determinant attributes do no
  # contain strings, integers, factors, or logicals in the data.frame. The idea
  # is that, for example, we don't expect floats to be part of a key.
  for (rel in relations) {
    lhs <- rel[[1]]
    for (attr in lhs) {
      if (!inherits(
        df[[attr]],
        c("character", "integer", "factor", "logical")
      )) {
        relations <- setdiff(relations, list(rel))
        break
      }
    }
  }
  relations
}
