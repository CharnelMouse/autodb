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
#'
#' @return An entity set, containing the data normalised into the required
#'   number of tables.
#' @export
auto_entityset <- function(
  df,
  accuracy,
  name = NA
) {
  deps <- find_dependencies(df, accuracy)
  depdf <- DepDF(Dependencies(deps), df)
  EntitySet(depdf, name)
}
