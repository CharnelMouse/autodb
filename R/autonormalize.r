#' Finds dependencies within dataframe df with the DFD search algorithm
#'
#' @inheritParams auto_entityset
#'
#' @return A list of dependencies.
#' @export
find_dependencies <- function(df, accuracy, index = NA) {
  # Finds dependencies within dataframe df with the DFD search algorithm.
  # Returns the dependencies as a Dependencies object.
  #
  # Arguments:
  #   df (pd.Dataframe) : the dataframe containing data
  #
  # accuracy (0 < float <= 1.00; default = 0.98) : the accuracy threshold required in order to conclude a dependency (i.e. with accuracy = 0.98, 0.98 of the rows must hold true the dependency LHS --> RHS)
  #
  # index (str, optional) : name of column that is intended index of df
  #
  # Returns:
  #
  #   dependencies (Dependencies) : the dependencies found in the data
  # within the contraints provided
  deps <- Dependencies(
    dependencies = dfd(df, accuracy, index),
    primary_key = index
  )
  if (is.na(index) || is.null(index)) {
    prim_key <- choose_index(find_candidate_keys(deps), df)
    deps$primary_key <- prim_key
  }else
    deps$primary_key <- index
  deps
}

#' Creates a normalized entity set from a dataframe
#'
#' @param df a data.frame, containing the data to be normalised.
#' @param accuracy a numeric in (0, 1], giving the accuracy threshold threshold
#'   required in order to conclude a dependency.
#' @param index a character scalar, giving the name of the columns intended as
#'   the primary key of \code{df}. Defaults to NA, no preferred primary key.
#' @param name a character scalar, giving the name of the created entity set.
#' @param time_index a character scalar, giving the name of the time column in
#'   \code{df}.
#'
#' @return An entity set, containing the data normalised into the required
#'   number of tables.
#' @export
auto_entityset <- function(
  df,
  accuracy,
  index = NA,
  name = NA,
  time_index = NULL
) {
  # Creates a normalized entityset from a dataframe.
  #
  # Arguments:
  #
  #   df (pd.Dataframe) : the dataframe containing data
  #
  # accuracy (0 < float <= 1.00; default = 0.98) : the accuracy threshold required in order to conclude a dependency (i.e. with accuracy = 0.98, 0.98 of the rows must hold true the dependency LHS --> RHS)
  #
  # index (str, optional) : name of column that is intended index of df
  #
  # name (str, optional) : the name of created EntitySet
  #
  # time_index (str, optional) : name of time column in the dataframe.
  #
  # Returns:
  #
  #   entityset (ft.EntitySet) : created entity set
  deps <- find_dependencies(df, accuracy, index)
  depdf <- DepDF(deps, df)
  EntitySet(depdf, name, time_index)
}
