#' Finds dependencies within dataframe df with the DFD search algorithm
#'
#' @inheritParams auto_entityset
#'
#' @return A list of dependencies.
#' @export
find_dependencies <- function(df, accuracy) {
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
  deps <- find_dependencies(df, accuracy)
  depdf <- DepDF(Dependencies(deps), df)
  EntitySet(depdf, name)
}
