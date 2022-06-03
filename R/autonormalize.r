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

#' Normalizes dependency relationships
#'
#' Normalizes the dependency relationships in dependencies into new
#' groups by breaking up all partial and transitive dependencies.
#'
#' @param x a Dependencies object, containing the dependencies to be normalised.
#' @inheritParams auto_entityset
#'
#' @return a Dependencies object, containing the normalised dependencies.
#' @export
normalize.Dependencies <- function(x, df) {
  # Breaks up a set of dependency relations into groups that are normalized,
  # meaning there are no partial or transitive dependencies within each group.
  #
  # Arguments:
  #   dependencies (Dependencies) : the dependencies to be normalized
  #
  # Returns:
  #   dependencies_groups (list[Dependencies]) : list of Dependencies objects each
  # containing the relations in a new group
  x <- remove_implied_extroneous(x)
  no_part_deps <- remove_part_deps(x, df)
  no_trans_deps <- list()
  for (grp in no_part_deps)
    no_trans_deps <- c(no_trans_deps, remove_trans_deps(grp, df))
  no_trans_deps
}

#' Normalise a data.frame based on given dependencies
#'
#' @param df a data.frame, containing the data to be normalised.
#' @param dependencies a Dependencies object, giving the dependencies to
#'   determine normalisation with.
#'
#' @return a list of data.frames, containing the normalised data.
#' @export
normalize.data.frame <- function(df, dependencies) {
  # Normalizes a dataframe based on the dependencies given. Keys for the newly
  # created DataFrames can only be columns that are strings, ints, or
  # categories. Keys are chosen according to the priority:
  #   1) shortest lenghts 2) has "id" in some form in the name of an attribute
  # 3) has attribute furthest to left in the table
  #
  # Arguments:
  #   df (pd.DataFrame) : dataframe to split up
  # dependencies (Dependencies) : the dependencies to be normalized
  #
  # Returns:
  #   new_dfs (list[DataFrame]) : list of new dataframes
  depdf <- DepDF(dependencies, df, get_prim_key(dependencies))
  df_list <- normalize(depdf)
  df_list
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

auto_normalize <- function(df, accuracy) {
  # Normalizes dataframe via dependencies discovered in data.
  #
  # Arguments:
  #   df (pd.DataFrame) : dataframe to split up
  #
  # Returns:
  #   new_dfs (list[pd.DataFrame]) : list of new dataframes
  normalize(df, find_dependencies(df, accuracy))
}

#' Normalise a given entity set
#'
#' @param es an EntitySet object, containing a single data.frame to be
#'   normalised.
#' @inheritParams auto_entityset
#'
#' @return The created EntitySet, containing the tables from normalising the
#'   original data.frame.
#' @export
normalize.entityset <- function(es, accuracy) {
  # TO DO: add option to pass an EntitySet with more than one dataframe, and
  # specify which one to normalize while preserving existing relationships
  if (length(es$dataframes) > 1)
    stop('There is more than one dataframe in this EntitySet')
  if (length(es$dataframes) == 0)
    stop('This EntitySet is empty')

  df <- es$dataframes[[1]]
  auto_entityset(
    df$df,
    accuracy,
    index = df$index,
    name = es$name,
    time_index = df$time_index
  )
}
