normalize_step <- function(x, ...) {
  UseMethod("normalize_step")
}

#' @export
normalize_step.DepDF <- function(x) {
  # Only splits off a descendent as needed, doesn't normalize it too.
  # Additionally, stops after the first split, e.g. splitting for partial
  # dependencies might leave transitive dependencies in the main data.frame.
  # This is a silly function.
  part_deps <- find_filtered_partial_deps(x$deps, x$df)
  if (length(part_deps) > 0) {
    new_depdfs <- split_for(x, part_deps)
    return(new_depdfs)
  }
  trans_deps <- find_filtered_trans_deps(x$deps, x$df)
  if (length(trans_deps) > 0) {
    new_depdfs <- split_for(x, trans_deps)
    return(new_depdfs)
  }
  stats::setNames(list(x), name_dataframe(x))
}

#' Normalise a data.frame based on given dependencies
#'
#' @param x a data.frame, containing the data to be normalised.
#' @param dependencies a Dependencies object, giving the dependencies to
#'   determine normalisation with.
#'
#' @return a list of data.frames, containing the normalised data.
#' @export
normalize_step.data.frame <- function(x, dependencies) {
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
  depdf <- DepDF(dependencies, x, get_prim_key(dependencies))
  df_list <- normalize_step.DepDF(depdf)
  df_list
}

split_up <- function(depdf, split_on) {
  UseMethod("split_up")
}

#' @export
split_up.DepDF <- function(depdf, split_on) {
  # Breaks off a depdf and forms its child. Recursively calls normalize on
  # the original depdf, and its newly formed child.
  #
  # Arguments:
  #     split_on (list[str]) : attributes to split the dataframe on
  #     depdf (DepDF) : the depdf ot split
  pc <- split_on_dep(split_on, depdf$deps)
  parent_deps <- pc[[1]]
  child_deps <- pc[[2]]
  child <- DepDF(
    deps = child_deps,
    df = form_child(depdf$df, child_deps),
    index = split_on,
    parent = name_dataframe(depdf)
  )
  depdf$deps <- parent_deps
  depdf$df <- depdf$df[
    ,
    names(parent_deps$dependencies)
  ]
  depdf$children <- c(depdf$children, name_dataframe(child))
  c(
    normalize_step(depdf),
    normalize_step(child)
  )
}

split_for <- function(depdf, unwanted_deps) {
  split_on <- find_most_comm(unwanted_deps, depdf$deps, depdf$df)
  depdfs <- split_up(depdf, split_on)
  nms <- vapply(depdfs, name_dataframe, character(1))
  stopifnot(!anyDuplicated(nms))
  stats::setNames(depdfs, make.unique(nms))
}

drop_primary_dups <- function(df, prim_key) {
  # Drops all duplicates based off of the columns in prim_key. If there isn't a
  # unique value for the other columns, for every unique instance of columns in
  # prim_key, keeps the "mode" of the unique instances' occurance.
  #
  # Arguments:
  #     df (pd.DataFrame) : dataframe to drop duplicates of
  #     prim_key (list[str]) : columns that form the primary key of the dataframe
  #
  # Returns:
  #     new_df (pd.DataFrame) : dataframe with duplicates dropped
  df_lst <- list()

  if (nrow(unique(df[, prim_key, drop = FALSE])) == nrow(df))
    return(df)

  groups <- split(
    df,
    as.list(df[, c(prim_key), drop = FALSE]),
    drop = TRUE
  )

  for (group in groups) {
    df_lst <- c(df_lst, list(data.frame(lapply(group, Mode))))
    # new_df = new_df.append(group.mode().iloc[0], ignore_index=TRUE)
  }
  result <- `rownames<-`(
    stats::setNames(Reduce(rbind, df_lst), colnames(df)),
    NULL
  )
  for (i in seq_along(df)) {
    class(result[[i]]) <- class(df[[i]])
  }
  result
}

Mode <- function(x) {
  uniqs <- unique(x)
  tabs <- tabulate(match(x, uniqs))
  uniqs[[which.max(tabs)]]
}

form_child <- function(df, deps) {
  # Returns a new dataframe based off of the dependencies in deps.
  #
  # Arguments:
  #     df (pd.DataFrame) : dataframe to create new dataframe from
  #     deps (Dependencies) : dependencies to base new dataframe off of
  attrs <- names(deps$dependencies)
  drops <- setdiff(colnames(df), attrs)
  new_df <- df[, setdiff(colnames(df), drops)]
  new_df <- drop_primary_dups(new_df, get_prim_key(deps))
  new_df
}

name_dataframe <- function(depdf) {
  paste(depdf$index, collapse = "_")
}

# class DepDF(object):
#   """
#     Represents dataframe and functional dependencies between columns in it.
#     Used in the normalization process.
#
#     Attributes:
#         deps
#         df
#         parent
#         children
#         index
#     """
#
# def __init__(self, deps, df, index, parent=None):
#   """
#         Creates a DepDF.
#
#         Arguments:
#             deps (Dependencies) : dependenies among the df
#             df (pd.DataFrame) : dataframe for the object
#             index (list[str]) : index columns for dataframe
#             parent (DepDF, optional) : parent DepDF object
#         """
# self.deps = deps
# self.df = df
# self.parent = parent
# self.children = []
# self.index = index
#
# def return_dfs(self):
#   """
#         Returns the dataframes stored in self and all its descendents.
#
#         Returns:
#             dfs (list[pd.DataFrame]) : dataframes
#         """
# if self.children == []:
#   return [self.df]
# result = [self.df]
# for child in self.children:
#   result += child.return_dfs()
# return result

DepDF <- function(
  deps,
  df,
  index = deps$primary_key,
  parent = NA_character_,
  children = character()
) {
  UseMethod("DepDF")
}

#' @export
DepDF.Dependencies <- function(
  deps,
  df,
  index = deps$primary_key,
  parent = NA_character_,
  children = character()
) {
  lst <- list(
    deps = deps,
    df = df,
    index = index,
    children = children,
    parent = parent
  )
  class(lst) <- c("DepDF", class(lst))
  lst
}
