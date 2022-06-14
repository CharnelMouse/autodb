#' Normalise a data.frame based on given dependencies
#'
#' @param df a data.frame, containing the data to be normalised.
#' @param dependencies a Dependencies object, giving the dependencies to
#'   determine normalisation with.
#'
#' @return a list of data.frames, containing the normalised data.
#' @export
normalize_dataframe <- function(df, dependencies) {
  norm_deps <- normalize_dependencies(dependencies)
  depdf_list <- list()
  reference_mat <- outer(
    lapply(norm_deps, `[[`, "attrs"),
    lapply(norm_deps, `[[`, "keys"),
    Vectorize(\(from_attrs, to_keys) {
      any(vapply(to_keys, \(key) all(key %in% from_attrs), logical(1)))
    })
  )
  indexes <- lapply(lapply(norm_deps, `[[`, "keys"), choose_index, df)
  for (n in seq_along(norm_deps)) {
    norm_dep_set <- norm_deps[[n]]
    attrs <- norm_dep_set$attrs
    sorted_attrs <- attrs[order(match(attrs, names(df)))]
    keys <- norm_dep_set$keys[order(match(norm_dep_set$keys, names(df)))]
    new_depdf <- list(
      df = unique(df[, sorted_attrs]),
      keys = keys,
      index = indexes[[n]]
    )
    depdf_list <- c(depdf_list, list(new_depdf))
  }
  relation_names <- vapply(depdf_list, name_dataframe, character(1))
  for (n in seq_along(norm_deps)) {
    refs <- reference_mat[n, ]
    ref_names <- relation_names[setdiff(which(refs), n)]
    depdf_list[[n]]$children <- ref_names
  }
  stats::setNames(depdf_list, relation_names)
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
