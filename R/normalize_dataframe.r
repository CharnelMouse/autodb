#' Normalise a data.frame based on given dependencies
#'
#' @param df a data.frame, containing the data to be normalised.
#' @param dependencies a Dependencies object, giving the dependencies to
#'   determine normalisation with.
#'
#' @return a list of data.frames, containing the normalised data.
#' @export
normalize_dataframe <- function(df, dependencies) {
  norm_deps <- normalize_dependencies(dependencies$dependencies)
  norm_attr_sets <- lapply(norm_deps, `[[`, "attrs")
  norm_key_sets <- lapply(norm_deps, `[[`, "keys")
  reference_mat <- outer(
    norm_attr_sets,
    norm_key_sets,
    Vectorize(\(from_attrs, to_keys) {
      any(vapply(to_keys, \(key) all(key %in% from_attrs), logical(1)))
    })
  )
  df_names <- names(df)
  indexes <- lapply(norm_key_sets, choose_index, df_names)
  relation_names <- vapply(indexes, name_dataframe, character(1))
  depdf_list <- Map(
    \(norm_dep_set, index) {
      attrs <- norm_dep_set$attrs
      sorted_attrs <- attrs[order(match(attrs, df_names))]
      keys <- norm_dep_set$keys[order(match(norm_dep_set$keys, df_names))]
      list(
        df = unique(df[, sorted_attrs]),
        keys = keys,
        index = index
      )
    },
    norm_deps,
    indexes
  )
  for (n in seq_along(norm_deps)) {
    refs <- reference_mat[n, ]
    ref_names <- relation_names[setdiff(which(refs), n)]
    depdf_list[[n]]$children <- ref_names
  }
  stats::setNames(depdf_list, relation_names)
}

drop_primary_dups <- function(df, prim_key) {
  # Reduces a data.frame to have unique values of the attributes in the given
  # primary key. If the other columns are not uniquely determined by the primary
  # key, as can be the case for approximate dependencies, the most common value
  # for each primary key value is used.
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

name_dataframe <- function(index) {
  paste(index, collapse = "_")
}
