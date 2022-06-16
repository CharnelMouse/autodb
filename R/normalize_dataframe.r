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

name_dataframe <- function(depdf) {
  paste(depdf$index, collapse = "_")
}
