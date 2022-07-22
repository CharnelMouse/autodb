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

  indexes <- lapply(norm_deps$keys, `[[`, 1)
  relation_names <- vapply(indexes, name_dataframe, character(1))

  reference_mat <- calculate_reference_matrix(indexes, norm_deps$attrs)

  depdf_list <- Map(
    \(attrs, keys, index) {
      list(
        df = unique(df[, attrs]),
        keys = keys,
        index = index
      )
    },
    norm_deps$attrs,
    norm_deps$keys,
    indexes
  )
  for (n in seq_along(norm_deps$attrs)) {
    refs <- reference_mat[n, ]
    ref_names <- relation_names[setdiff(which(refs), n)]
    depdf_list[[n]]$children <- ref_names
  }
  stats::setNames(depdf_list, relation_names)
}

calculate_reference_matrix <- function(indexes, attrs) {
  ref_detsets <- list()
  ref_deps <- character()
  seq_rel <- seq_along(indexes)
  for (n in seq_rel) {
    for (m in seq_rel[-n]) {
      ind <- indexes[[n]]
      if (all(ind %in% attrs[[m]])) {
        ref_detsets <- c(ref_detsets, paste(m, ind, sep = "."))
        ref_deps <- c(ref_deps, paste(n, ind, sep = "."))
      }
    }
  }

  ref_attrs <- unique(c(unlist(ref_detsets), unlist(ref_deps)))
  ref_vecs <- list(determinant_sets = ref_detsets, dependents = ref_deps) |>
    convert_to_integer_attributes(ref_attrs) |>
    remove_extraneous_dependencies()
  # convert back to characters
  convert_back <- function(x) {
    ref_attrs[x] |>
      as.character() |>
      strsplit(".", fixed = TRUE)
  }
  filtered_determinant_sets <- convert_back(unlist(ref_vecs$determinant_sets))
  filtered_dependents <- convert_back(ref_vecs$dependents)
  rel_inds <- function(x) strtoi(vapply(x, `[[`, character(1), 1))
  rel_attrs <- function(x) vapply(x, `[[`, character(1), 2)
  filtered_children <- rel_inds(filtered_determinant_sets)
  filtered_parents <- rel_inds(filtered_dependents)
  filtered_attrs <- rel_attrs(filtered_determinant_sets)
  stopifnot(identical(filtered_attrs, rel_attrs(filtered_dependents)))

  unique_ref_splits <- !duplicated(Map(c, filtered_children, filtered_parents))
  unique_ref_children <- filtered_children[unique_ref_splits]
  unique_ref_parents <- filtered_parents[unique_ref_splits]
  ref_order <- order(unique_ref_children, unique_ref_parents)
  children <- unique_ref_children[ref_order]
  parents <- unique_ref_parents[ref_order]

  n_tables <- length(indexes)
  reference_mat <- matrix(FALSE, nrow = n_tables, ncol = n_tables)
  for (ref in seq_along(ref_order)) {
    reference_mat[children[ref], parents[ref]] <- TRUE
  }

  reference_mat
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
