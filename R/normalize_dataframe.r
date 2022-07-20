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

  ref_detsets <- list()
  ref_deps <- character()
  for (n in seq_along(indexes)) {
    rel_n <- relation_names[n]
    ind <- indexes[[n]]
    for (m in seq_along(norm_deps$attrs)) {
      rel_m <- relation_names[m]
      if (m != n && all(ind %in% norm_deps$attrs[[m]])) {
        for (a in ind) {
          ref_detsets <- c(ref_detsets, paste(rel_m, a, sep = "."))
          ref_deps <- c(ref_deps, paste(rel_n, a, sep = "."))
        }
      }
    }
  }
  ref_attrs <- unique(c(unlist(ref_detsets), unlist(ref_deps)))
  ref_vecs <- list(
    determinant_sets = ref_detsets,
    dependents = ref_deps
  ) |>
    convert_to_integer_attributes(ref_attrs) |>
    remove_extraneous_dependencies()
  # convert back to characters
  ref_vecs$determinant_sets <- lapply(ref_vecs$determinant_sets, \(a) ref_attrs[a])
  ref_vecs$dependents <- ref_attrs[ref_vecs$dependents]
  ref_split <- Map(
    c,
    strsplit(as.character(unlist(ref_vecs$determinant_sets)), ".", fixed = TRUE),
    strsplit(as.character(ref_vecs$dependents), ".", fixed = TRUE)
  )
  ref_split_from <- vapply(ref_split, `[[`, character(1), 1)
  ref_split_deps <- vapply(ref_split, `[[`, character(1), 2)
  ref_split_to <- vapply(ref_split, `[[`, character(1), 3)
  ref_split_grouped <- split(ref_split_deps, paste(ref_split_from, ref_split_to, sep = "->"))
  grps <- strsplit(names(ref_split_grouped), "->", fixed = TRUE)
  foreign_keys <- Map(
    \(attrs, tables) {
      from <- tables[1]
      to <- tables[2]
      list(
        attributes = attrs,
        child = from,
        parent = to
      )
    },
    unname(ref_split_grouped),
    grps
  )

  reference_mat <- matrix(
    FALSE,
    nrow = length(norm_deps$attrs),
    ncol = length(norm_deps$keys)
  )
  for (ref in foreign_keys) {
    reference_mat[
      match(ref$child, relation_names),
      match(ref$parent, relation_names)
    ] <- TRUE
  }

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
