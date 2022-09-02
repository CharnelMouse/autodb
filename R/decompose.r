#' Decompose a data.frame based on given normalised dependencies
#'
#' Decomposes a data.frame into several tables, based on the given database
#' scheme. It's intended that the relations are derived from a list of
#' functional dependencies for the same data.frame: using anything else will
#' give undefined behaviour.
#'
#' @param df a data.frame, containing the data to be normalised.
#' @param scheme a database scheme with foreign key relationships, as given by
#'   \code{\link{cross_reference}}.
#' @inheritParams autonorm
#'
#' @return A database, represented by a named list, with three elements:
#'   \itemize{
#'     \item \code{name} contains the assigned name of the relation set, if any;
#'     \item \code{tables} contains a list of tables in third normal form, that
#'     can reproduce the original data.frame;
#'     \item \code{relationships} contains relationships between the tables,
#'     represented as a list of length-four character vectors. In order, the
#'     elements are the name of the child table, the name of the linked
#'     attribute in the child table, the name of the parent table, and the name
#'     of the linked attribute in the parent table. Currently, the attribute is
#'     expected to have the same name in both tables.
#'   }
#'
#'   Tables are lists with the following elements: \itemize{ \item \code{df},
#'   the data.frame containing the data. \item \code{keys}, the list of
#'   character vectors, representing (candidate) keys for the table. \item
#'   \code{index}, a character vector, representing the index / primary key of
#'   the table. \item \code{parents}, containing names of parent tables, i.e.
#'   tables referenced in foreign keys. }
#' @export
decompose <- function(df, scheme, name = NA_character_) {
  indexes <- lapply(scheme$keys, `[[`, 1)
  relation_names <- vapply(indexes, name_dataframe, character(1))
  stopifnot(!anyDuplicated(relation_names))

  depdf_list <- Map(
    \(attrs, keys, index, parents) {
      list(
        df = unique(df[, attrs, drop = FALSE]),
        keys = keys,
        index = index,
        parents = relation_names[parents]
      )
    },
    scheme$attrs,
    scheme$keys,
    indexes,
    scheme$parents
  )
  relationships <- lapply(
    scheme$relationships,
    \(r) {
      c(relation_names[r[[1]][1]], r[[2]], relation_names[r[[1]][2]], r[[2]])
    }
  )
  list(
    name = name,
    tables = stats::setNames(depdf_list, relation_names),
    relationships = relationships
  )
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
