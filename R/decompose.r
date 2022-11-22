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
  relation_names <- scheme$relation_names
  # stopifnot(!anyDuplicated(relation_names))
  stopifnot(identical(names(df), scheme$all_attrs))

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
  structure(
    list(
      name = name,
      tables = stats::setNames(depdf_list, relation_names),
      relationships = relationships,
      attributes = scheme$all_attrs
    ),
    class = c("database", "list")
  )
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

#' @exportS3Method
print.database <- function(x, max = 10, ...) {
  n_relations <- length(x$tables)
  cat(paste0(
    "database ",
    x$name,
    " with ",
    n_relations,
    " relation",
    if (n_relations != 1) "s",
    "\n"
  ))
  for (n in seq_len(min(n_relations, max))) {
    rows <- nrow(x$tables[[n]]$df)
    cat(paste0(
      "relation ",
      names(x$tables)[n],
      ": ",
      toString(names(x$tables[[n]]$df)),
      "; ",
      rows,
      " row", if (rows != 1) "s", "\n"
    ))
    keys <- x$tables[[n]]$keys
    n_keys <- length(keys)
    for (k in seq_len(min(n_keys, max))) {
      cat(paste0("  key ", k, ": ", toString(keys[[k]]), "\n"))
    }
    if (max < n_keys)
      cat("  ... and", n_keys - max, "other keys\n")
  }
  if (max < n_relations) {
    cat("... and", n_relations - max, "other relations\n")
  }
  if (length(x$relationships) == 0)
    cat("no relationships\n")
  else {
    cat(paste("relationships:\n"))
    n_relationships <- length(x$relationships)
    for (r in seq_len(min(n_relationships, max))) {
      rel <- x$relationships[[r]]
      cat(paste0(rel[1], ".", rel[2], " -> ", rel[3], ".", rel[4], "\n"))
    }
    if (max < n_relationships)
      cat("... and", n_relationships - max, "other relationships\n")
  }
}
