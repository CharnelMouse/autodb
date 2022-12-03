#' Decompose a data frame based on given normalised dependencies
#'
#' Decomposes a data frame into several relations, based on the given database
#' schema. It's intended that the relations are derived from a list of
#' functional dependencies for the same data frame: using anything else will
#' give undefined behaviour.
#'
#' Currently, there is no removal of records that violate dependencies, which
#' usually occurs due to searching for approximate dependencies.
#'
#' @param df a data.frame, containing the data to be normalised.
#' @param schema a database schema with foreign key relationships, as given by
#'   \code{\link{cross_reference}}.
#' @inheritParams autodb
#'
#' @return A database, represented by a named list, with three elements:
#'   \itemize{
#'     \item \code{name} contains the assigned name of the relation set, if any;
#'     \item \code{relations} contains a list of relations in third normal form,
#'     that can reproduce the original data.frame;
#'     \item \code{relationships} contains relationships between the relations,
#'     represented as a list of length-four character vectors. In order, the
#'     elements are the name of the child relation, the name of the linked
#'     attribute in the child relation, the name of the parent relation, and the
#'     name of the linked attribute in the parent relation. Currently, the
#'     attribute is expected to have the same name in both relations.
#'     \item \code{attributes} contains the attribute names in priority order.
#'     This order can be taken from their order in \code{df}, or from the
#'     \code{all_attrs} element in \code{schema}; these orderings must be the
#'     same.
#'   }
#'
#'   Relations are lists with the following elements:
#'   \itemize{
#'     \item \code{df}, the data.frame containing the data.
#'     \item \code{keys}, a list of character vectors representing
#'     (candidate) keys for the relation. The first key in the list is the
#'     primary key.
#'     \item \code{parents}, a character vector containing names of parent
#'     relations, i.e. relations referenced in foreign keys.
#'   }
#' @export
decompose <- function(df, schema, name = NA_character_) {
  relation_names <- schema$relation_names
  stopifnot(!anyDuplicated(relation_names))
  stopifnot(identical(names(df), schema$all_attrs))

  relation_list <- Map(
    \(attrs, keys, parents) {
      list(
        df = unique(df[, attrs, drop = FALSE]),
        keys = keys,
        parents = relation_names[parents]
      )
    },
    schema$attrs,
    schema$keys,
    schema$parents
  )
  relationships <- lapply(
    schema$relationships,
    \(r) {
      c(relation_names[r[[1]][1]], r[[2]], relation_names[r[[1]][2]], r[[2]])
    }
  )
  structure(
    list(
      name = name,
      relations = stats::setNames(relation_list, relation_names),
      relationships = relationships,
      attributes = schema$all_attrs
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
  n_relations <- length(x$relations)
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
    rows <- nrow(x$relations[[n]]$df)
    cat(paste0(
      "relation ",
      names(x$relations)[n],
      ": ",
      toString(names(x$relations[[n]]$df)),
      "; ",
      rows,
      " record", if (rows != 1) "s", "\n"
    ))
    keys <- x$relations[[n]]$keys
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
