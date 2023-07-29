#' Decompose a data frame based on given normalised dependencies
#'
#' Decomposes a data frame into several relations, based on the given database
#' schema. It's intended that the data frame satisfies all the functional
#' dependencies implied by the schema, such as if the schema was constructed
#' from the same data frame. If this is not the case, the function will returns
#' an error.
#'
#' If the schema was constructed using approximate dependencies for the same
#' data frame, `decompose` returns an error, to prevent either duplicate records
#' or lossy decompositions. This is temporary: for the next update, we plan to
#' add an option to allow this, or to add "approximate" equivalents of databases
#' and database schemas.
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
#'     \code{attrs_order} element in \code{schema}; these orderings must be the
#'     same.
#'   }
#'
#'   Relations are lists with the following elements:
#'   \itemize{
#'     \item \code{df}, the data.frame containing the data.
#'     \item \code{keys}, a list of character vectors representing
#'     (candidate) keys for the relation. The first key in the list is the
#'     primary key.
#'   }
#' @export
decompose <- function(df, schema, name = NA_character_) {
  relation_names <- names(schema)
  stopifnot(!anyDuplicated(relation_names))
  stopifnot(identical(names(df), attrs_order(schema)))

  inferred_fds <- synthesised_fds(attrs(schema), keys(schema))
  if (length(inferred_fds) > 0L)
    inferred_fds <- unlist(inferred_fds, recursive = FALSE)
  check_fd <- function(df, fd) {
    if (length(fd[[1]]) == 0L) {
      dep_vals <- df[[fd[[2]]]]
      all(vapply(dep_vals, identical, logical(1), dep_vals[1]))
    }else{
      both_proj <- unique(df[, unlist(fd), drop = FALSE])
      key_proj <- unique(both_proj[, fd[[1]], drop = FALSE])
      nrow(key_proj) == nrow(both_proj)
    }
  }
  fds_satisfied <- vapply(
    inferred_fds,
    check_fd,
    logical(1L),
    df = df
  )
  if (!all(fds_satisfied)) {
    stop(paste(
      "df doesn't satisfy functional dependencies in schema:",
      paste(
        vapply(
          inferred_fds[!fds_satisfied],
          \(fd) paste0("{", toString(fd[[1]]), "} -> ", fd[[2]]),
          character(1)
        ),
        collapse = "\n"
      ),
      sep = "\n"
    ))
  }

  relation_list <- Map(
    \(attrs, keys) {
      list(
        # conditional needed to handle 0-attrs case,
        # i.e. decomposing to table_dum and table_dee
        df = if (length(attrs) == 0L)
          df[seq_len(nrow(df) >= 1L), attrs, drop = FALSE]
        else
          unique(df[, attrs, drop = FALSE]),
        keys = keys
      )
    },
    attrs(schema),
    keys(schema)
  )
  relationships <- lapply(
    relationships(schema),
    \(r) {
      c(relation_names[r[[1]][1]], r[[2]], relation_names[r[[1]][2]], r[[2]])
    }
  )
  structure(
    list(
      name = name,
      relations = stats::setNames(relation_list, relation_names),
      relationships = relationships,
      attributes = attrs_order(schema)
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
