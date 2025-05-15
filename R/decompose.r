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
#' @param schema a database schema with foreign key references, such as given by
#'   \code{\link{autoref}}.
#' @param digits a positive integer, indicating how many significant digits are
#'   to be used for numeric and complex variables. A value of \code{NA} results
#'   in no rounding. By default, this uses \code{getOption("digits")}, similarly
#'   to \code{\link{format}}. See the "Floating-point variables" section for
#'   \code{\link{discover}} for why this rounding is necessary for consistent
#'   results across different machines. See the note in
#'   \code{\link{print.default}} about \code{digits >= 16}.
#' @param check a logical, indicating whether to check that \code{df} satisfies
#'   the functional dependencies enforced by \code{schema} before creating the
#'   result. This can find key violations without spending time creating the
#'   result first, but is redundant if \code{df} was used to create
#'   \code{schema} in the first place.
#'
#' @return A \code{\link{database}} object, containing the data in \code{df}
#'   within the database schema given in \code{schema}.
#' @export
decompose <- function(df, schema, digits = getOption("digits"), check = TRUE) {
  stopifnot(!anyDuplicated(names(schema)))
  stopifnot(identical(names(df), attrs_order(schema)))

  if (!is.na(digits))
    df <- df_coarsen(df, digits)

  if (check) {
    inferred_fds <- synthesised_fds(attrs(schema), keys(schema))
    if (length(inferred_fds) > 0L)
      inferred_fds <- unlist(inferred_fds, recursive = FALSE)
    check_fd <- function(df, fd) {
      both_proj <- df_unique(df[, unlist(fd), drop = FALSE])
      key_proj <- df_unique(both_proj[, fd[[1]], drop = FALSE])
      nrow(key_proj) == nrow(both_proj)
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
  }

  create_insert(df, schema, digits = digits) |>
    database(references(schema))
}

create_insert <- function(df, schema, digits = getOption("digits")) {
  if (!is.na(digits))
    df <- df_coarsen(df, digits)
  relations <- stats::setNames(
    Map(
      \(attrs, keys) {
        list(
          df = df_unique(df[, attrs, drop = FALSE]),
          keys = keys
        )
      },
      attrs(schema),
      keys(schema)
    ),
    names(schema)
  )
  relation(relations, attrs_order(schema))
}

drop_primary_dups <- function(df, prim_key) {
  # Reduces a data.frame to have unique values of the attributes in the given
  # primary key. If the other columns are not uniquely determined by the primary
  # key, as can be the case for approximate dependencies, the most common value
  # for each primary key value is used.
  df_lst <- list()

  if (nrow(df_unique(df[, prim_key, drop = FALSE])) == nrow(df))
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
