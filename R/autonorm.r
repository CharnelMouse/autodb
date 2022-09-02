#' Create a normalised database from a data frame
#'
#' This is a wrapper function for applying \code{\link{dfd}},
#' \code{\link{flatten}}, \code{\link{normalise}}, \code{\link{decompose}}, and
#' \code{\link{cross_reference}}, in order.
#'
#' @param df a data.frame, containing the data to be normalised.
#' @param accuracy a numeric in (0, 1], giving the accuracy threshold threshold
#'   required in order to conclude a dependency.
#' @param name a scalar character, giving the name of the database. This name
#'   is used for the resulting graph when using \code{\link{plot_tables}}, to
#'   allow for easier combining of graphs into a single diagram if required.
#' @param check_key a logical, indicating whether to check whether the
#'   normalisation is lossless. If it is not, then an additional table is added
#'   to the final "database", containing a key for \code{df}. This is enough to
#'   make the normalisation lossless.
#' @param progress an integer, for whether to display progress to the user. 0
#'   (default) displays nothing. 1 notes the start of finding each non-constant
#'   attribute's determinant sets. 2 also briefly describes the status of the
#'   search for an attribute's determinant sets when generating new seeds. 3
#'   also gives the status after visiting each candidate determinant set / node.
#' @param progress_file a scalar character or a connection. If \code{progress}
#'   is non-zero, determines where the progress is written to, in the same way
#'   as the \code{file} argument for \code{\link[base]{cat}}.
#' @param ... further arguments passed on to \code{\link{dfd}}.
#'
#' @return A database, represented by a list of three elements. See
#'   \code{\link{cross_reference}} for details.
#' @export
autonorm <- function(
  df,
  accuracy,
  name = NA_character_,
  check_key = TRUE,
  progress = 0L,
  progress_file = "",
  ...
) {
  report <- reporter(progress, progress_file)

  dfd(df, accuracy, progress = progress, progress_file = "", ...) |>
    report$op(flatten, "flattening") |>
    report$op(normalise, "normalising", check_key) |>
    report$op(decompose, "decomposing", df = df, name) |>
    report$op(cross_reference, "cross-referencing")
}

#' Flatten functional dependency list for normalisation
#'
#' @param dependencies a list, containing functional dependencies as returned by
#'   \code{\link{dfd}}.
#'
#' @return A copy of \code{dependencies}, where the \code{dependencies} element
#'   has been transformed to a list of two-element lists, with the dependency's
#'   determinants in the first element, its dependent in the second. This is the
#'   format required by \code{\link{normalise}}.
#' @export
flatten <- function(dependencies) {
  # Takes dependencies grouped by dependent attribute, and returns the
  # dependencies in a flat list with (parent table, parent attr, child table,
  # child attr) format.
  result <- list()
  for (i in seq_along(dependencies$dependencies)) {
    rhs <- names(dependencies$dependencies)[i]
    result <- c(
      result,
      lapply(dependencies$dependencies[[i]], \(lhs) list(lhs, rhs))
    )
  }
  dependencies$dependencies <- result
  dependencies
}
