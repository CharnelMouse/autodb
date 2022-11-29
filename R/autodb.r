#' Create a normalised database from a data frame
#'
#' This is a wrapper function for applying \code{\link{dfd}},
#' \code{\link{flatten}}, \code{\link{normalise}},
#' \code{\link{cross_reference}}, and \code{\link{decompose}}, in order.
#'
#' @param df a data.frame, containing the data to be normalised.
#' @param accuracy a numeric in (0, 1], giving the accuracy threshold threshold
#'   required in order to conclude a dependency.
#' @param name a scalar character, giving the name of the database. This name
#'   is used for the resulting graph when using \code{\link{gv.database}}, to
#'   allow for easier combining of graphs into a single diagram if required.
#' @param ensure_lossless a logical, indicating whether to check whether the
#'   normalisation is lossless. If it is not, then an additional relation is
#'   added to the final "database", containing a key for \code{df}. This is
#'   enough to make the normalisation lossless.
#' @param remove_avoidable a logical, indicating whether to remove avoidable
#'   attributes in relations. If so, then an attribute are removed from
#'   relations if the keys can be changed such that it is not needed to preserve
#'   the given functional dependencies.
#' @param constants_name a scalar character, giving the name for any relation
#'   created to store constant attributes. If this is the same as a generated
#'   relation name, it will be changed, with a warning, to ensure that all
#'   relations have a unique name.
#' @param progress a logical, for whether to display progress to the user during
#'   dependency search in \code{\link{dfd}} and normalisation in
#'   \code{\link{normalise}}.
#' @param progress_file a scalar character or a connection. If \code{progress}
#'   is non-zero, determines where the progress is written to, in the same way
#'   as the \code{file} argument for \code{\link[base]{cat}}.
#' @param ... further arguments passed on to \code{\link{dfd}}.
#'
#' @return A database, represented by a list of three elements. See
#'   \code{\link{cross_reference}} for details.
#' @export
autodb <- function(
  df,
  accuracy,
  name = NA_character_,
  ensure_lossless = TRUE,
  remove_avoidable = FALSE,
  constants_name = "constants",
  progress = FALSE,
  progress_file = "",
  ...
) {
  report <- reporter(progress, progress_file)

  dfd(df, accuracy, progress = progress, progress_file = "", ...) |>
    report$op(flatten, "flattening") |>
    report$op(normalise, "normalising", remove_avoidable, constants_name) |>
    report$op(cross_reference, "cross-referencing", ensure_lossless) |>
    report$op(decompose, "decomposing", df = df, name)
}

#' Flatten functional dependency list for normalisation
#'
#' @param dependencies a list, containing functional dependencies as returned by
#'   \code{\link{dfd}}.
#'
#' @return A copy of \code{dependencies}, where the \code{dependencies} element
#'   has been transformed to a list of two-element lists, with the dependency's
#'   determinants in the first element, and its dependent in the second. This is
#'   the format required by \code{\link{normalise}}.
#' @export
flatten <- function(dependencies) {
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
