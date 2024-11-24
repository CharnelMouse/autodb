#' Create a normalised database from a data frame
#'
#' This is a wrapper function for applying \code{\link{normalise}},
#' \code{\link{autoref}}, and \code{\link{decompose}}. This takes a data frame
#' and converts it straight into a database, which is the main intended use case
#' for the package.
#'
#' Since `decompose` only works with functional dependencies, not approximate
#' dependencies, the accuracy in `discover` is fixed as 1.
#'
#' @param df a data.frame, containing the data to be normalised.
#' @param single_ref a logical, FALSE by default. If TRUE, then only one
#'   reference between each relation pair is kept when generating foreign key
#'   references. If a pair has multiple references, the kept reference refers to
#'   the earliest key for the child relation, as sorted by priority order.
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
#'   dependency search in \code{\link{discover}}.
#' @param progress_file a scalar character or a connection. If \code{progress}
#'   is non-zero, determines where the progress is written to, in the same way
#'   as the \code{file} argument for \code{\link[base]{cat}}.
#' @param ... further arguments passed on to \code{\link{discover}}.
#'
#' @return A \code{\link{database}}, containing the data in \code{df} within the
#'   inferred database schema.
#' @examples
#' # simple example
#' autodb(ChickWeight)
#' @export
autodb <- function(
  df,
  single_ref = FALSE,
  ensure_lossless = TRUE,
  remove_avoidable = FALSE,
  constants_name = "constants",
  progress = FALSE,
  progress_file = "",
  ...
) {
  report <- reporter(progress, progress_file)

  discover(df, 1, progress = progress, progress_file = "", ...) |>
    report$op(
      normalise,
      "normalising",
      single_ref,
      ensure_lossless,
      remove_avoidable,
      constants_name
    ) |>
    report$op(decompose, "decomposing", df = df)
}
