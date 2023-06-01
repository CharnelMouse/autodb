#' Create a normalised database from a data frame
#'
#' This is a wrapper function for applying \code{\link{dfd}},
#' \code{\link{normalise}}, \code{\link{cross_reference}}, and
#' \code{\link{decompose}}, in order.
#'
#' Since `decompose` only works with functional dependencies, not approximate
#' dependencies, the accuracy in `dfd` is fixed as 1.
#'
#' @param df a data.frame, containing the data to be normalised.
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
#'   \code{\link{decompose}} for details.
#' @examples
#' # simple example
#' autodb(ChickWeight)
#' @export
autodb <- function(
  df,
  name = NA_character_,
  ensure_lossless = TRUE,
  remove_avoidable = FALSE,
  constants_name = "constants",
  progress = FALSE,
  progress_file = "",
  ...
) {
  report <- reporter(progress, progress_file)

  dfd(df, 1, progress = progress, progress_file = "", ...) |>
    report$op(normalise, "normalising", remove_avoidable, constants_name) |>
    report$op(cross_reference, "cross-referencing", ensure_lossless) |>
    report$op(decompose, "decomposing", df = df, name)
}
