#' Create normalised database schemas from functional dependencies
#'
#' Creates a database schema from given functional dependencies, satisfying at
#' least third normal form, using Bernstein's synthesis.
#'
#' This is a wrapper function for applying \code{\link{synthesise}} and
#' \code{\link{cross_reference}}, in order. For creating relation schemas and
#' foreign key references separately, use these functions directly.
#'
#' For details on the synthesis algorithm used, see \code{\link{synthesise}}.
#'
#' @param dependencies a \code{\link{functional_dependency}} object, as given by
#'   \code{\link{discover}}.
#' @param ensure_lossless a logical, TRUE by default. If TRUE, and the
#'   decomposition isn't lossless, an extra relation is added to make the
#'   decomposition lossless.
#' @inheritParams autodb
#'
#' @return A \code{\link{database_schema}} object, containing the synthesis
#'   relation schemas and the created foreign key references.
#' @export
normalise <- function(
  dependencies,
  ensure_lossless = TRUE,
  remove_avoidable = FALSE,
  constants_name = "constants",
  progress = FALSE,
  progress_file = ""
) {
  cross_reference(
    synthesise(
      dependencies,
      ensure_lossless = ensure_lossless,
      remove_avoidable = remove_avoidable,
      constants_name = constants_name,
      progress = progress,
      progress_file = progress_file
    )
  )
}
