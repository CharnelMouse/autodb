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
#' @return A database schema with foreign key references, represented by a named
#'   list of three lists and two character vectors, with the first four having
#'   equal length and representing relation schemas:
#'   \itemize{
#'     \item \code{attrs} elements contain the attributes present in the
#'     relation schemas, with attributes in keys given first.
#'     \item \code{keys} elements contain a list of the candidate keys for the
#'     relation schemas.
#'     \item \code{parents} elements contain integers, representing a relation
#'     schema's parent relation schemas by their position in the paired lists.
#'     \item \code{relationships} contains a list of relationships, each
#'     represented by a list containing two elements. In order, the elements
#'     are a two-length integer vector, giving the positions of the child and
#'     parent relation schemas, and a scalar character, giving the name of the
#'     linked attribute in both relation schemas.
#'     \item \code{relation_names} is a character vector, containing the names
#'     of the relation schemas
#'     \item \code{all_attrs} is a character vector, containing all attribute
#'     names in priority order for placement and key ordering, i.e. as ordered
#'     in the original data frame.
#'  }
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
      remove_avoidable = remove_avoidable,
      constants_name = constants_name,
      progress = progress,
      progress_file = progress_file
    ),
    ensure_lossless = ensure_lossless
  )
}
