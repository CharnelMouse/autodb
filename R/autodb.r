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
#' @param digits a positive integer, indicating how many significant digits are
#'   to be used for numeric and complex variables. This is used for both
#'   pre-formatting in \code{\link{discover}}, and for rounding the data before
#'   use in \code{\link{decompose}}, so that the data satisfies the resulting
#'   schema. A value of \code{NA} results in no rounding. By default, this uses
#'   \code{getOption("digits")}, similarly to \code{\link{format}}. See the
#'   "Floating-point variables" section for \code{\link{discover}} for why this
#'   rounding is necessary for consistent results across different machines. See
#'   the note in \code{\link{print.default}} about \code{digits >=
#'   16}.
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
  digits = getOption("digits"),
  single_ref = FALSE,
  ensure_lossless = TRUE,
  remove_avoidable = FALSE,
  constants_name = "constants",
  progress = FALSE,
  progress_file = "",
  ...
) {
  report <- reporter(progress, progress_file)
  if (!is.na(digits)) {
    report(paste(
      "coarsening numerical/complex variables to",
      digits,
      "significant digits"
    ))
    df <- df_coarsen(df, digits)
  }
  fds <- discover(df, digits = NA, progress = progress, progress_file = progress_file, ...)
  report("normalising")
  ds <- normalise(
    fds,
    single_ref = single_ref,
    ensure_lossless = ensure_lossless,
    reduce_attributes = FALSE,
    remove_avoidable = remove_avoidable,
    constants_name = constants_name
  )
  report("decomposing")
  decompose(df, ds, digits = NA, check = FALSE)
}

df_coarsen <- function(x, digits) {
  x[] <- lapply(x, coarsen_if_float, digits)
  x
}

# like format_if_float for discover(), but keeping the original class
# "NA" -> NA to prevent "NAs introduced by coercion" warning
coarsen_if_float <- function(x, digits) {
  if (is.na(digits))
    return(x)
  if (inherits(x, "numeric")) {
    nas <- is.na(x)
    x[!nas] <- as.numeric(format(x[!nas], digits = digits, scientific = TRUE))
    return(x)
  }
  if (inherits(x, "complex")) {
    nas <- is.na(x)
    x[!nas] <- as.complex(format(x[!nas], digits = digits, scientific = TRUE))
    return(x)
  }
  x
}
