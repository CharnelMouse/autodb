#' Create a relation from a data frame
#'
#' This is a wrapper function for applying \code{\link{discover_keys}},
#' \code{\link{relation_schema}}, \code{\link{create}}, and
#' \code{\link{insert}}. This takes a data frame and adds its keys, resulting in
#' a single relation. If only the keys are required, this can be much quicker
#' than running \code{\link{autodb}}.
#'
#' @param digits a positive integer, indicating how many significant digits are
#'   to be used for numeric and complex variables. This is used for both
#'   pre-formatting in \code{\link{discover_keys}}, and for rounding the data
#'   before use in \code{\link{insert}}, so that the data satisfies the
#'   resulting schema. A value of \code{NA} results in no rounding. By default,
#'   this uses \code{getOption("digits")}, similarly to \code{\link{format}}.
#'   See the "Floating-point variables" section for \code{\link{discover}} for
#'   why this rounding is necessary for consistent results across different
#'   machines. See the note in \code{\link{print.default}} about \code{digits >=
#'   16}.
#' @param ... further arguments passed on to \code{\link{discover_keys}}.
#' @inheritParams autodb
#'
#' @return A \code{\link{relation}} of length 1, containing the data in
#'   \code{df} and its keys.
#' @examples
#' # simple example
#' autokey(ChickWeight)
#' @export
autokey <- function(
  df,
  keep_rownames = FALSE,
  digits = getOption("digits"),
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
  if (!isFALSE(keep_rownames)) {
    nm <- if (isTRUE(keep_rownames)) "row" else keep_rownames[[1]]
    df <- cbind(stats::setNames(data.frame(rownames(df)), nm), df)
  }
  keys <- discover_keys(
    df,
    digits = NA,
    progress = progress,
    progress_file = progress_file,
    ...
  )
  report("adding keys")
  schema <- relation_schema(list(data = list(names(df), keys)), names(df))
  create_insert(df, schema, digits = digits, check = FALSE)
}
