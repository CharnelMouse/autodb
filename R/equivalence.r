#' Test data frames for equivalence under row reordering
#'
#' A convenience function, mostly used to testing that \code{\link{rejoin}}
#' works as intended. It checks that data frames have the same dimensions and
#' column names, with duplicates allowed, then checks they contain the same
#' data. For the latter step, column names are made unique first, so columns
#' with duplicate names must be presented in the same order in both data frames.
#'
#' @param df1,df2 Data frames.
#' @param digits a positive integer, indicating how many significant digits are
#'   to be used for numeric and complex variables. A value of NA results in no
#'   rounding. By default, this uses \code{getOption("digits")}, similarly to
#'   \code{\link{format}}. See the note in \code{\link{print.default}} about
#'   digits >= 16.
#'
#' @return A logical.
#' @export
df_equiv <- function(df1, df2, digits = getOption("digits")) {
  if (!is.na(digits)) {
    df1[] <- lapply(df1, format_if_float, digits)
    df2[] <- lapply(df2, format_if_float, digits)
  }
  identical(dim(df1), dim(df2)) &&
    identical(table(names(df1)), table(names(df2))) && (
      if (ncol(df1) == 0)
        TRUE
      else {
        dfs1 <- stats::setNames(df1, make.unique(names(df1)))
        dfs2 <- stats::setNames(df2, make.unique(names(df2)))
        dfs2 <- dfs2[, names(dfs1), drop = FALSE]
        recs1 <- do.call(Map, `names<-`(c(list, dfs1), NULL))
        recs2 <- do.call(Map, `names<-`(c(list, dfs2), NULL))
        # below is similar to
        # identical(table(match(recs2, recs1)), table(match(recs1, recs1))),
        # without match's undocumented treatment of floats, lists etc.
        recs1_matches <- outer(recs1, recs1, Vectorize(identical))
        recs1_groups <- apply(recs1_matches, 1, match, x = TRUE)
        cross_matches <- outer(recs2, recs1[recs1_groups], Vectorize(identical))
        cross_groups <- apply(cross_matches, 1, match, x = TRUE)
        if (anyNA(cross_groups))
          return(FALSE)
        cross_ref <- recs1_groups[cross_groups]
        identical(table(cross_ref, dnn = NULL), table(recs1_groups, dnn = NULL))
      }
    )
}
