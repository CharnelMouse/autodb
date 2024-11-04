#' Test data frames for equivalence under row reordering
#'
#' A convenience function, mostly used to testing that \code{\link{rejoin}}
#' works as intended. It checks that data frames have the same dimensions and
#' column names, with duplicates allowed, then checks they contain the same
#' data. For the latter step, column names are made unique first, so columns
#' with duplicate names must be presented in the same order in both data frames.
#'
#' @param df1,df2 Data frames.
#'
#' @return A logical.
#' @export
df_equiv <- function(df1, df2) {
  identical(dim(df1), dim(df2)) &&
    identical(table(names(df1)), table(names(df2))) &&
    identical(
      nrow(df_join(
        stats::setNames(df1, make.unique(names(df1))),
        stats::setNames(df2, make.unique(names(df2))),
        sort = FALSE
      )),
      nrow(df1)
    )
}
