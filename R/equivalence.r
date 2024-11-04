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
    identical(table(names(df1)), table(names(df2))) && (
      if (ncol(df1) == 0)
        TRUE
      else {
        dfs1 <- stats::setNames(df1, make.unique(names(df1)))
        dfs2 <- stats::setNames(df2, make.unique(names(df2)))
        dfs2 <- dfs2[, names(dfs1), drop = FALSE]
        recs1 <- do.call(Map, `names<-`(c(list, dfs1), NULL))
        recs2 <- do.call(Map, `names<-`(c(list, dfs2), NULL))
        identical(table(match(recs2, recs1)), table(match(recs1, recs1)))
      }
    )
}
