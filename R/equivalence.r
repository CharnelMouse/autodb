#' Test data frames for equivalence under row reordering
#'
#' @param df1,df2 Data frames.
#'
#' @return A logical.
#' @export
df_equiv <- function(df1, df2) {
  identical(dim(df1), dim(df2)) &&
    identical(table(names(df1)), table(names(df2))) &&
    identical(
      nrow(merge(
        stats::setNames(df1, make.unique(names(df1))),
        stats::setNames(df2, make.unique(names(df2))),
        sort = FALSE
      )),
      nrow(df1)
    )
}
