#' Test data frames for equivalence under row reordering
#'
#' @param df1,df2 Data frames.
#'
#' @return A logical.
#' @export
df_equiv <- function(df1, df2) {
  if (
    !identical(dim(df1), dim(df2)) ||
    !identical(table(names(df1)), table(names(df2)))
  )
    FALSE
  else
    identical(
      `rownames<-`(df1[do.call(order, unname(df1)), ], NULL),
      `rownames<-`(df2[do.call(order, unname(df2)), ], NULL)
    )
}
