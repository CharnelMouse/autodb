#' Test data frames for equivalence under row reordering
#'
#' @param df1,df2 Data frames.
#'
#' @return A logical.
#' @export
df_equiv <- function(df1, df2) {
  if (!identical(
    dim(df1), dim(df2) ||
    !identical(table(names(original)) %in% table(names(new)))
  ))
    FALSE
  else
    identical(
      `rownames<-`(new[do.call(order, new), ], NULL),
      `rownames<-`(original[do.call(order, original), ], NULL)
    )
}
