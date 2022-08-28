#' Add foreign key references to a normalised database
#'
#' @param tables a relation set, i.e. named list of tables, as given by
#'   \code{\link{decompose}}.
#'
#' @return A "database", defined as a list of two elements:
#'   \itemize{
#'     \item \code{dataframes} contains the given relation set, \code{tables}.
#'     \item \code{relationships} contains a list of length-four character
#'     vectors, describing a single attribute pair in a foreign key
#'     relationship. In order, the elements give the name of the child table,
#'     the attribute in the child table, the parent table, and the attribute in
#'     the parent table. normalised into the required number of tables.
#'  }
#' @export
cross_reference <- function(tables) {
  relationships <- list()
  stack <- tables
  while (length(stack) > 0) {
    current_df_name <- names(stack)[1]
    current <- stack[[1]]
    child_attrs <- names(current$df)
    stack <- stack[-1]

    for (parent_name in current$parents) {
      parent <- tables[[parent_name]]
      relationships <- c(
        relationships,
        lapply(
          intersect(child_attrs, unlist(parent$keys)),
          \(ind) c(current_df_name, ind, parent_name, ind)
        )
      )
    }
  }

  es <- list(
    dataframes = tables,
    relationships = relationships
  )
  class(es) <- c("database", class(es))
  es
}
