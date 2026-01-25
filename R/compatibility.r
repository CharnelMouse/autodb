# This contains functions meant to keep the package working in older versions
# of R.

#' Determine Duplicate Elements
#'
#' \code{\link{duplicated}} "determines which elements of a vector or data frame
#' are duplicates of elements with smaller subscripts, and returns a logical
#' vector indicating which elements (rows) are duplicates". However, as of R
#' 4.1, calling this on a data frame with zero columns always returns an empty
#' logical vector. This has repercussions on other functions that use
#' `duplicated`, such as \code{\link{unique}} and \code{\link{anyDuplicated}}.
#' These functions add zero-column data frames as a special case.
#'
#' Additionally, if a data frame has one column, duplicated is called directly
#' on that column instead. This causes issues if that one column is a matrix
#' with zero columns, since it returns an empty matrix instead of an empty
#' vector. These functions treat such a case by comparing the matrix rows
#' instead.
#'
#' @param x a data frame.
#' @inheritParams base::duplicated.data.frame
#' @return For \code{df_duplicated}, a logical vector with one element for each
#'   row.
#' @export
#' @seealso \code{\link{df_rbind}}
df_duplicated <- function(x, incomparables = FALSE, fromLast = FALSE, ...) {
  if (ncol(x) == 0)
    return(duplicated(
      rep(1L, nrow(x)),
      incomparables = incomparables,
      fromLast = fromLast,
      ...
    ))
  if (ncol(x) == 1 && inherits(x[[1]], "array"))
    arr_duplicated(x[[1]])
  else
    duplicated(x, incomparables = incomparables, fromLast = fromLast, ...)
}

arr_duplicated <- function(x, incomparables = FALSE, fromLast = FALSE, ...) {
  duplicated.default(asplit(x, 1))
}

#' @rdname df_duplicated
#' @return For \code{df_unique}, a data frame is returned with the same
#'   columns, but possible fewer rows (and with row names from the first
#'   occurrences of the unique rows).
#' @export
df_unique <- function(x, incomparables = FALSE, fromLast = FALSE, ...) {
  if (!isFALSE(incomparables))
    .NotYetUsed("incomparables != FALSE")
  x[!df_duplicated(x, fromLast = fromLast, ...), , drop = FALSE]
}

#' @rdname df_duplicated
#' @return For \code{df_anyDuplicated}, an integer or real vector of length
#'   one with value the 1-based index of the first duplicate if any, otherwise
#'   0.
#' @export
df_anyDuplicated <- function(x, incomparables = FALSE, fromLast = FALSE, ...) {
  if (ncol(x) == 0)
    return(anyDuplicated(
      rep(1L, nrow(x)),
      incomparables = incomparables,
      fromLast = fromLast,
      ...
    ))
  if (ncol(x) == 1 && inherits(x[[1]], "array"))
    arr_anyDuplicated(x[[1]])
  else
    anyDuplicated(x, incomparables = incomparables, fromLast = fromLast, ...)
}

arr_anyDuplicated <- function(x, incomparables = FALSE, fromLast = FALSE, ...) {
  anyDuplicated.default(asplit(x, 1))
}

#' Combine R Objects by Rows or Columns
#'
#' \code{\link{rbind}} takes "a sequence of vector, matrix or data-frame
#' arguments", and combines by rows for the latter. However, as of R 4.1,
#' calling this on data frame with zero columns always returns zero rows, due to
#' the issue mentioned for \code{\link{df_duplicated}}. This function adds
#' zero-column data frames as a special case.
#'
#' @param ... data frames.
#'
#' @return A data frame containing the \code{...} arguments row-wise.
#' @export
#' @seealso \code{\link{df_duplicated}}
df_rbind <- function(...) {
  dfs <- list(...)
  dfs <- lapply(dfs, \(df) cbind(df, dummy = seq_len(nrow(df))))
  res <- do.call(rbind, dfs)
  res[, -ncol(res), drop = FALSE]
}

#' @rdname df_duplicated
#' @param use_rownames a logical, FALSE by default, indicating whether row
#'   values should keep the row names from \code{x}. Defaults to FALSE.
#' @param use_colnames a logical, FALSE by default, indicating whether row
#'   values should keep the column names from \code{x} for their elements.
#'   Defaults to FALSE.
#' @return For \code{df_records}, a list of the row values in \code{x}. This is
#'   based on a step in \code{\link{duplicated.data.frame}}. However, for data
#'   frames with zero columns, special handling returns a list of empty row
#'   values, one for each row in \code{x}. Without special handling, this step
#'   returns an empty list. This was the cause for \code{\link{duplicated}}
#'   returning incorrect results for zero-column data frames in older versions
#'   of R.
#' @examples
#' # row values for a 5x0 data frame
#' x <- data.frame(a = 1:5)[, FALSE, drop = FALSE]
#' do.call(Map, unname(c(list, x))) # original step returns empty list
#' df_records(x) # corrected version preserves row count
#' @export
df_records <- function(x, use_rownames = FALSE, use_colnames = FALSE) {
  if (ncol(x) == 0)
    return(rep(list(list()), nrow(x)))
  args <- c(list, x)
  if (!use_colnames)
    names(args) <- NULL
  res <- do.call(Map, args)
  if (use_rownames)
    names(res) <- row.names(x)
  res
}
