# This contains functions meant to keep the package working in older versions
# of R.

#' Determine Duplicate Elements
#'
#' \code{\link{duplicated}} " determines which elements of a vector or data
#' frame are duplicates of elements with smaller subscripts, and returns a
#' logical vector indicating which elements (rows) are duplicates". However, as
#' of R 4.1, calling this on a data frame with zero columns always returns an
#' empty logical vector. This has repercussions on other functions that use
#' `duplicated`, such as `unique` and `anyDuplicated`. These functions add
#' zero-column data frames as a special case.
#'
#' @param x a data frame.
#' @inheritParams base::duplicated.data.frame
#' @return For \code{df_duplicated}, a logical vector with one element for each
#'   row.
#' @export
#' @seealso \code{\link{df_rbind}}
df_duplicated <- function(x, incomparables = FALSE, fromLast = FALSE, ...) {
  if (ncol(x) == 0) {
    ints <- rep(1L, nrow(x))
    duplicated(ints, incomparables = incomparables, fromLast = fromLast, ...)
  }else{
    duplicated(x, incomparables = incomparables, fromLast = fromLast, ...)
  }
}

#' @rdname df_duplicated
#' @return For \code{unique_duplicated}, a data frame is returned with the same
#'   columns, but possible fewer rows (and with row names from the first
#'   occurrences of the unique rows).
#' @export
df_unique <- function(x, incomparables = FALSE, fromLast = FALSE, ...) {
  if (!isFALSE(incomparables))
    .NotYetUsed("incomparables != FALSE")
  x[!df_duplicated(x, fromLast = fromLast, ...), , drop = FALSE]
}

#' @rdname df_duplicated
#' @return For \code{unique_anyDuplicated}, an integer or real vector of length
#'   one with value the 1-based index of the first duplicate if any, otherwise
#'   0.
#' @export
df_anyDuplicated <- function(x, incomparables = FALSE, fromLast = FALSE, ...) {
  if (ncol(x) == 0) {
    ints <- rep(1L, nrow(x))
    anyDuplicated(ints, incomparables = incomparables, fromLast = fromLast, ...)
  }else{
    anyDuplicated(x, incomparables = incomparables, fromLast = fromLast, ...)
  }
}

#' Combine R Objects by Rows or Columns
#'
#' \code{\link{nrow}} takes "a sequence of vector, matrix or data-frame
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
