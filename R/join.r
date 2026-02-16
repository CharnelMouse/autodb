df_join <- function(x, y, by = intersect(names(x), names(y)), by.x = by, by.y = by, ...) {
  # edge cases are for single-column bys
  # if (length(by.x) != 1 || length(by.y) != 1)
    # fails if there are list by columns, do the below for any relevant ones instead
    # return(merge(x, y, by = by, by.x = by.x, by.y = by.y, ...))
  vx <- x[, by.x, drop = FALSE]
  vy <- y[, by.y, drop = FALSE]

  x_dfs <- vapply(vx, is.data.frame, logical(1))
  y_dfs <- vapply(vy, is.data.frame, logical(1))
  df_els <- x_dfs | y_dfs
  any_dfs <- x_dfs | y_dfs
  all_dfs <- x_dfs & y_dfs
  if (any(any_dfs & !all_dfs))
    stop("non-compatible elements")
  df_els <- which(all_dfs)
  if (length(df_els) > 0) {
    name_mismatch <- vapply(
      df_els,
      \(n) !identical(names(vx[[n]]), names(vy[[n]])),
      logical(1)
    )
    if (any(name_mismatch))
      stop("non-compatible elements")
    vals_df <- lapply(
      df_els,
      \(n) df_rbind(vx[[n]], vy[[n]])
    )
    stopifnot(all(vapply(vals_df, is.data.frame, logical(1))))
    indices_vec <- lapply(vals_df, lookup_indices) |>
      as.data.frame()
    for (n in df_els) {
      vx[[n]] <- indices_vec[[match(n, df_els)]][seq_len(nrow(x))]
      vy[[n]] <- indices_vec[[match(n, df_els)]][nrow(x) + seq_len(nrow(y))]
    }
  }

  x_matrices <- vapply(vx, is.matrix, logical(1))
  y_matrices <- vapply(vy, is.matrix, logical(1))
  matrices <- which(x_matrices | y_matrices)
  if (length(matrices) > 0) {
    for (n in matrices) {
      if (!is.matrix(vx[[n]]))
        stop("non-compatible elements")
      if (!is.matrix(vy[[n]]))
        stop("non-compatible elements")
    }
    ncol_mismatch <- vapply(
      matrices,
      \(n) NCOL(vx[[n]]) != NCOL(vy[[n]]),
      logical(1)
    )
    if (any(ncol_mismatch))
      stop("non-compatible elements")
    vx[, matrices[ncol_mismatch]] <- lapply(
      vx[, matrices[ncol_mismatch], drop = FALSE],
      apply,
      1,
      identity,
      simplify = FALSE
    )
    vy[, matrices[ncol_mismatch]] <- lapply(
      vy[, matrices[ncol_mismatch], drop = FALSE],
      apply,
      1,
      identity,
      simplify = FALSE
    )
    matrices <- matrices[!ncol_mismatch]
    vals_mat <- rbind(
      vx[, matrices, drop = FALSE],
      stats::setNames(vy[, matrices, drop = FALSE], names(vx)[matrices]),
      make.row.names = FALSE
    )
    vals_mat <- lapply(vals_mat, as.matrix) # undoing coercion by rbind
    vals_mat[] <- lapply(
      vals_mat,
      \(x, y) {
        dimnames(x) <- NULL
        x
      }
    )
    stopifnot(all(vapply(vals_mat, is.matrix, logical(1))))
    vals_vec <- lapply(vals_mat, apply, 1, identity, simplify = FALSE)
    indices_vec <- lapply(vals_vec, lookup_indices) |>
      as.data.frame()
    vx[, matrices] <- indices_vec[seq_len(nrow(x)), , drop = FALSE]
    vy[, matrices] <- indices_vec[nrow(x) + seq_len(nrow(y)), , drop = FALSE]
  }

  x_lists <- vapply(vx, is.list, logical(1))
  y_lists <- vapply(vy, is.list, logical(1))
  lists <- which(x_lists | y_lists)
  if (length(lists) > 0) {
    for (n in lists) {
      if (!is.list(vx[[n]]))
        stop("non-compatible elements")
      if (!is.list(vy[[n]]))
        stop("non-compatible elements")
    }
    vals_list <- rbind(
      vx[, lists, drop = FALSE],
      stats::setNames(vy[, lists, drop = FALSE], names(vx)[lists]),
      make.row.names = FALSE
    )
    for (n in lists) {
      vals <- c(vx[[n]], vy[[n]])
      inds <- lookup_indices(vals)
      vx[[n]] <- inds[seq_len(nrow(x))]
      vy[[n]] <- inds[nrow(x) + seq_len(nrow(y))]
    }
  }

  x[, by.x[df_els]] <- vx[, df_els, drop = FALSE]
  y[, by.y[df_els]] <- vy[, df_els, drop = FALSE]
  x[, by.x[matrices]] <- vx[, matrices, drop = FALSE]
  y[, by.y[matrices]] <- vy[, matrices, drop = FALSE]
  x[, by.x[lists]] <- vx[, lists, drop = FALSE]
  y[, by.y[lists]] <- vy[, lists, drop = FALSE]
  res <- merge(x, y, by = by, by.x = by.x, by.y = by.y, ...)
  class(res) <- class(x)
  if (length(df_els) > 0) {
    for (n in df_els)
      res[[n]] <- vals_df[[match(n, df_els)]][res[[n]], , drop = FALSE]
  }
  if (length(matrices) > 0) {
    for (n in seq_along(vals_mat)) {
      res[[by.x[[matrices[[n]]]]]] <- vals_mat[[n]][
        res[[by.x[[matrices[[n]]]]]],
        ,
        drop = FALSE
      ]
    }
  }
  if (length(lists) > 0)
    res[, by.x[lists]] <- Map(`[`, vals_list, res[, by.x[lists], drop = FALSE])
  res
}
