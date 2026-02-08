df_join <- function(x, y, by = intersect(names(x), names(y)), by.x = by, by.y = by, ...) {
  # edge cases are for single-column bys
  # if (length(by.x) != 1 || length(by.y) != 1)
    # fails if there are list by columns, do the below for any relevant ones instead
    # return(merge(x, y, by = by, by.x = by.x, by.y = by.y, ...))
  vx <- x[, by.x, drop = FALSE]
  vy <- y[, by.y, drop = FALSE]

  x_matrices <- vapply(vx, is.matrix, logical(1))
  y_matrices <- vapply(vy, is.matrix, logical(1))
  matrices <- which(x_matrices | y_matrices)
  if (length(matrices) > 0) {
    for (n in matrices) {
      if (!is.matrix(vx[[n]]))
        vx[[n]] <- as.matrix(vx[[n]])
      if (!is.matrix(vy[[n]]))
        vy[[n]] <- as.matrix(vy[[n]])
    }
    ncol_mismatch <- vapply(
      matrices,
      \(n) NCOL(vx[[n]]) != NCOL(vy[[n]]),
      logical(1)
    )
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

  x[, by.x[matrices]] <- vx[, matrices, drop = FALSE]
  y[, by.y[matrices]] <- vy[, matrices, drop = FALSE]
  x[, by.x[lists]] <- vx[, lists, drop = FALSE]
  y[, by.y[lists]] <- vy[, lists, drop = FALSE]
  res <- merge(x, y, by = by, by.x = by.x, by.y = by.y, ...)
  if (length(matrices) > 0)
    res[, by.x[matrices]] <- Map(
      \(v, r) v[r, , drop = FALSE],
      vals_mat,
      res[, by.x[matrices], drop = FALSE]
    )
  if (length(lists) > 0)
    res[, by.x[lists]] <- Map(`[`, vals_list, res[, by.x[lists], drop = FALSE])
  res
}
