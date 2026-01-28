df_join <- function(x, y, by = intersect(names(x), names(y)), by.x = by, by.y = by, ...) {
  # edge cases are for single-column bys
  if (length(by.x) != 1 || length(by.y) != 1)
    return(merge(x, y, by = by, by.x = by.x, by.y = by.y, ...))
  vx <- x[[by.x]]
  vy <- y[[by.y]]
  if (is.matrix(vx) || is.matrix(vy)) {
    if (!is.matrix(vx))
      vx <- as.matrix(vx)
    if (!is.matrix(vy))
      vy <- as.matrix(vy)
    if (NCOL(vx) != NCOL(vy)) {
      x2 <- x
      y2 <- y
      x2[[by.x]] <- seq_len(nrow(x2))
      y2[[by.y]] <- nrow(x2) + seq_len(nrow(y2))
      return(merge(x2, y2, by = by, by.x = by.x, by.y = by.y, ...))
    }
    vals_mat <- rbind(vx, vy)
    vals_vec <- asplit(vals_mat, 1)
    inds <- lookup_indices(vals_vec)
    x2 <- x
    y2 <- y
    x2[[by.x]] <- inds[seq_len(nrow(x))]
    y2[[by.y]] <- inds[nrow(x) + seq_len(nrow(y))]
    res <- merge(x2, y2, by.x = by.x, by.y = by.y, ...)
    res[[by.x]] <- vx[res[[by.x]], , drop = FALSE]
    return(res)
  }
  if (is.list(vx) || is.list(vy)) {
    vals <- c(vx, vy)
    inds <- lookup_indices(vals)
    x2 <- x
    y2 <- y
    x2[[by.x]] <- inds[seq_len(nrow(x))]
    y2[[by.y]] <- inds[nrow(x) + seq_len(nrow(y))]
    res <- merge(x2, y2, by.x = by.x, by.y = by.y, ...)
    res[[by.x]] <- vx[res[[by.x]]]
    return(res)
  }
  merge(x, y, by = by, by.x = by.x, by.y = by.y, ...)
}
