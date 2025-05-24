partition_rank <- function(partition) {
  # invariant to whether partition is stripped
  sum(lengths(partition)) - length(partition)
}

stripped_partition_product <- function(sp1, sp2, n_rows) {
  # vectorised union of stripped partitions to replace algorithm from Tane
  if (length(sp1) == 0L || length(sp2) == 0L)
    return(list())
  tab <- rep(NA_integer_, n_rows)
  tab[unlist(sp1)] <- rep(seq_along(sp1), lengths(sp1))
  tab2 <- rep(NA_integer_, n_rows)
  tab2[unlist(sp2)] <- rep(seq_along(sp2), lengths(sp2))
  in_both <- which(!is.na(tab) & !is.na(tab2))
  tab_both <- paste(tab[in_both], tab2[in_both])
  # not pre-factoring with specified levels results in split calling
  # order(tab_both), which can be very slow for long tab_both
  # below is very similar to fsplit, but we've already combined factors
  tab_both <- factor(tab_both, levels = unique(tab_both), ordered = FALSE)
  sp <- split(in_both, tab_both, drop = FALSE)
  unname(sp[lengths(sp) >= 2])
}

stripped_partition_error <- function(lhs_sp, union_sp) {
  # For LHS -> RHS, error is the total number of records to remove
  # for the FD to be satisfied. Each subset in the LHS partition
  # becomes one or more subsets in the combined partition, and all
  # but one of them must be removed for LHS -> RHS to be satisfied,
  # i.e. we keep the largest one.
  # Note that the above is for non-stripped partitions. The algorithm
  # below is that for stripped partitions, as given in the Tane paper,
  # but without scaling by the record count at the end.
  error <- 0L
  Ts <- integer()
  for (cl in union_sp) {
    Ts[cl[1]] <- length(cl)
  }
  for (cl in lhs_sp) {
    m <- 1L
    for (ts in cl) {
      m <- max(m, Ts[ts], na.rm = TRUE)
    }
    error <- error + length(cl) - m
  }
  error
}
