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
