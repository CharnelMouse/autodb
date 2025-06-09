partition_rank <- function(partition) {
  # invariant to whether partition is stripped
  # equivalent to e(X) in Tane paper, i.e. how many
  # records must be removed to make the remaining ones
  # unique
  sum(lengths(partition)) - length(partition)
}

stripped_partition_product <- function(sp1, sp2, n_rows) {
  # vectorised union of stripped partitions to replace algorithm from Tane
  if (length(sp1) == 0L || length(sp2) == 0L)
    return(list())
  tab <- invert_partition(sp1, n_rows)
  tab2 <- invert_partition(sp2, n_rows)
  in_both <- which(!is.na(tab) & !is.na(tab2))
  tab_both <- paste(tab[in_both], tab2[in_both])
  # not pre-factoring with specified levels results in split calling
  # order(tab_both), which can be very slow for long tab_both
  # below is very similar to fsplit, but we've already combined factors
  tab_both <- factor(tab_both, levels = unique(tab_both), ordered = FALSE)
  sp <- split(in_both, tab_both, drop = FALSE)
  unname(sp[lengths(sp) >= 2])
}

stripped_partition_error <- function(lhs_sp, union_sp, n_rows) {
  # For LHS -> RHS, error is the total number of records to remove
  # for the FD to be satisfied. Each subset in the LHS partition
  # becomes one or more subsets in the combined partition, and all
  # but one of them must be removed for LHS -> RHS to be satisfied,
  # i.e. we keep the largest one.
  # Note that the above is for non-stripped partitions. The algorithm
  # below is that for stripped partitions, as given in the Tane paper,
  # but without scaling by the record count at the end.
  # Equivalent to e(X -> Y) in Tane paper.
  tab_both <- rep(NA_integer_, n_rows)
  tab_both[vapply(union_sp, `[[`, integer(1), 1)] <- lengths(union_sp)
  minimal_removal_counts <- vapply(
    lhs_sp,
    \(cl) length(cl) - max(c(1L, tab_both[cl]), na.rm = TRUE),
    integer(1)
  )
  sum(minimal_removal_counts)
}

value_partition_error <- function(rhs_value_lhs_partition, n_rows) {
  # Partition is RHS lookup values, split by LHS (non-stripped).
  # Equivalent to e(X -> Y) in Tane paper.
  majority_size <- function(x) max(tabulate(x))
  majorities_total <- sum(vapply(
    rhs_value_lhs_partition,
    majority_size,
    integer(1)
  ))
  n_rows - majorities_total
}

invert_partition <- function(sp, n_rows) {
  inv <- rep(NA_integer_, n_rows)
  inv[unlist(sp)] <- rep(seq_along(sp), lengths(sp))
  inv
}
