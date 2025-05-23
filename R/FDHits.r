FDHits <- function(
  lookup,
  method = c("Sep", "Joint"),
  determinants = seq_along(lookup),
  dependants = seq_along(lookup),
  detset_limit = ncol(lookup) - 1L,
  report = reporter(report = FALSE, con = "", new = TRUE)
) {
  method <- match.arg(method)
  if (ncol(lookup) == 0)
    return(functional_dependency(list(), character()))
  report$stat("calculating single-attribute PLIs")
  plis <- lapply(lookup, pli)
  report$stat("sampling difference sets")
  D <- lapply(plis, sample_diffsets, lookup) |>
    unlist(recursive = FALSE) |>
    unique()
  report$stat(with_number(length(D), "initial diffset", "\n", "s\n"))
  switch(
    method,
    Sep = FDHitsSep(lookup, determinants, dependants, detset_limit, D, report),
    Joint = FDHitsJoint(lookup, determinants, dependants, detset_limit, D, report)
  )
}

FDHitsSep <- function(lookup, determinants, dependants, detset_limit, D, report) {
  attrs <- names(lookup)
  res <- list()
  n_visited <- 0L
  partition_handler <- bitset_partition_handler(lookup)
  partition_cache <- list(
    key = as.character(seq_along(attrs)),
    value = lapply(unname(as.list(lookup)), pli)
  )
  D <- lapply(D, partition_handler$as.bitset)
  for (A in dependants) {
    report$stat(paste("dependant", attrs[A]))
    A_bitset <- partition_handler$as.bitset(A)
    rest <- partition_handler$as.bitset(determinants[determinants != A])
    empty <- partition_handler$as.bitset(integer())
    return_stack <- list(list(empty, rest, A_bitset))
    visited <- character()
    while (length(return_stack) > 0) {
      node <- return_stack[[1]]
      return_stack <- return_stack[-1]
      node_string <- paste0(
        paste(node[[1]], collapse = ""),
        paste(node[[2]], collapse = ""),
        paste(node[[3]], collapse = "")
      )
      if (is.element(node_string, visited))
        stop("node ", node_string, " already visited")
      attr_res <- FDHitsSep_visit(
        node[[1]],
        node[[2]],
        node[[3]],
        node_string,
        D,
        lookup,
        report = report,
        partition_handler,
        partition_cache
      )
      visited <- c(visited, node_string)
      res <- c(res, attr_res[[1]])
      D <- attr_res[[2]]
      new_nodes <- attr_res[[3]]
      partition_cache <- attr_res[[4]]
      return_stack <- c(
        new_nodes[vapply(
          new_nodes,
          \(node) partition_handler$key_size(node[[1]]) <= detset_limit,
          logical(1)
        )],
        return_stack
      )
    }
    n_visited <- n_visited + length(visited)
  }
  report$stat(paste0(
    "\n",
    "FDHitsSep complete",
    "\n",
    with_number(length(D), "final diffset", "", "s"),
    "\n",
    with_number(n_visited, "node", " visited", "s visited"),
    "\n",
    with_number(length(partition_cache$key), "partition", " cached", "s cached")
  ))
  res <- lapply(res, lapply, \(x) attrs[as.logical(rawToBits(x))])
  functional_dependency(res, attrs)
}

FDHitsJoint <- function(lookup, determinants, dependants, detset_limit, D, report) {
  attrs <- names(lookup)
  res <- list()
  visited <- character()
  partition_handler <- bitset_partition_handler(lookup)

  W_bitset <- partition_handler$as.bitset(dependants)
  V_bitset <- partition_handler$as.bitset(determinants)
  D <- lapply(D, partition_handler$as.bitset)
  empty <- partition_handler$as.bitset(integer())
  return_stack <- list(list(empty, V_bitset, W_bitset))

  partition_cache <- list(
    key = as.character(seq_along(attrs)),
    value = lapply(unname(as.list(lookup)), pli)
  )
  while (length(return_stack) > 0) {
    node <- return_stack[[1]]
    return_stack <- return_stack[-1]
    node_string <- paste0(
      paste(node[[1]], collapse = ""),
      paste(node[[2]], collapse = ""),
      paste(node[[3]], collapse = "")
    )
    if (is.element(node_string, visited))
      stop("node ", node_string, " already visited")
    attr_res <- FDHitsJoint_visit(
      node[[1]],
      node[[2]],
      node[[3]],
      node_string,
      D,
      lookup,
      report = report,
      partition_handler,
      partition_cache
    )
    visited <- c(visited, node_string)
    res <- c(res, attr_res[[1]])
    D <- attr_res[[2]]
    new_nodes <- attr_res[[3]]
    return_stack <- c(
      new_nodes[vapply(
        new_nodes,
        \(node) partition_handler$key_size(node[[1]]) <= detset_limit,
        logical(1)
      )],
      return_stack
    )
  }
  report$stat(paste0(
    "\n",
    "FDHitsJoint complete",
    "\n",
    with_number(length(D), "final diffset", "", "s"),
    "\n",
    with_number(length(visited), "node", " visited", "s visited"),
    "\n",
    with_number(length(partition_cache$key), "partition", " cached", "s cached")
  ))
  res <- lapply(res, lapply, \(x) attrs[as.logical(rawToBits(x))])
  # split up into one-dependant FDs
  res <- lapply(res, \(x) lapply(x[[2]], \(dependant) list(x[[1]], dependant))) |>
    unlist(recursive = FALSE)
  functional_dependency(res, attrs)
}

FDHitsSep_visit <- function(
  S_bitset,
  V_bitset,
  A_bitset,
  node_string,
  D_bitsets,
  lookup,
  report,
  partition_handler,
  partition_cache,
  visited = character()
) {
  # pruning
  for (C in individual_bitsets(S_bitset)) {
    # no critical edge for C
    # => C is redundant in S for A
    # => S isn't irreducible for A
    crits <- critical(C, A_bitset, S_bitset, D_bitsets)
    if (length(crits) == 0)
      return(list(list(), D_bitsets, list(), partition_cache))
    for (B in individual_bitsets(V_bitset)) {
      # remove B from V if ∃ C∈S ∀ E∈critical(C,A,S): B∈E,
      # i.e. adding B to S would make some C in S redundant WRT A
      # does not check for B being redundant if added
      if (all(vapply(D_bitsets[crits], \(Db) any((Db & B) != 0), logical(1))))
        V_bitset <- xor(V_bitset, B)
    }
  }
  # validation at the leaves
  uncovered <- D_bitsets[uncov_sep(S_bitset, A_bitset, D_bitsets)]
  if (length(uncovered) == 0) {
    refinement <- partition_handler$refine(A_bitset, S_bitset, partition_cache)
    refined_partitions <- refinement[[1]]
    relevant_Spli <- refinement[[2]]
    partition_cache <- refinement[[3]]
    if (validate(refined_partitions, relevant_Spli))
      return(list(list(list(S_bitset, A_bitset)), D_bitsets, list(), partition_cache))
    stopifnot(length(relevant_Spli) > 0)
    ds <- new_diffset(relevant_Spli, refined_partitions, lookup)
    dsl <- list(ds)
    ds2 <- sample_diffsets(relevant_Spli, lookup)
    added <- setdiff(lapply(c(dsl, ds2), partition_handler$as.bitset), D_bitsets)
    stopifnot(length(added) > 0)
    uncovered <- added[uncov_sep(S_bitset, A_bitset, added)]
    D_bitsets <- c(D_bitsets, added)
  }
  # branching
  if (length(uncovered) == 0) {
    stop("edge selection impossible at ", node_string)
  }
  E_bitset <- uncovered[[sample_minheur_sep(uncovered, V_bitset)]]
  Bs_bitsets <- individual_bitsets(E_bitset & V_bitset)
  res <- list()
  # rev() differs from the description in the paper, but the authors gave it as
  # a fix in private correspondence; I'll add a reference when they've published
  # the new work
  new_nodes <- lapply(
    rev(seq_along(Bs_bitsets)),
    \(n) {
      b <- Bs_bitsets[[n]]
      rem <- Reduce(`|`, Bs_bitsets[seq_len(n)])
      list(S_bitset | b, V_bitset & !rem, A_bitset)
    }
  )
  list(res, D_bitsets, new_nodes, partition_cache)
}

FDHitsJoint_visit <- function(
  S_bitset,
  V_bitset,
  W_bitset,
  node_string,
  D_bitsets,
  lookup,
  report,
  partition_handler,
  partition_cache
) {
  # pruning
  critical_edges <- list()
  for (A in individual_bitsets(W_bitset)) {
    for (C in individual_bitsets(S_bitset)) {
      # no critical edge for C
      # => C is redundant in S for (some part of) W
      # => S isn't irreducible for (some part of) W
      crits <- critical(C, A, S_bitset, D_bitsets)
      if (length(crits) == 0) {
        W_bitset <- xor(W_bitset, A)
        critical_edges[[paste(A, collapse = "")]] <- NULL
        break
      }else{
        critical_edges[[paste(A, collapse = "")]][[paste(C, collapse = "")]] <- D_bitsets[crits]
      }
    }
  }
  if (all(W_bitset == 0)) {
    return(list(list(), D_bitsets, list()))
  }
  for (B in individual_bitsets(V_bitset)) {
    # remove if ∀ A∈W ∃ C∈S ∀ E∈critical(C,A,S): B∈E,
    # i.e. adding B to S would make some C in S redundant WRT all of W
    # does not check for B being redundant if added
    if (all(vapply(
      individual_bitsets(W_bitset),
      \(A) any(vapply(
        individual_bitsets(S_bitset),
        \(C) all(vapply(
          critical_edges[[paste(A, collapse = "")]][[paste(C, collapse = "")]],
          \(E) all((B & E) == B),
          logical(1)
        )),
        logical(1)
      )),
      logical(1)
    ))) {
      V_bitset <- xor(V_bitset, B)
    }
  }
  # validation at the leaves
  uncovered_bitsets <- D_bitsets[uncov_joint(S_bitset, W_bitset, D_bitsets)]
  if (length(uncovered_bitsets) == 0) {
    refinement <- partition_handler$refine(W_bitset, S_bitset, partition_cache)
    refined_partitions <- refinement[[1]]
    relevant_Spli <- refinement[[2]]
    partition_cache <- refinement[[3]]
    if (validate(refined_partitions, relevant_Spli))
      return(list(list(list(S_bitset, W_bitset)), D_bitsets, list()))
    stopifnot(length(relevant_Spli) > 0)
    ds <- new_diffset(relevant_Spli, refined_partitions, lookup)
    dsl <- list(ds)
    ds2 <- sample_diffsets(relevant_Spli, lookup)
    added_bitsets <- setdiff(lapply(c(dsl, ds2), partition_handler$as.bitset), D_bitsets)
    stopifnot(length(added_bitsets) > 0)
    uncovered_bitsets <- added_bitsets[uncov_joint(S_bitset, W_bitset, added_bitsets)]
    D_bitsets <- c(D_bitsets, added_bitsets)
  }
  # branching
  if (length(uncovered_bitsets) == 0) {
    stop("edge selection impossible at ", node_string)
  }
  E_bitset <- sample_minheur_joint(uncovered_bitsets, V_bitset, W_bitset)
  Bs_bitset <- E_bitset & V_bitset
  Bs_bitsets <- individual_bitsets(Bs_bitset)
  res <- list()
  # rev() differs from the description in the paper, but the authors gave it as
  # a fix in private correspondence; I'll add a reference when they've published
  # the new work
  new_nodes <- c(
    if (any((W_bitset & E_bitset) != W_bitset))
      list(list(S_bitset, V_bitset, W_bitset & !E_bitset)), # mu_0
    lapply( # mu_i
      rev(seq_along(Bs_bitsets)),
      \(n) {
        B <- Bs_bitsets[[n]]
        list(
          S_bitset | B,
          V_bitset & !Reduce(`|`, Bs_bitsets[seq_len(n)]),
          W_bitset & E_bitset & !B
        )
      }
    )
  )
  list(res, D_bitsets, new_nodes)
}

critical <- function(C_bitset, A_bitset, S_bitset, D_bitsets) {
  # Set that contains the critical edges for C WRT S in the subhypergraph H_A,
  # i.e. using only elements of D that include A.
  # If every C is S has a critical edge, then S is a minimal hitting set.
  # An edge is critical for C in S WRT S if it contains C, and no other vertex
  # from S, i.e. E /\ S = {C}.
  # E and S are already sets, so S[match(E, S, 0L)] is quicker than
  # intersect(E, S), since it skips removing duplicates.
  which(vapply(
    D_bitsets,
    \(E) any((E & A_bitset) != 0) && all((S_bitset & E) == C_bitset),
    logical(1)
  ))
}

uncov_joint <- function(S, W, D) {
  # set of hyperedges that contain a vertex from W but nothing from S
  vapply(D, \(E) any((E & W) > 0) && all((S & E) == 0), logical(1))
}

uncov_sep <- function(S_bitset, A_bitset, D_bitset) {
  # set of hyperedges that contain a vertex from W but nothing from S
  vapply(D_bitset, \(E) any((E & A_bitset) > 0) && all((S_bitset & E) == 0), logical(1))
}

sample_minheur_joint <- function(set_bitsets, V_bitset, W_bitset) {
  if (length(set_bitsets) == 0)
    stop("can't sample edge from empty set")
  heuristics <- vapply(
    set_bitsets,
    function(E) sum(rawToBits(E & V_bitset) == 1) + sum(rawToBits(W_bitset & !E) == 1),
    integer(1)
  )
  set_bitsets[[which.min(heuristics)]]
}

sample_minheur_sep <- function(set_bitsets, V_bitset) {
  # For FDHitsSep, |W| = 1 and W /\ E is empty, so second part
  # of heuristic in sample_minheur_joint is redundant
  if (length(set_bitsets) == 0)
    stop("can't sample edge from empty set")
  heuristics <- vapply(
    set_bitsets,
    function(E) sum(as.logical(rawToBits(E & V_bitset))),
    integer(1)
  )
  which.min(heuristics)
}

bitset_partition_handler <- function(df) {
  # handles lookup and calculation of stripped partitions, without calling code
  # having to worry about handling the store, or the lookup table df itself.
  # the partitions_ui handles the former.
  # general partitions UI elements:
  # set: original values, that subset the original lookup table df
  # key: transformed set; what is usually passed around
  # hash: key transformed into a string for lookup (lookup uses hash, not key)
  # lookup_hash: hash -> partitions -> option<index>
  # bitset partitions UI elements:
  # set: a no-duplicate integer vector, for which columns of df to use
  # key: a bitset
  # hash: the bitset, with its bytes pasted together to make a string
  bitlen <- 8*ceiling(ncol(df)/8)
  unkey <- function(key) which(rawToBits(key) == 1)
  bitset_partitions_ui <- list(
    key = function(set) bitset(set, bitlen),
    hash = function(key) to_partition_node_char(key),
    unkey = function(key) unkey(key),
    key_size = function(key) length(unkey(key)),
    decompose_key = function(key) individual_bitsets(key),
    key_children = function(key) lapply(individual_bitsets(key), xor, key),
    invert_key = function(key) {
      df_mask <- packBits(c(rep(TRUE, ncol(df)), rep(FALSE, bitlen - ncol(df))))
      !key & df_mask
    },
    lookup_hash = function(hash, partitions) match(hash, partitions$key),
    get_with_index = function(index, partitions) partitions$value[[index]],
    calculate_partition = function(key) {
      attr_indices <- unkey(key)
      sp <- fsplit_rows_emptyable(df, attr_indices)
      unname(sp[lengths(sp) > 1])
    },
    add_partition = function(hash, val, partitions) {
      partitions$key <- c(partitions$key, hash)
      partitions$value <- c(partitions$value, list(val))
      partitions
    }
  )
  fetch_partition <- function(attrs_bitset, df, partitions) {
    fetch_partition_stripped(attrs_bitset, df, partitions, bitset_partitions_ui)
  }
  list(
    as.bitset = bitset_partitions_ui$key,
    key_size = bitset_partitions_ui$key_size,
    refine = function(rhs_bitset, lhs_bitset, partitions) {
      fetch_refined_partition(
        df,
        bitset_partitions_ui$unkey(rhs_bitset),
        lhs_bitset,
        partitions,
        fetch_partition
      )
    }
  )
}

fetch_partition_stripped <- function(
  key,
  df,
  partitions,
  partitions_ui
) {
  key_elements <- partitions_ui$decompose_key(key)
  element_hashes <- vapply(key_elements, partitions_ui$hash, character(1))
  hash <- partitions_ui$hash(key)
  partition_index <- partitions_ui$lookup_hash(hash, partitions)
  if (!is.na(partition_index)) {
    sp <- partitions_ui$get_with_index(partition_index, partitions)
    return(list(sp, partitions))
  }
  child_keys <- partitions_ui$key_children(key)
  child_hashes <- vapply(child_keys, partitions_ui$hash, character(1))
  child_indices <- vapply(
    child_hashes,
    partitions_ui$lookup_hash,
    integer(1L),
    partitions
  )
  if (sum(!is.na(child_indices)) >= 2) {
    chosen_indices <- which(!is.na(child_indices))[1:2]
    sp <- stripped_partition_product(
      partitions_ui$get_with_index(child_indices[[chosen_indices[[1]]]], partitions),
      partitions_ui$get_with_index(child_indices[[chosen_indices[[2]]]], partitions),
      nrow(df)
    )
  }else{
    if (sum(!is.na(child_indices)) == 1 && partitions_ui$key_size(key) > 1) {
      existing_child_position <- which(!is.na(child_indices))
      remainder_element <- key_elements[[existing_child_position]]
      remainder_hash <- element_hashes[[existing_child_position]]
      existing_child_index <- child_indices[[existing_child_position]]
      existing_child_sp <- partitions_ui$get_with_index(
        existing_child_index,
        partitions
      )
      remainder_result <- fetch_partition_stripped(
        remainder_element,
        df,
        partitions,
        partitions_ui
      )
      remainder_sp <- remainder_result[[1]]
      partitions <- remainder_result[[2]]
      sp <- stripped_partition_product(
        existing_child_sp,
        remainder_sp,
        nrow(df)
      )
    }else{
      sp <- partitions_ui$calculate_partition(key)
    }
  }
  partitions <- partitions_ui$add_partition(hash, sp, partitions)
  list(sp, partitions)
}

to_partition_node_char <- function(attrs_bitset) {
  paste(attrs_bitset, collapse = "")
}

individual_bitsets <- function(attrs_bitset) {
  bitlen <- 8L*length(attrs_bitset)
  bits <- which(rawToBits(attrs_bitset) == 1)
  lapply(bits, bitset, bitlen)
}

bitset <- function(attr_indices, bitlen) {
  bools <- rep(FALSE, bitlen)
  bools[attr_indices] <- TRUE
  packBits(bools)
}

fsplit_rows_emptyable <- function(df, attr_indices) {
  if (length(attr_indices) == 0)
    return(list(seq_len(nrow(df))))
  fsplit_rows(df, attr_indices)
}

fetch_refined_partition <- function(
  lookup,
  rhs_set,
  lhs_key,
  partition_cache,
  fetch_partition
) {
  res1 <- fetch_partition(lhs_key, lookup, partition_cache)
  lhs_partition <- res1[[1]]
  partition_cache <- res1[[2]]
  individual_rhs_lookup_indices <- lapply(rhs_set, \(r) lookup[[r]])
  rhs_lookup_indices <- if (length(rhs_set) == 1)
    individual_rhs_lookup_indices[[1]]
  else {
    do.call(paste, unname(lookup[rhs_set])) |>
      (\(x) match(x, x))()
  }
  relevant_lhs_partition <- filter_partition(lhs_partition, rhs_lookup_indices)
  if (partition_rank(relevant_lhs_partition) == 0)
    return(list(rep(list(list()), length(rhs_set)), list(), partition_cache))
  list(
    lapply(
      individual_rhs_lookup_indices,
      refine_partition_by_lookup,
      relevant_partition = relevant_lhs_partition
    ),
    relevant_lhs_partition,
    partition_cache
  )
}

validate <- function(new_partitions, Spli) {
  Spli_rank <- partition_rank(Spli)
  new_ranks <- vapply(new_partitions, partition_rank, integer(1))
  all(new_ranks == Spli_rank)
}

pli <- function(indices) {
  clusters <- split(seq_along(indices), ffactor1(indices))
  unname(clusters[lengths(clusters) > 1])
}

ffactor1 <- function(x) {
  # x is a character vector, as from combining lookups
  levels <- unique.default(x)
  f <- match(x, levels)
  levels(f) <- levels
  class(f) <- "factor"
  f
}

ffactor1i <- function(x) {
  # x is an integer vector
  levels <- unique.default(x)
  f <- match(x, levels)
  levels(f) <- as.character(levels)
  class(f) <- "factor"
  f
}

sample_diffsets <- function(pli, lookup, epsilon = 0.3) {
  sizes <- choose(lengths(pli), 2)
  cp <- sum(sizes)
  indices <- sample.int(cp, floor(cp^epsilon), replace = FALSE)
  boundaries <- c(0L, cumsum(sizes))
  intervals <- findInterval(indices, boundaries, left.open = TRUE)
  offsets <- indices - boundaries[intervals]
  sample_sizes <- sizes[intervals]
  stopifnot(all(1 <= offsets & offsets <= sample_sizes))
  samples <- Map(
    \(offset, rows) {
      # safe to use `:`, since length(rows) > 1 in stripped partitions
      first_boundaries <- c(0, cumsum(as.numeric((length(rows) - 1):1)))
      index <- findInterval(offset, first_boundaries, left.open = TRUE)
      rows[index + c(0L, offset - first_boundaries[[index]])]
    },
    offsets,
    pli[intervals]
  )
  lapply(
    samples,
    \(pair) which(lookup[pair[[1]], ] != lookup[pair[[2]], ])
  ) |>
    (\(x) x[lengths(x) > 0])() |>
    unique()
}

new_diffset <- function(Spli, refined_partitions, lookup) {
  # need a pair of rows that are together in S, but not in W
  # "... the FD is valid if the PLI is empty. If it is not empty, it is
  # sufficient to inspect any of the clusters that it contains to find a
  # violation." ~ Bleifuss et al. 2024
  # in other words, we take the partition for S, and remove any clusters
  # that don't differ on A.
  # For example, if we take the product of the partitions for S and A,
  # we could then take setdiff(partition(S), partition(S X A)).
  # The PLI mentioned above comes from the filtering used to update the
  # partition in refine_partition.
  new_clusters <- lapply(refined_partitions, \(rp) Spli[!is.element(Spli, rp)]) |>
    unlist(recursive = FALSE)
  stopifnot(length(new_clusters) > 0)
  rows <- new_clusters[[1]]
  unname(which(vapply(
    lookup[rows, , drop = FALSE],
    \(vals) any(vals != vals[[1]]),
    logical(1)
  )))
}

filter_partition <- function(partition, indices) {
  if (length(partition) == 0)
    return(list())
  single_index <- vapply(
    partition,
    \(cluster) {
      local_indices <- indices[cluster]
      all(local_indices == local_indices[[1]])
    },
    logical(1)
  )
  partition[!single_index]
}

refine_partition_by_lookup <- function(relevant_partition, indices) {
  if (length(relevant_partition) == 0)
    return(list())
  lapply(
    relevant_partition,
    \(cluster) {
      local_indices <- indices[cluster]
      split(cluster, ffactor1i(local_indices))
    }
  ) |>
    unlist(recursive = FALSE) |>
    (\(x) x[lengths(x) > 1])() |>
    unname()
}
