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
  refine_partition_wrapped <- partition_refiner(lookup)
  partition_cache <- list(key = character(), value = list())
  for (A in dependants) {
    report$stat(paste("dependant", attrs[A]))
    rest <- determinants[determinants != A]
    return_stack <- list(list(integer(), rest, A))
    visited <- list()
    while (length(return_stack) > 0) {
      node <- return_stack[[1]]
      return_stack <- return_stack[-1]
      attr_res <- FDHitsSep_visit(
        node[[1]],
        node[[2]],
        node[[3]],
        D,
        lookup,
        report = report,
        refine_partition_wrapped,
        partition_cache,
        visited = visited
      )
      visited <- c(visited, list(node[1:3]))
      res <- c(res, attr_res[[1]])
      D <- attr_res[[2]]
      new_nodes <- attr_res[[3]]
      partition_cache <- attr_res[[4]]
      return_stack <- c(
        new_nodes[vapply(
          new_nodes,
          \(node) length(node[[1]]) <= detset_limit,
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
  res <- lapply(res, lapply, \(x) attrs[x])
  functional_dependency(res, attrs)
}

FDHitsJoint <- function(lookup, determinants, dependants, detset_limit, D, report) {
  attrs <- names(lookup)
  res <- list()
  return_stack <- list(list(integer(), determinants, dependants))
  visited <- list()
  refine_partition_wrapped <- partition_refiner(lookup)
  partition_cache <- list(key = character(), value = list())
  while (length(return_stack) > 0) {
    node <- return_stack[[1]]
    return_stack <- return_stack[-1]
    attr_res <- FDHitsJoint_visit(
      node[[1]],
      node[[2]],
      node[[3]],
      D,
      lookup,
      report = report,
      refine_partition_wrapped,
      partition_cache,
      visited = visited
    )
    visited <- c(visited, list(node[c(1, 3)]))
    res <- c(res, attr_res[[1]])
    D <- attr_res[[2]]
    new_nodes <- attr_res[[3]]
    return_stack <- c(
      new_nodes[vapply(
        new_nodes,
        \(node) length(node[[1]]) <= detset_limit,
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
  res <- lapply(res, lapply, \(x) attrs[x])
  # split up into one-dependant FDs
  res <- lapply(res, \(x) lapply(x[[2]], \(dependant) list(x[[1]], dependant))) |>
    unlist(recursive = FALSE)
  functional_dependency(res, attrs)
}

FDHitsSep_visit <- function(
  S,
  V,
  A,
  D,
  lookup,
  report,
  refine_partition_wrapped,
  partition_cache,
  visited = list()
) {
  node_string <- paste0(
    "Node (S: {",
    toString(names(lookup)[S]),
    "}, V: {",
    toString(names(lookup)[V]),
    "}, A: {",
    toString(names(lookup)[[A]]),
    "})"
  )
  if (is.element(list(list(S, V, A)), visited)) {
    report$stat(paste0(
      "already visited ", node_string, "\n",
      "visited:\n",
      paste(
        vapply(
          visited,
          \(node) {
            paste0(
              "Node (S: {",
              toString(names(lookup)[node[[1]]]),
              "}, A: {",
              toString(names(lookup)[[node[[2]]]]),
              "})"
            )
          },
          character(1)
        ),
        collapse = "\n"
      ),
      "\ndifference sets:\n",
      paste(
        vapply(D, \(d) toString(names(lookup)[d]), character(1)),
        collapse = "\n"
      )
    ))
    stop("already visited ", node_string)
  }
  visited <- c(visited, list(list(S, V, A)))
  # pruning
  for (C in S) {
    # no critical edge for C
    # => C is redundant in S for A
    # => S isn't irreducible for A
    crits <- critical(C, A, S, D)
    if (length(crits) == 0)
      return(list(list(), D, list(), partition_cache))
    for (B in V) {
      # remove B from V if ∃ C∈S ∀ E∈critical(C,A,S): B∈E,
      # i.e. adding B to S would make some C in S redundant WRT A
      # does not check for B being redundant if added
      if (all(vapply(crits, is.element, logical(1), el = B)))
        V <- V[V != B]
    }
  }
  # validation at the leaves
  uncovered <- uncov_sep(S, A, D)
  if (length(uncovered) == 0) {
    refinement <- refine_partition_wrapped(A, S, partition_cache)
    refined_partitions <- refinement[[1]]
    relevant_Spli <- refinement[[2]]
    partition_cache <- refinement[[3]]
    if (validate(refined_partitions, relevant_Spli))
      return(list(list(list(S, A)), D, list(), partition_cache))
    stopifnot(length(relevant_Spli) > 0)
    ds <- new_diffset(relevant_Spli, refined_partitions, lookup)
    dsl <- list(ds)
    ds2 <- sample_diffsets(relevant_Spli, lookup)
    added <- setdiff(c(dsl, ds2), D)
    stopifnot(length(added) > 0)
    uncovered <- uncov_sep(S, A, added)
    D <- c(D, added)
  }
  # branching
  if (length(uncovered) == 0) {
    node_string <- paste0(
      "Node (S: {",
      toString(names(lookup)[S]),
      "}, V: {",
      toString(names(lookup)[V]),
      "})"
    )
    stop("edge selection impossible at ", node_string)
  }
  E <- sample_minheur_sep(uncovered, V)
  Bs <- intersect(E, V)
  res <- list()
  # rev() differs from the description in the paper, but the authors gave it as
  # a fix in private correspondence; I'll add a reference when they've published
  # the new work
  new_nodes <- lapply(
    rev(seq_along(Bs)),
    \(n) list(sort(union(S, Bs[[n]])), setdiff(V, Bs[seq_len(n)]), A)
  )
  list(res, D, new_nodes, partition_cache)
}

FDHitsJoint_visit <- function(
  S,
  V,
  W,
  D,
  lookup,
  report,
  refine_partition_wrapped,
  partition_cache,
  visited = list()
) {
  node_string <- paste0(
    "Node (S: {",
    toString(names(lookup)[S]),
    "}, V: {",
    toString(names(lookup)[V]),
    "}, W: {",
    toString(names(lookup)[W]),
    "})"
  )
  if (is.element(list(list(S,  W)), visited)) {
    report$stat(paste0(
      "already visited ", node_string, "\n",
      "visited:\n",
      paste(
        vapply(
          visited,
          \(node) {
            paste0(
              "Node (S: {",
              toString(names(lookup)[node[[1]]]),
              "}, W: {",
              toString(names(lookup)[node[[2]]]),
              "})"
            )
          },
          character(1)
        ),
        collapse = "\n"
      ),
      "\ndifference sets:\n",
      paste(
        vapply(D, \(d) toString(names(lookup)[d]), character(1)),
        collapse = "\n"
      )
    ))
    stop("already visited ", node_string)
  }
  visited <- c(visited, list(list(S, V, W)))
  # pruning
  critical_edges <- list()
  for (A in W) {
    for (C in S) {
      # no critical edge for C
      # => C is redundant in S for (some part of) W
      # => S isn't irreducible for (some part of) W
      crits <- critical(C, A, S, D)
      if (length(crits) == 0) {
        W <- W[W != A]
        critical_edges[[as.character(A)]] <- NULL
        break
      }else{
        critical_edges[[as.character(A)]][[as.character(C)]] <- crits
      }
    }
  }
  if (length(W) == 0) {
    return(list(list(), D, list()))
  }
  for (B in V) {
    # remove if ∀ A∈W ∃ C∈S ∀ E∈critical(C,A,S): B∈E,
    # i.e. adding B to S would make some C in S redundant WRT all of W
    # does not check for B being redundant if added
    if (all(vapply(
      W,
      \(A) any(vapply(
        S,
        \(C) all(vapply(
          critical_edges[[as.character(A)]][[as.character(C)]],
          \(E) B %in% E,
          logical(1)
        )),
        logical(1)
      )),
      logical(1)
    ))) {
      V <- setdiff(V, B)
    }
  }
  # validation at the leaves
  uncovered <- uncov_joint(S, W, D)
  if (length(uncovered) == 0) {
    refinement <- refine_partition_wrapped(W, S, partition_cache)
    refined_partitions <- refinement[[1]]
    relevant_Spli <- refinement[[2]]
    partition_cache <- refinement[[3]]
    if (validate(refined_partitions, relevant_Spli))
      return(list(list(list(S, W)), D, list()))
    stopifnot(length(relevant_Spli) > 0)
    ds <- new_diffset(relevant_Spli, refined_partitions, lookup)
    dsl <- list(ds)
    ds2 <- sample_diffsets(relevant_Spli, lookup)
    added <- setdiff(c(dsl, ds2), D)
    stopifnot(length(added) > 0)
    uncovered <- uncov_joint(S, W, added)
    D <- c(D, added)
  }
  # branching
  if (length(uncovered) == 0) {
    node_string <- paste0(
      "Node (S: {",
      toString(names(lookup)[S]),
      "}, V: {",
      toString(names(lookup)[V]),
      "}, W: {",
      toString(names(lookup)[W]),
      "})"
    )
    stop("edge selection impossible at ", node_string)
  }
  E <- sample_minheur_joint(uncovered, V, W)
  Bs <- intersect(E, V)
  res <- list()
  # rev() differs from the description in the paper, but the authors gave it as
  # a fix in private correspondence; I'll add a reference when they've published
  # the new work
  new_nodes <- c(
    if (any(!is.element(W, E))) list(list(S, V, setdiff(W, E))), # mu_0
    lapply( # mu_i
      rev(seq_along(Bs)),
      \(n) list(sort(union(S, Bs[[n]])), setdiff(V, Bs[seq_len(n)]), setdiff(intersect(W, E), Bs[[n]]))
    )
  )
  list(res, D, new_nodes)
}

critical <- function(C, A, S, D) {
  # Set that contains the critical edges for C WRT S in the subhypergraph H_A,
  # i.e. using only elements of D that include A.
  # If every C is S has a critical edge, then S is a minimal hitting set.
  # An edge is critical for C in S WRT S if it contains C, and no other vertex
  # from S, i.e. E /\ S = {C}.
  # E and S are already sets, so S[match(E, S, 0L)] is quicker than
  # intersect(E, S), since it skips removing duplicates.
  D[vapply(D, \(E) any(E == A) && identical(S[match(E, S, 0L)], C), logical(1))]
}

uncov_joint <- function(S, W, D) {
  # set of hyperedges that contain a vertex from W but nothing from S
  D[vapply(D, \(E) any(is.element(W, E)) && !any(is.element(S, E)), logical(1))]
}

uncov_sep <- function(S, A, D) {
  # set of hyperedges that contain a vertex from W but nothing from S
  D[vapply(D, \(E) any(E == A) && !any(is.element(S, E)), logical(1))]
}

sample_minheur_joint <- function(set, V, W) {
  if (length(set) == 0)
    stop("can't sample edge from empty set")
  heuristics <- vapply(
    set,
    function(E) length(intersect(E, V)) + length(setdiff(W, E)),
    integer(1)
  )
  set[[which.min(heuristics)]]
}

sample_minheur_sep <- function(set, V) {
  # For FDHitsSep, |W| = 1 and W /\ E is empty, so second part
  # of heuristic in sample_minheur_joint is redundant
  if (length(set) == 0)
    stop("can't sample edge from empty set")
  heuristics <- vapply(
    set,
    function(E) length(intersect(E, V)),
    integer(1)
  )
  set[[which.min(heuristics)]]
}

partition_refiner <- function(df) {
  partitions_ui <- list(
    # we could use the partkey directly as an index into a list of
    # pre-allocated length, but this often requires a very large list that is
    # slow to assign elements in, so we stick to matching on a growing list
    # here.
    # It would also require the partkey to be representable as an integer,
    # rather than a double, which introduces a tighter constraint on the maximum
    # number of columns df can have (nonfixed attrs instead of just LHS attrs).
    # "Partition nodes" in this UI refer to IDs within the partition: these are
    # different to those used in find_LHSs and powersets.
    add_partition = function(partition_node, val, partitions) {
      partitions$key <- c(partitions$key, partition_node)
      partitions$value <- c(partitions$value, list(val))
      partitions
    },
    get_with_index = function(index, partitions) {
      partitions$value[[index]]
    },
    lookup_node = function(partition_node, partitions) {
      match(partition_node, partitions$key)
    }
  )
  fetch_partition <- function(attr_indices, df, partitions) {
    fetch_partition_stripped(attr_indices, df, partitions, partitions_ui)
  }
  function(rhs, lhs_set, partitions) {
    refine_partition_generic(
      df,
      rhs,
      lhs_set,
      partitions,
      fetch_partition
    )
  }
}

fetch_partition_stripped <- function(
  attr_indices,
  df,
  partitions,
  partitions_ui
) {
  attr_nodes <- to_partition_nodes_char(attr_indices)
  partition_node <- to_partition_node_char(attr_indices)
  partkey <- partitions_ui$lookup_node(partition_node, partitions)
  if (!is.na(partkey)) {
    sp <- partitions_ui$get_with_index(partkey, partitions)
    return(list(sp, partitions))
  }
  subset_nodes <- vapply(
    seq_along(attr_indices),
    \(n) paste(attr_nodes[-n], collapse = ""),
    character(1)
  )
  subsets_match <- vapply(
    subset_nodes,
    partitions_ui$lookup_node,
    integer(1L),
    partitions
  )
  if (sum(!is.na(subsets_match)) >= 2) {
    indices <- which(!is.na(subsets_match))[1:2]
    sp <- stripped_partition_product(
      partitions_ui$get_with_index(subsets_match[indices[[1]]], partitions),
      partitions_ui$get_with_index(subsets_match[indices[[2]]], partitions),
      nrow(df)
    )
  }else{
    if (sum(!is.na(subsets_match)) == 1 && length(attr_indices) > 1) {
      index <- which(!is.na(subsets_match))
      small_subset <- attr_indices[[index]]
      small_subset_node <- attr_nodes[[index]]
      main_partition <- partitions_ui$get_with_index(
        subsets_match[index],
        partitions
      )
      subres <- fetch_partition_stripped(
        small_subset,
        df,
        partitions,
        partitions_ui
      )
      partitions <- subres[[2]]
      small_partition <- partitions_ui$get_with_index(
        partitions_ui$lookup_node(small_subset_node, partitions),
        partitions
      )
      sp <- stripped_partition_product(
        main_partition,
        small_partition,
        nrow(df)
      )
    }else{
      sp <- fsplit_rows_emptyable(df, attr_indices)
      sp <- unname(sp[lengths(sp) > 1])
    }
  }
  partitions <- partitions_ui$add_partition(partition_node, sp, partitions)
  list(sp, partitions)
}

to_partition_nodes_char <- function(attr_indices) {
  chars <- vapply(
    attr_indices,
    \(n) rawToChar(packBits(intToBits(n), "raw"), multiple = FALSE),
    character(1)
  )
  stopifnot(all(nchar(chars) == 1))
  chars
}

to_partition_node_char <- function(attr_indices) {
  chars <- vapply(
    attr_indices,
    \(n) rawToChar(packBits(intToBits(n), "raw"), multiple = FALSE),
    character(1)
  )
  stopifnot(all(nchar(chars) == 1))
  paste(chars, collapse = "")
}

fsplit_rows_emptyable <- function(df, attr_indices) {
  if (length(attr_indices) == 0)
    return(list(seq_len(nrow(df))))
  fsplit_rows(df, attr_indices)
}

refine_partition_generic <- function(
  lookup,
  rhs,
  lhs_set,
  partition_cache,
  fetch_partition
) {
  res1 <- fetch_partition(lhs_set, lookup, partition_cache)
  lhs_partition <- res1[[1]]
  partition_cache <- res1[[2]]
  individual_rhs_indices <- lapply(rhs, \(r) lookup[[r]])
  rhs_indices <- if (length(rhs) == 1)
    individual_rhs_indices[[1]]
  else {
    do.call(paste, unname(lookup[rhs])) |>
      (\(x) match(x, x))()
  }
  relevant_lhs_partition <- filter_partition(lhs_partition, rhs_indices)
  if (partition_rank(relevant_lhs_partition) == 0)
    return(list(rep(list(list()), length(rhs)), list(), partition_cache))
  list(
    lapply(individual_rhs_indices, \(r) refine_partition(relevant_lhs_partition, r)),
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

refine_partition_old <- function(partition, attr, lookup) {
  if (length(partition) == 0)
    return(list())
  lapply(
    partition,
    \(cluster) split(cluster, ffactor1i(lookup[[attr]][cluster]))
  ) |>
    unlist(recursive = FALSE) |>
    (\(x) x[lengths(x) > 1])() |>
    unname()
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

refine_partition <- function(relevant_partition, indices) {
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
