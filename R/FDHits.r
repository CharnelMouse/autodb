FDHitsSep <- function(x, progress = FALSE, progress_file = "") {
  report <- reporter(progress, progress_file, new = TRUE)
  if (ncol(x) == 0)
    return(functional_dependency(list(), character()))
  report$stat("simplifying data types")
  lookup <- lookup_table(x)
  attrs <- names(lookup)
  attr_indices <- seq_along(x)
  report$stat("calculating single-attribute PLIs")
  plis <- lapply(lookup, pli)
  report$stat("sampling difference sets")
  D <- lapply(plis, sample_diffsets, lookup) |>
    unlist(recursive = FALSE) |>
    unique()
  report$stat(with_number(length(D), "initial diffset", "\n", "s\n"))
  res <- list()
  n_visited <- 0L
  for (A in attr_indices) {
    report$stat(paste("dependant", attrs[A]))
    rest <- setdiff(attr_indices, A)
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
        visited = visited
      )
      visited <- c(visited, list(node[1:3]))
      res <- c(res, attr_res[[1]])
      D <- attr_res[[2]]
      return_stack <- c(attr_res[[3]], return_stack)
    }
    report$stat("")
    n_visited <- n_visited + length(visited)
  }
  report$stat(paste0(
    with_number(length(D), "final diffset", "\n", "s\n"),
    with_number(n_visited, "node", " visited", "s visited")
  ))
  res <- lapply(res, lapply, \(x) attrs[x])
  functional_dependency(res, attrs)
}

FDHitsJoint <- function(x, progress = FALSE, progress_file = "") {
  report <- reporter(progress, progress_file, new = TRUE)
  if (ncol(x) == 0)
    return(functional_dependency(list(), character()))
  report$stat("simplifying data types")
  lookup <- lookup_table(x)
  attrs <- names(lookup)
  attr_indices <- seq_along(x)
  report$stat("calculating single-attribute PLIs")
  plis <- lapply(lookup, pli)
  report$stat("sampling difference sets")
  D <- lapply(plis, sample_diffsets, lookup) |>
    unlist(recursive = FALSE) |>
    unique()
  report$stat(with_number(length(D), "initial diffset", "\n", "s\n"))
  res <- list()
  return_stack <- list(list(integer(), attr_indices, attr_indices))
  visited <- list()
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
      visited = visited
    )
    visited <- c(visited, list(node[c(1, 3)]))
    res <- c(res, attr_res[[1]])
    D <- attr_res[[2]]
    return_stack <- c(attr_res[[3]], return_stack)
  }
  report$stat(paste0(
    "\n",
    with_number(length(D), "final diffset", "\n", "s\n"),
    with_number(length(visited), "node", " visited", "s visited")
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
  report$stat(node_string)
  visited <- c(visited, list(list(S, V, A)))
  # pruning
  for (C in S) {
    # no critical edge for C
    # => C is redundant in S for A
    # => S isn't irreducible for A
    crits <- critical(C, A, S, D)
    if (length(crits) == 0)
      return(list(list(), D, list()))
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
    Spli <- if (length(S) == 0) {
      if (nrow(lookup) <= 1)
        list()
      else
        list(seq_len(nrow(lookup)))
    }else
      pli(do.call(paste, unname(lookup[S])))
    refined_partitions <- list(
      refine_partition(Spli, A, lookup) |>
        # sort to avoid using is.element or setequal
        (\(x) x[order(vapply(x, `[`, integer(1),1))])()
    )
    if (validate(refined_partitions, Spli)) {
      report$stat(paste0(
        "  found {",
        toString(names(lookup)[S]),
        "} -> {",
        toString(names(lookup)[A]),
        "}"
      ))
      return(list(list(list(S, A)), D, list()))
    }
    report$stat(paste0(
      "  found false {",
      toString(names(lookup)[S]),
      "} -> {",
      toString(names(lookup)[A]),
      "}"
    ))
    stopifnot(length(Spli) > 0)
    ds <- new_diffset(Spli, refined_partitions, lookup)
    dsl <- list(ds)
    ds2 <- sample_diffsets(Spli, lookup)
    added <- setdiff(c(dsl, ds2), D)
    stopifnot(length(added) > 0)
    report$stat(paste0(
      "  added ",
      with_number(length(added), "diffset", "", "s")
    ))
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
  list(res, D, new_nodes)
}

FDHitsJoint_visit <- function(
  S,
  V,
  W,
  D,
  lookup,
  report,
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
  report$stat(node_string)
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
    Spli <- if (length(S) == 0) {
      if (nrow(lookup) <= 1)
        list()
      else
        list(seq_len(nrow(lookup)))
    }else
      pli(do.call(paste, unname(lookup[S])))
    refined_partitions <- lapply(
      W,
      \(A) refine_partition(Spli, A, lookup) |>
        # sort to avoid using is.element or setequal
        (\(x) x[order(vapply(x, `[`, integer(1),1))])()
    )
    if (validate(refined_partitions, Spli)) {
      report$stat(paste0(
        "  found {",
        toString(names(lookup)[S]),
        "} -> {",
        toString(names(lookup)[W]),
        "}"
      ))
      return(list(list(list(S, W)), D, list()))
    }
    report$stat(paste0(
      "  found false {",
      toString(names(lookup)[S]),
      "} -> {",
      toString(names(lookup)[W]),
      "}"
    ))
    stopifnot(length(Spli) > 0)
    ds <- new_diffset(Spli, refined_partitions, lookup)
    dsl <- list(ds)
    ds2 <- sample_diffsets(Spli, lookup)
    added <- setdiff(c(dsl, ds2), D)
    stopifnot(length(added) > 0)
    report$stat(paste0(
      "  added ",
      with_number(length(added), "diffset", "", "s")
    ))
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
  report$stat(paste0(
    "  ",
    with_number(length(new_nodes), "new return stack node", "", "s"),
    if (length(new_nodes) > 0)
      "\n",
    paste(
      "  ",
      vapply(
        new_nodes,
        \(node) {
          paste0(
            "Node (S: {",
            toString(names(lookup)[node[[1]]]),
            "}, V: {",
            toString(names(lookup)[node[[2]]]),
            "}, W: {",
            toString(names(lookup)[node[[3]]]),
            "})"
          )
        },
        character(1)
      ),
      sep = "",
      collapse = "\n",
      recycle0 = TRUE
    )
  ))
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

validate <- function(new_partitions, Spli) {
  all(vapply(new_partitions, identical, logical(1), Spli))
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

refine_partition <- function(partition, attr, lookup) {
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
