treeSearchSep <- function(x, D, progress = FALSE) {
  lookup <- lookup_table(x)
  attrs <- names(x)
  plis <- lapply(lookup, pli)
  lapply(
    attrs,
    \(a) {
      if (progress) {
        cat("dependant", a, "\n")
        flush.console
      }
      treeSearchSep_rec(
        character(),
        setdiff(attrs, a),
        a,
        D,
        orig = setdiff(attrs, a),
        lookup,
        plis,
        progress = progress
      )
    }
  ) |>
    Reduce(f = c, init = list()) |>
    functional_dependency(names(x))
}

treeSearchSep_rec <- function(
  S,
  V,
  attr,
  D,
  orig,
  lookup,
  plis,
  visited = list(),
  progress = FALSE
) {
  if (progress) {
    cat("S =", toString(S), "\n")
    cat("V =", toString(V), "\n")
    flush.console()
  }
  if (is.element(list(list(S, V, attr)), visited))
    stop("already visited")
  visited <- c(visited, list(list(S, V, attr)))
  # pruning
  for (C in S) {
    if (length(critical(C, attr, S, D)) == 0) {
      if (progress) {
        cat("no critical edges for", C, "WRT S\n")
        flush.console()
      }
      return(list())
    }
  }
  for (B in V) {
    # ∀𝐴∈𝑊∃𝐶∈𝑆∀𝐸∈critical(𝐶,𝐴,𝑆):𝐵∈𝐸
    if (all(vapply(
      attr,
      \(A) any(vapply(
        S,
        \(C) all(vapply(
          critical(C, A, S, D),
          \(E) B %in% E,
          logical(1)
        )),
        logical(1)
      )),
      logical(1)
    ))) {
      if (progress) {
        cat("pruning", B, "from V\n")
        flush.console()
      }
      V <- setdiff(V, B)
    }
  }
  if (length(attr) == 0) {
    if (progress) {
      cat("nothing found\n\n")
      flush.console()
    }
    return(list())
  }
  # validation at the leaves
  uncovered <- uncov(S, attr, D)
  if (length(uncovered) == 0) {
    if (validate(list(S, attr), lookup, plis, progress)) {
      if (progress) {
        cat("found {", toString(S), "} -> {", toString(attr), "}\n\n")
        flush.console()
      }
      return(list(list(S, attr)))
    }else{
      stop(paste("found false {", toString(S), "} -> {", toString(attr), "}"))
      # if (progress)
      #   cat("found false {", toString(S), "} -> {", toString(W), "}\n")
      # ds <- new_diffset(S, W, lookup)
      # D <- D + list(ds)
      # if (progress)
      #   cat("added diffset", toString(ds), "\n\n")
    }
  }
  # Branching
  if (progress) {
    cat("S =", toString(S), "\n")
    cat("V =", toString(V), "\n")
    cat("uncovered:", sapply(uncovered, \(x) paste0("\n", toString(x))), "\n")
    flush.console()
  }
  if (length(uncovered) == 0)
    stop(
      "edge selection impossible at ",
      toString(S),
      "; ",
      toString(V),
      "; ",
      toString(attr)
    )
  E <- sample_minheur(uncovered, E, V, attr)
  if (progress) {
    cat("E = ", toString(E), "\n\n")
    flush.console()
  }
  Bs <- intersect(E, V)
  lapply(
    seq_along(Bs),
    \(n) treeSearchSep_rec(
      S = union(S, Bs[[n]]) |> (\(x) x[order(match(x, orig))])(),
      V = setdiff(V, Bs[seq_len(n)]),
      attr = attr,
      D = D,
      orig = orig,
      lookup = lookup,
      plis = plis,
      visited = visited,
      progress = progress
    )
  ) |>
    Reduce(f = c)
}

critical <- function(C, A, S, D) {
  # set that contains the critical edges for C WRT S in the subhypergraph H_A,
  # i.e. using only elements of D that include A.
  # an edge is critical for C in S WRT S if it contains C, and no other vertex
  # from S.
  # if every C is S has such a critical edge, then S is a minimal hitting set.
  D |>
    Filter(f = \(E) is.element(A, E)) |>
    Filter(f = \(E) identical(intersect(E, S), C))
}

uncov <- function(S, W, D) {
  # set of hyperedges that contain a vertex from W but nothing from S
  D |>
    Filter(f = \(E) any(is.element(W, E)) && !any(is.element(S, E)))
}

sample_minheur <- function(set, E, V, W) {
  if (length(set) == 0)
    stop("can't sample edge from empty set")
  heuristics <- vapply(
    set,
    function(E) length(intersect(E, V)) + length(setdiff(W, E)),
    integer(1)
  )
  set[[which.min(heuristics)]]
  # sample(set[which(heuristics == min(heuristics))], 1)[[1]]
}

validate <- function(fd, lookup, plis, progress = FALSE) {
  if (progress)
    cat("validating", toString(fd[[1]]), "->", fd[[2]], "\n")
  detset_pli <- Reduce(\(x, y) stripped_partition_product(x, y, nrow(lookup)), plis[fd[[1]]])
  combined_plis <- lapply(
    fd[[2]],
    \(attr) setdiff(detset_pli, refine_partition(detset_pli, attr, lookup))
  )
  res <- all(lengths(combined_plis) == 0)
  if (progress)
    cat(res, "\n")
  res
}

pli <- function(indices) {
  clusters <- split(seq_along(indices), indices)
  unname(clusters[lengths(clusters) > 1])
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
      first_boundaries <- c(0L, cumsum((length(rows) - 1):1))
      index <- findInterval(offset, first_boundaries, left.open = TRUE)
      rows[index + c(0L, offset - first_boundaries[[index]])]
    },
    offsets,
    pli[intervals]
  )
  lapply(
    samples,
    \(pair) names(lookup)[lookup[pair[[1]], ] != lookup[pair[[2]], ]]
  ) |>
    (\(x) x[lengths(x) > 0])() |>
    unique()
}

new_diffset <- function(S, W, lookup) {
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
  partition <- if (length(S) == 0) {
    list(seq_len(nrow(lookup))) |>
      (\(x) x[lengths(x) > 1])()
  }else{
    unname(split(
      seq_len(nrow(lookup)),
      lookup[, S, drop = FALSE]
    )) |>
      (\(x) x[lengths(x) > 1])()
  }
  if (length(partition) == 0)
    stop("{", toString(S), "} -> {", toString(W), "} is satisfied")
  joint_partitions <- lapply(
    W,
    \(attr) setdiff(partition, refine_partition(partition, attr, lookup))
  ) |>
    Reduce(f = c, init = list())
  stopifnot(length(joint_partitions) > 0)
  rows <- joint_partitions[[1]]
  names(lookup)[vapply(
    lookup[rows, , drop = FALSE],
    \(vals) any(vals != vals[[1]]),
    logical(1)
  )]
}

refine_partition <- function(partition, attr, lookup) {
  lapply(
    partition,
    \(cluster) unname(split(cluster, lookup[[attr]][cluster])) |>
      (\(x) x[lengths(x) > 1])()
  ) |>
    Reduce(f = c, init = list())
}
