treeSearchSep <- function(x, progress = FALSE) {
  lookup <- lookup_table(x)
  attrs <- names(lookup)
  attr_indices <- seq_along(x)
  plis <- lapply(lookup, pli)
  D <- lapply(plis, sample_diffsets, lookup) |>
    Reduce(f = c, init = list()) |>
    unique()
  if (progress) {
    cat(with_number(length(D), "initial diffset", "\n\n", "s\n\n"))
    flush.console()
  }
  res <- list()
  for (a in attr_indices) {
    if (progress) {
      cat("dependant", match(a, attrs), "\n\n")
      flush.console()
    }
    rest <- setdiff(attr_indices, a)
    return_stack <- list(list(integer(), rest, a))
    visited <- list()
    while (length(return_stack) > 0) {
      node <- return_stack[[1]]
      return_stack <- return_stack[-1]
      attr_res <- treeSearchSep_visit(
        node[[1]],
        node[[2]],
        node[[3]],
        D,
        lookup,
        visited = visited,
        progress = progress
      )
      res <- c(res, attr_res[[1]])
      D <- attr_res[[2]]
      return_stack <- c(attr_res[[3]], return_stack)
    }
    if (progress) {
      cat("\n")
      flush.console()
    }
  }
  if (progress) {
    cat(with_number(length(D), "final diffset", "\n", "s\n"))
    flush.console()
  }
  res <- lapply(res, lapply, \(x) attrs[x])
  functional_dependency(res, attrs)
}

treeSearchSep_visit <- function(
  S,
  V,
  attr,
  D,
  lookup,
  visited = list(),
  progress = FALSE
) {
  if (is.element(list(list(S, V, attr)), visited))
    stop("already visited {", toString(S), "}, {", toString(V), "}")
  visited <- c(visited, list(list(S, V, attr)))
  # pruning
  for (C in S) {
    # no critical edge for C
    # => C is redundant in S for W
    # => S isn't irreducible for W
    if (length(critical(C, attr, S, D)) == 0) {
      return(list(list(), D, list()))
    }
  }
  for (B in V) {
    # ∀ A∈W ∃ C∈S ∀ E∈critical(C,A,S): B∈E
    # i.e. adding B to S would make some C in S redundant WRT W, as per above
    # does not check for B being redundant if added
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
      V <- setdiff(V, B)
    }
  }
  if (length(attr) == 0) {
    return(list(list(), D, list()))
  }
  # validation at the leaves
  uncovered <- uncov(S, attr, D)
  if (length(uncovered) == 0) {
    Spli <- if (length(S) == 0) {
      if (nrow(lookup) <= 1)
        list()
      else
        list(seq_len(nrow(lookup)))
    }else
      pli(do.call(paste, lookup[S]))
    if (validate(Spli, attr, lookup)) {
      if (progress) {
        cat("found {", toString(names(lookup)[S]), "} -> {", toString(names(lookup)[attr]), "}\n", sep = "")
        flush.console()
      }
      return(list(list(list(S, attr)), D, list()))
    }else{
      if (progress) {
        cat("found false {", toString(names(lookup)[S]), "} -> {", toString(names(lookup)[attr]), "}\n", sep = "")
        flush.console()
      }
      ds <- new_diffset(S, attr, lookup)
      dsl <- list(ds)
      new_D <- c(D, dsl)
      ds2 <- sample_diffsets(Spli, lookup)
      new_D <- union(new_D, ds2)
      if (progress) {
        cat(paste0(
          "added ",
          with_number(length(new_D) - length(D), "diffset", "", "s"),
          "\n"
        ))
        flush.console()
      }
      D <- new_D
      uncovered <- uncov(S, attr, D)
    }
  }
  # branching
  if (length(uncovered) == 0)
    stop(
      "edge selection impossible at ",
      toString(names(lookup)[S]),
      "; ",
      toString(names(lookup)[V]),
      "; ",
      toString(names(lookup)[attr])
    )
  E <- sample_minheur(uncovered, V, attr)
  Bs <- intersect(E, V)
  res <- list()
  # rev() differs from the description in the paper, but the authors gave it as
  # a fix in private correspondence; I'll add a reference when they've published
  # the new work
  new_nodes <- lapply(
    rev(seq_along(Bs)),
    \(n) list(sort(union(S, Bs[[n]])), setdiff(V, Bs[seq_len(n)]), attr)
  )
  list(res, D, new_nodes)
}

critical <- function(C, A, S, D) {
  # set that contains the critical edges for C WRT S in the subhypergraph H_A,
  # i.e. using only elements of D that include A.
  # an edge is critical for C in S WRT S if it contains C, and no other vertex
  # from S.
  # if every C is S has such a critical edge, then S is a minimal hitting set.
  D |>
    Filter(f = \(E) any(E == A)) |>
    Filter(f = \(E) identical(intersect(E, S), C))
}

uncov <- function(S, W, D) {
  # set of hyperedges that contain a vertex from W but nothing from S
  D |>
    Filter(f = \(E) any(is.element(W, E)) && !any(is.element(S, E)))
}

sample_minheur <- function(set, V, W) {
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

validate <- function(Spli, W, lookup) {
  all(vapply(
    W,
    \(attr) identical(
      Spli,
      refine_partition(Spli, attr, lookup) |>
        # sort to avoid using is.element or setequal
        (\(x) x[order(sapply(x, `[`, 1))])()
    ),
    logical(1)
  ))
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
    \(pair) which(lookup[pair[[1]], ] != lookup[pair[[2]], ])
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
    unname(fsplit(
      seq_len(nrow(lookup)),
      lookup[, S, drop = FALSE]
    )) |>
      (\(x) x[lengths(x) > 1])()
  }
  if (length(partition) == 0)
    stop("{", toString(names(lookup)[S]), "} -> {", toString(names(lookup)[W]), "} is satisfied")
  joint_partitions <- lapply(
    W,
    \(attr) setdiff(partition, refine_partition(partition, attr, lookup))
  ) |>
    Reduce(f = c, init = list())
  stopifnot(length(joint_partitions) > 0)
  rows <- joint_partitions[[1]]
  which(vapply(
    lookup[rows, , drop = FALSE],
    \(vals) any(vals != vals[[1]]),
    logical(1)
  ))
}

refine_partition <- function(partition, attr, lookup) {
  lapply(
    partition,
    \(cluster) {
      unname(split(cluster, lookup[[attr]][cluster])) |>
        (\(x) x[lengths(x) > 1])()
    }
  ) |>
    Reduce(f = c, init = list())
}
