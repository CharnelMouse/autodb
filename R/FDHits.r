treeSearchJoint <- function(attrs, D, expected, progress = FALSE) {
  treeSearchJoint_rec(
    character(),
    attrs,
    attrs,
    D,
    orig = attrs,
    expected = expected,
    progress = progress
  )
}

treeSearchJoint_rec <- function(
  S,
  V,
  W,
  D,
  orig,
  expected,
  visited = list(),
  progress = FALSE
) {
  if (progress) {
    cat("S =", toString(S), "\n")
    cat("V =", toString(V), "\n")
    cat("W =", toString(W), "\n")
  }
  if (is.element(list(list(S, V, W)), visited))
    stop("already visited")
  visited <- c(visited, list(list(S, V, W)))
  # pruning
  for (x in Map(list, rep(S, length(W)), rep(W, each = length(S)))) {
    C <- x[[1]]
    A <- x[[2]]
    if (length(critical(C, A, S, D)) == 0) {
      if (progress)
        cat("pruning", A, "from W: no critical edges for", C, "WRT S\n")
      W <- setdiff(W, A)
    }
  }
  for (B in V) {
    # ∀𝐴∈𝑊∃𝐶∈𝑆∀𝐸∈critical(𝐶,𝐴,𝑆):𝐵∈𝐸
    if (all(vapply(
      W,
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
      if (progress)
        cat("pruning", B, "from V\n")
      V <- setdiff(V, B)
    }
  }
  if (length(W) == 0) {
    if (progress)
      cat("nothing found\n\n")
    return(list())
  }
  # validation at the leaves
  uncovered <- uncov(S, W, D)
  if (length(uncovered) == 0 && validate(list(S, W), expected, progress)) {
    if (progress)
      cat("found {", toString(S), "} -> {", toString(W), "}\n\n")
    return(list(list(S, W)))
  }
  # Branching
  if (progress) {
    cat("S =", toString(S), "\n")
    cat("V =", toString(V), "\n")
    cat("W =", toString(W), "\n")
    cat("uncovered:", sapply(uncovered, \(x) paste0("\n", toString(x))), "\n")
  }
  if (length(uncovered) == 0)
    stop(
      "edge selection impossible at ",
      toString(S),
      "; ",
      toString(V),
      "; ",
      toString(W)
    )
  E <- sample_minheur(uncovered, E, V, W)
  if (progress)
    cat("E = ", toString(E), "\n\n")
  Bs <- intersect(E, V)
  c(
    list(treeSearchJoint_rec(S, V, setdiff(W, E), D, orig, expected, visited, progress = progress)),
    lapply(
      seq_along(Bs),
      \(n) treeSearchJoint_rec(
        union(S, Bs[[n]]) |> (\(x) x[order(match(x, orig))])(),
        setdiff(V, Bs[seq_len(n)]),
        setdiff(intersect(W, E), Bs[[n]]),
        D,
        orig,
        expected,
        visited,
        progress = progress
      )
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

validate <- function(fd, expected, progress = FALSE) {
  if (progress)
    cat("validating", toString(fd[[1]]), "->", fd[[2]], "\n")
  detset_match <- match(list(fd[[1]]), lapply(expected, `[[`, 1))
  res <- if (is.na(detset_match))
    FALSE
  else{
    all(is.element(fd[[2]], expected[[detset_match]][[2]]))
  }
  if (progress)
    cat(res, "\n")
  res
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
    unique()
}
