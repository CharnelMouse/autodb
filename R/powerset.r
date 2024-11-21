powerset_nodes <- function(cardinality, use_visited, max_size = cardinality) {
  max_size <- min(max_size, cardinality)
  if (max_size == 0)
    return(c(
      list(
        bits = list(),
        children = list(),
        parents = list(),
        category = integer()
      ),
      if (use_visited)
        list(visited = logical())
    )
    )
  if (cardinality == 1)
    return(c(
      list(
        bits = list(TRUE),
        children = list(integer()),
        parents = list(integer()),
        category = 0L
      ),
      if (use_visited)
        list(visited = FALSE)
    ))
  n_nonempty_subsets <- 2^cardinality - 1
  n_limited_nonempty_subsets <- sum(choose(cardinality, seq_len(max_size)))
  node_bits <- lapply(seq_len(n_nonempty_subsets), \(i) as.logical(intToBits(i))[1:cardinality])
  within_limit <- vapply(node_bits, sum, integer(1)) <= max_size
  limited_node_bits <- node_bits[within_limit]
  limited_to_unlimited_map <- which(within_limit)
  children <- rep(list(integer()), n_limited_nonempty_subsets)
  parents <- rep(list(integer()), n_limited_nonempty_subsets)
  for (x in seq_len(n_limited_nonempty_subsets - 1)) {
    zeroes <- which(!limited_node_bits[[x]][1:cardinality])
    ys <- as.integer(limited_to_unlimited_map[[x]] + 2^(zeroes - 1))
    limited_ys <- na.omit(match(ys, limited_to_unlimited_map))
    parents[[x]] <- c(parents[[x]], limited_ys)
    children[limited_ys] <- lapply(children[limited_ys], c, x)
  }
  c(
    list(
      bits = limited_node_bits,
      children = children,
      parents = parents,
      category = rep(0L, n_limited_nonempty_subsets)
    ),
    if (use_visited)
      list(visited = rep(FALSE, n_limited_nonempty_subsets))
  )
}

reduce_powerset <- function(powerset, cardinality) {
  n_nodes <- length(powerset$category)
  old_cardinality <- if (n_nodes) length(powerset$bits[[1]]) else 0L
  if (cardinality == old_cardinality || old_cardinality == 0)
    return(powerset)
  if (cardinality > old_cardinality)
    stop(
      "new cardinality (",
      cardinality,
      ") is larger than current cardinality (",
      old_cardinality,
      ")"
    )
  keep <- which(vapply(
    powerset$bits,
    \(x) !any(x[setdiff(seq_len(old_cardinality), seq_len(cardinality))]),
    logical(1)
  ))
  trimmed <- lapply(powerset, `[`, keep)
  trimmed$parents <- lapply(trimmed$parents, \(x) match(x[x %in% keep], keep))
  trimmed$bits <- lapply(trimmed$bits, head, cardinality)
  trimmed
}

is_minimal <- function(node, powerset) {
  children <- powerset$children[[node]]
  if (any(powerset$category[children] > 0))
    return(FALSE)
  if (all(powerset$category[children] < 0))
    return(TRUE)
  NA
}

is_maximal <- function(node, powerset) {
  parents <- powerset$parents[[node]]
  if (any(powerset$category[parents] < 0))
    return(FALSE)
  if (all(powerset$category[parents] > 0))
    return(TRUE)
  NA
}

update_dependency_type <- function(node, powerset, min_deps, max_non_deps) {
  children <- powerset$children[[node]]
  if (any(children %in% min_deps))
    return(1L)
  parents <- powerset$parents[[node]]
  if (any(parents %in% max_non_deps))
    return(-1L)
  powerset$category[node]
}

infer_type <- function(node, powerset) {
  # Attempts to infer the category of self by checking if any subsets are a
  # dependency, or if any supersets are a non-dependency.
  category <- NA
  if (has_dependency_subset(node, powerset))
    category <- 1L
  if (has_nondependency_superset(node, powerset))
    category <- -1L
  category
}

has_dependency_subset <- function(node, powerset) {
  node_bits <- powerset$bits[[node]]
  dependencies <- powerset$category > 0
  dependencies[node] <- FALSE
  dependency_bitsets <- powerset$bits[dependencies]
  any(vapply(
    dependency_bitsets,
    is_subset,
    logical(1),
    node_bits
  ))
}

has_nondependency_superset <- function(node, powerset) {
  node_bits <- powerset$bits[[node]]
  nondependencies <- powerset$category < 0
  nondependencies[node] <- FALSE
  nondependency_bitsets <- powerset$bits[nondependencies]
  any(vapply(
    nondependency_bitsets,
    is_superset,
    logical(1),
    node_bits
  ))
}

unchecked_subsets <- function(node, powerset) {
  visited <- which(powerset$visited)
  children <- powerset$children[[node]]
  setdiff(children, visited)
}

unchecked_supersets <- function(node, powerset) {
  visited <- which(powerset$visited)
  parents <- powerset$parents[[node]]
  setdiff(parents, visited)
}

is_subset <- function(bits1, bits2) all(bits2[bits1])
is_superset <- function(bits1, bits2) all(bits1[bits2])

to_node <- function(element_indices) {
  as.integer(sum(2^(element_indices - 1L)))
}

to_nodes <- function(element_indices) {
  as.integer(2^(element_indices - 1L))
}
