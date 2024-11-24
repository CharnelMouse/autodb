# non-empty in the sense of being the set of non-empty subsets, so {} has no
# node
nonempty_powerset <- function(cardinality, use_visited, max_size = cardinality) {
  max_size <- min(max_size, cardinality)
  if (max_size == 0)
    return(c(
      list(
        bits = list(),
        bitset_index = rep(NA_integer_, 2^cardinality - 1),
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
        bitset_index = 1L,
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
  bitset_index <- rep(NA_integer_, n_nonempty_subsets)
  bitset_index[within_limit] <- seq_len(n_limited_nonempty_subsets)
  children <- rep(list(integer()), n_limited_nonempty_subsets)
  parents <- rep(list(integer()), n_limited_nonempty_subsets)
  for (x in seq_len(n_limited_nonempty_subsets - 1)) {
    zeroes <- which(!limited_node_bits[[x]])
    ys <- as.integer(limited_to_unlimited_map[[x]] + 2^(zeroes - 1))
    limited_ys <- bitset_index[ys]
    limited_ys <- limited_ys[!is.na(limited_ys)]
    parents[[x]] <- c(parents[[x]], limited_ys)
    children[limited_ys] <- lapply(children[limited_ys], c, x)
  }
  c(
    list(
      bits = limited_node_bits,
      bitset_index = bitset_index,
      children = children,
      parents = parents,
      category = rep(0L, n_limited_nonempty_subsets)
    ),
    if (use_visited)
      list(visited = rep(FALSE, n_limited_nonempty_subsets))
  )
}

reduce_powerset <- function(powerset, cardinality) {
  bi_len <- length(powerset$bitset_index)
  old_cardinality <- if (bi_len)
    as.integer(log(length(powerset$bitset_index), 2) + 1)
  else
    0L
  if (cardinality == old_cardinality) {
    return(powerset)
  }
  if (old_cardinality == 0) {
    powerset$bitset_index <- integer()
    return(powerset)
  }
  if (cardinality > old_cardinality)
    stop(
      "new cardinality (",
      cardinality,
      ") is larger than current cardinality (",
      old_cardinality,
      ")"
    )
  trimmed <- powerset
  trimmed$bitset_index <- trimmed$bitset_index[seq_len(2^cardinality - 1)]
  keep <- trimmed$bitset_index
  keep <- keep[!is.na(keep)]
  trim <- setdiff(names(trimmed), "bitset_index")
  trimmed[trim] <- lapply(trimmed[trim], `[`, keep)
  trimmed$bits <- lapply(trimmed$bits, utils::head, cardinality)
  # updating parents is slow, and the main reason why caching powerset
  # reductions in discover() saves a lot of time
  trimmed$parents <- lapply(trimmed$parents, \(x) which(keep %in% x))
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

nonvisited_children <- function(node, powerset) {
  visited <- which(powerset$visited)
  children <- powerset$children[[node]]
  setdiff(children, visited)
}

nonvisited_parents <- function(node, powerset) {
  visited <- which(powerset$visited)
  parents <- powerset$parents[[node]]
  setdiff(parents, visited)
}

is_subset <- function(bits1, bits2) all(bits2[bits1])
is_superset <- function(bits1, bits2) all(bits1[bits2])

to_node <- function(element_indices, powerset) {
  res <- powerset$bitset_index[[sum(2^(element_indices - 1L))]]
  stopifnot(!is.na(res))
  res
}

# Takes indices of elements (i.e. leaf nodes),
# converts into node indices for if powerset was constructed
# with size == cardinality, and returns their indices given
# the actual size, removing any missing ones
to_nodes <- function(element_indices, powerset) {
  res <- powerset$bitset_index[2^(element_indices - 1L)]
  res[!is.na(res)]
}
