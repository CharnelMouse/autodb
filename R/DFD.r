DFD <- function(
  lookup,
  valid_dependant_attrs,
  valid_determinant_attrs,
  valid_determinant_nonfixed_indices,
  attr_names,
  rhs_nonfixed_indices,
  accuracy = 1,
  full_cache = TRUE,
  store_cache = TRUE,
  detset_limit = ncol(lookup) - 1L,
  report = reporter(report = FALSE, con = "", new = TRUE)
) {
  # Maximum size of determinant set for a dependant is number
  # of other valid determinants.
  # If there are dependants that aren't valid determinants,
  # this is number of valid determinant attributes. If there
  # aren't, subtract one.
  n_dependant_only <- length(setdiff(valid_dependant_attrs, valid_determinant_attrs))
  max_n_lhs_attrs <- length(valid_determinant_attrs) -
    as.integer(n_dependant_only == 0)
  # using 0 would allow for one more column, but makes indexing a pain
  lhs_attrs_limit <- floor(log(.Machine$integer.max, 2))
  if (max_n_lhs_attrs > lhs_attrs_limit)
    stop(paste(
      "only data.frames with up to",
      lhs_attrs_limit,
      "columns possible in a determinant set currently supported"
    ))

  dependencies <- unflatten(list(), attr_names)
  # main search
  if (max_n_lhs_attrs > 0) {
    report("constructing powerset")
    powerset <- nonempty_powerset(
      max_n_lhs_attrs,
      use_visited = TRUE
    )
    # cache generated powerset and reductions, otherwise we spend a lot
    # of time duplicating reduction work
    all_powersets <- stats::setNames(list(powerset), max_n_lhs_attrs)
    partition_handler <- checkable_partition_handler(
      unname(lookup),
      key_class = "integer",
      accuracy = accuracy,
      full_cache = full_cache
    )
    for (rhs in rhs_nonfixed_indices) {
      report(paste("dependant", attr_names[rhs]))
      lhs_nonfixed_indices <- setdiff(valid_determinant_nonfixed_indices, rhs)
      n_lhs_attrs <- length(lhs_nonfixed_indices)
      stopifnot(n_lhs_attrs <= max_n_lhs_attrs)
      if (n_lhs_attrs == 0)
        next
      if (n_lhs_attrs %in% names(all_powersets))
        nodes <- all_powersets[[as.character(n_lhs_attrs)]]
      else{
        nodes <- reduce_powerset(powerset, n_lhs_attrs)
        all_powersets[[as.character(n_lhs_attrs)]] <- nodes
      }
      report("determinants available, starting search")
      lhss <- find_LHSs_dfd(
        rhs,
        lhs_nonfixed_indices,
        nodes,
        n_lhs_attrs,
        partition_handler,
        detset_limit
      )
      if (!store_cache)
        partition_handler$reset()
      dependencies[[attr_names[rhs]]] <- c(
        dependencies[[attr_names[rhs]]],
        lapply(lhss, \(x) attr_names[x])
      )
    }
  }

  report(paste0(
    "DFD complete",
    "\n",
    with_number(
      if (max_n_lhs_attrs == 0) 0L else partition_handler$cache_size(),
      "partition",
      " cached",
      "s cached"
    )
  ))
  dependencies
}

find_LHSs_dfd <- function(
  rhs,
  lhs_nonfixed_indices,
  nodes,
  n_lhs_attrs,
  partition_handler,
  detset_limit
) {
  # The original library "names" nodes with their attribute set,
  # so finding a node involves matching a character vector against
  # a list of character vectors. This is slow.
  # Instead, we assign each possible attribute set an integer,
  # by taking the integer that matches the bitset. This limits
  # us to floor(.Machine$integer.max) attributes as potential determinants.
  # We could use numerics to get to .Machine$double.digits determinant
  # attributes, but these would be arbitrary floats rather than integers,
  # so more work would be required to match indices etc.
  # Assigning the integers per find_LHSs call allows one more column,
  # since we don't need to include the dependant.
  # Node categories:
  # -3 = candidate maximal non-dependency
  # -2 = maximal non-dependency
  # -1 = non-maximal non-dependency
  #  0 = unresolved
  #  1 = non-minimal dependency
  #  2 = minimal dependency
  #  3 = candidate minimal dependency

  # initial seeds are the single-attribute nodes, possibly pruned by detset
  # constraints
  lhs_attr_nodes <- to_nodes(seq_len(n_lhs_attrs), nodes)
  initial_seeds <- lhs_attr_nodes
  seeds <- generate_next_seeds(
    max_non_deps = integer(),
    min_deps = integer(),
    initial_seeds = lhs_attr_nodes,
    nodes = nodes,
    detset_limit = detset_limit
  )
  min_deps <- integer()
  max_non_deps <- integer()
  trace <- integer()

  while (length(seeds) != 0) {
    node <- seeds[sample.int(length(seeds), 1)]
    while (!is.na(node)) {
      if (nodes$visited[node]) {
        if (nodes$category[node] == 3) { # dependency
          min_infer <- is_minimal(node, nodes)
          if (isTRUE(min_infer)) {
            nodes$category[node] <- 2L
            min_deps <- c(min_deps, node)
          }
          if (isFALSE(min_infer))
            nodes$category[node] <- 1L
        }
        if (nodes$category[node] == -3) { # non-dependency
          max_infer <- is_maximal(node, nodes)
          if (isTRUE(max_infer)) {
            nodes$category[node] <- -2L
            max_non_deps <- c(max_non_deps, node)
          }
          if (isFALSE(max_infer)) {
            nodes$category[node] <- -1L
          }
        }
        nodes$category[node] <- update_dependency_type(
          node,
          nodes,
          min_deps,
          max_non_deps
        )
      }else{
        inferred_type <- infer_type(node, nodes)
        if (!is.na(inferred_type)) {
          nodes$category[node] <- inferred_type
        }
        if (nodes$category[node] == 0L) {
          lhs_set <- lhs_nonfixed_indices[nodes$bits[[node]]]
          result <- partition_handler$check(
            rhs,
            lhs_set
          )
          if (result) {
            min_infer <- is_minimal(node, nodes)
            if (isFALSE(min_infer))
              nodes$category[node] <- 1L
            if (isTRUE(min_infer)) {
              min_deps <- c(min_deps, node)
              nodes$category[node] <- 2L
            }
            if (is.na(min_infer))
              nodes$category[node] <- 3L
          }else{
            max_infer <- is_maximal(node, nodes)
            if (isFALSE(max_infer)) {
              nodes$category[node] <- -1L
            }
            if (isTRUE(max_infer)) {
              max_non_deps <- c(max_non_deps, node)
              nodes$category[node] <- -2L
            }
            if (is.na(max_infer))
              nodes$category[node] <- -3L
          }
        }
        nodes$visited[node] <- TRUE
      }
      res <- pick_next_node(
        node,
        nodes,
        trace,
        min_deps,
        max_non_deps
      )
      trace <- res[[2]]
      nodes <- res[[3]]
      min_deps <- res[[4]]
      max_non_deps <- res[[5]]
      node <- res[[1]]
    }
    seeds <- generate_next_seeds(
      max_non_deps,
      min_deps,
      initial_seeds,
      nodes,
      detset_limit
    )
  }
  lapply(min_deps, \(md) lhs_nonfixed_indices[nodes$bits[[md]]])
}

pick_next_node <- function(node, nodes, trace, min_deps, max_non_deps) {
  if (nodes$category[node] == 3) { # candidate dependency
    s <- nonvisited_children(node, nodes)
    s <- remove_pruned_subsets(s, min_deps, nodes$bits)
    if (length(s) == 0) {
      min_deps <- c(min_deps, node)
      nodes$category[node] <- 2L
    }else{
      trace <- c(node, trace)
      return(list(min(s), trace, nodes, min_deps, max_non_deps))
    }
  }else if (nodes$category[node] == -3) { # candidate non-dependency
    s <- nonvisited_parents(node, nodes)
    s <- remove_pruned_supersets(s, max_non_deps, nodes$bits)
    if (length(s) == 0) {
      max_non_deps <- c(max_non_deps, node)
      nodes$category[node] <- -2L
    }else{
      trace <- c(node, trace)
      return(list(min(s), trace, nodes, min_deps, max_non_deps))
    }
  }
  if (length(trace) == 0)
    return(list(NA, trace, nodes, min_deps, max_non_deps))
  list(trace[1], trace[-1], nodes, min_deps, max_non_deps)
}

remove_pruned_subsets <- function(subsets, supersets, bitsets) {
  if (length(subsets) == 0 || length(supersets) == 0)
    return(subsets)
  check_pairs <- outer(
    bitsets[subsets],
    bitsets[supersets],
    Vectorize(is_subset)
  )
  prune <- apply(check_pairs, 1, any)
  subsets[!prune]
}

remove_pruned_supersets <- function(supersets, subsets, bitsets) {
  if (length(subsets) == 0 || length(supersets) == 0)
    return(supersets)
  check_pairs <- outer(
    bitsets[supersets],
    bitsets[subsets],
    Vectorize(is_superset)
  )
  prune <- rowSums(check_pairs) > 0
  supersets[!prune]
}

generate_next_seeds <- function(
  max_non_deps,
  min_deps,
  initial_seeds,
  nodes,
  detset_limit
) {
  # Seed generation assumes that the empty set is known to be a non-determinant.
  # The below is equivalent to beginning with a single empty seed, and having
  # the empty set as an additional non-determinant. Being able to refer to the
  # empty set directly would remove the special cases we have below for
  # max_non_deps being empty, and for applying the first one.
  # Nodes are completely unknown iff they're non-visited
  stopifnot(
    all(nodes$visited == (nodes$category != 0)),
    !any(abs(nodes$category) == 3)
  )
  if (length(max_non_deps) == 0) {
    # original DFD paper doesn't mention case where no maximal non-dependencies
    # found yet, so this approach could be inefficient
    seeds <- remove_pruned_subsets(
      initial_seeds,
      min_deps,
      nodes$bits
    )
    # At generation time, all seed nodes are non-categorised, and nodes in
    # general are non-categorised or minimal dependencies, because to be in this
    # case the only explored nodes must be single-attribute nodes found to be
    # minimal. i.e.:
    stopifnot(
      all(nodes$category[seeds] == 0),
      all(nodes$category %in% c(0L, 2L))
    )
  }else{
    seeds <- integer()
    for (n in seq_along(max_non_deps)) {
      nfd <- max_non_deps[[n]]
      max_non_dep_c <- remove_pruned_subsets(initial_seeds, nfd, nodes$bits)
      # paper condition is "seeds is empty", trimming cross-intersections
      # by detset_limit means seeds can be empty before the end,
      # which would cause the remaining nfds to start from scratch.
      # we therefore just initiate the seeds on the first nfd, as
      # intended anyway.
      if (n == 1) {
        seeds <- max_non_dep_c
      }else{
        seeds <- cross_intersection(seeds, max_non_dep_c, nodes, detset_limit)
      }
    }
  }
  remove_pruned_supersets(seeds, min_deps, nodes$bits)
}

cross_intersection <- function(seeds, max_non_dep, powerset, detset_limit) {
  new_seed_full_indices <- integer()
  for (dep in seeds) {
    seed_bitset <- powerset$bits[[dep]]
    for (set in max_non_dep) {
      set_bitset <- powerset$bits[[set]]
      new_seed_bitset <- seed_bitset | set_bitset
      if (sum(new_seed_bitset) > detset_limit)
        next
      new_seed_bitset_index <- to_bitset_index(which(new_seed_bitset))
      new_seed_full_indices <- c(new_seed_full_indices, new_seed_bitset_index)
    }
  }
  seeds <- powerset$bitset_index[new_seed_full_indices]
  seeds <- seeds[!is.na(seeds)]
  minimise_seeds(seeds, powerset$bits)
}

to_bitset_index <- function(bits) {
  sum(2^(bits - 1))
}

minimise_seeds <- function(seeds, bitsets) {
  if (length(seeds) == 0)
    return(seeds)
  # remove duplicates first instead of comparing identical bitsets
  unique_seeds <- unique(seeds)
  n_seeds <- length(unique_seeds)
  include <- rep(TRUE, length(unique_seeds))
  for (n in seq_len(n_seeds - 1)) {
    if (include[n]) {
      for (m in seq.int(n + 1L, n_seeds)) {
        if (include[[m]]) {
          if (is_subset(bitsets[[unique_seeds[n]]], bitsets[[unique_seeds[m]]]))
            include[m] <- FALSE
          else{
            if (is_superset(bitsets[[unique_seeds[n]]], bitsets[[unique_seeds[m]]])) {
              include[n] <- FALSE
              break
            }
          }
        }
      }
    }
  }
  unique_seeds[include]
}

add_deps_implied_by_bijections <- function(
  dependencies,
  bijections,
  nonfixed,
  column_names
) {
  for (b in bijections) {
    # first is the one used in discovery
    first_index <- nonfixed[[b[[1]]]]
    # first determined by others
    dependencies[[first_index]] <- c(
      dependencies[[first_index]],
      nonfixed[b[-1]]
    )
    # non-first determined by first and each other
    for (nonfixed_index in b[-1]) {
      replacement <- nonfixed[[nonfixed_index]]
      dependencies[[replacement]] <- c(
        first_index,
        setdiff(dependencies[[first_index]], replacement)
      )
      stopifnot(!anyDuplicated(dependencies[[nonfixed_index]]))
    }
    # non-first can substitute for first in determinants
    for (rhs in setdiff(seq_along(dependencies), match(nonfixed[b], column_names))) {
      for (nonfixed_index in b[-1]) {
        replacement <- nonfixed[[nonfixed_index]]
        dependencies[[rhs]] <- union(
          dependencies[[rhs]],
          lapply(
            Filter(\(d) is.element(first_index, d), dependencies[[rhs]]),
            \(d) c(setdiff(d, first_index), replacement)
          )
        )
        stopifnot(!anyDuplicated(dependencies[[rhs]]))
      }
    }
  }
  dependencies
}

add_deps_implied_by_simple_keys <- function(
  dependencies,
  determinant_keys,
  dependant_keys,
  valid_dependant_attrs
) {
  # non-first keys have same non-key determinants as first, plus other keys
  if (length(dependant_keys) > 0) {
    first_dep <- dependant_keys[[1]]
    deps <- setdiff(dependencies[[first_dep]], as.list(determinant_keys))
    for (key in dependant_keys) {
      replacements <- setdiff(determinant_keys, key)
      dependencies[[key]] <- c(deps, as.list(replacements))
      stopifnot(!anyDuplicated(dependencies[[key]]))
    }
  }

  # non-first can substitute for first in compound determinants
  if (length(determinant_keys) > 0) {
    first_det <- determinant_keys[[1]]
    for (rhs in setdiff(valid_dependant_attrs, dependant_keys)) {
      for (replacement in determinant_keys[-1]) {
        dependencies[[rhs]] <- c(
          dependencies[[rhs]],
          lapply(
            Filter(
              \(d) is.element(first_det, d) && length(d) > 1,
              dependencies[[rhs]]
            ),
            \(d) c(setdiff(d, first_det), replacement)
          )
        )
        stopifnot(!anyDuplicated(dependencies[[rhs]]))
      }
    }
  }

  dependencies
}

flatten <- function(dependencies) {
  result <- list()
  for (i in seq_along(dependencies)) {
    rhs <- names(dependencies)[i]
    result <- c(
      result,
      lapply(dependencies[[i]], \(lhs) list(lhs, rhs))
    )
  }
  result
}

unflatten <- function(flattened, attrs_order) {
  split(
    lapply(flattened, `[[`, 1),
    factor(
      vapply(flattened, `[[`, character(1), 2),
      attrs_order
    )
  )
}

filter_nonflat_dependencies <- function(
  dependencies,
  detset_limit
) {
  lapply(
    dependencies,
    \(x) {
      if (length(x) == 0)
        return(x[FALSE])
      wanted <- lengths(x) <= detset_limit
      x[wanted]
    }
  )
}
