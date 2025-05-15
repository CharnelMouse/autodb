DFD <- function(
  lookup,
  accuracy = 1,
  full_cache = TRUE,
  store_cache = TRUE,
  skip_bijections = FALSE,
  determinants = seq_along(lookup),
  dependants = seq_along(lookup),
  detset_limit = ncol(lookup) - 1L,
  report = reporter(report = FALSE, con = "", new = TRUE)
) {
  attrs <- seq_along(lookup)
  attr_names <- names(lookup)
  n_cols <- length(attrs)

  partitions <- list()
  dependencies <- stats::setNames(rep(list(list()), n_cols), attr_names)

  # check for constant-value columns, because if columns are fixed we can
  # ignore them for the rest of the search
  fixed <- integer()
  for (attr in attrs) {
    if (all(lookup[[attr]] == 1L)) {
      fixed <- report$op(fixed, c, paste(attr_names[[attr]], "is fixed"), attr)
      if (attr %in% dependants)
        dependencies[[attr]] <- list(character())
    }
  }
  nonfixed <- setdiff(attrs, fixed)

  valid_dependant_attrs <- intersect(dependants, nonfixed)
  # check for zero dependants before removing simple keys, otherwise
  # returning early would leave out the simple-key results
  if (
    length(valid_dependant_attrs) == 0 ||
    detset_limit < 1
  ) {
    report$stat("no valid dependants, or detset_limit < 1, skipping search")
    return(flatten(
      filter_nonflat_dependencies(dependencies, detset_limit),
      attr_names
    ))
  }

  # For non-fixed non-key attributes, all can be dependants,
  # but might not all be valid determinants.
  valid_determinant_attrs_prekeys <- intersect(
    nonfixed,
    determinants
  )

  # Non-fixed attributes might be single-attribute keys: we can list them as
  # determining all other non-fixed attributes, use them in the main search only
  # as dependants. If there are several single-attribute keys, and we're
  # skipping bijections, then we can also remove all but one of them as
  # dependants.
  valid_determinant_attrs <- valid_determinant_attrs_prekeys
  # Can't just check column values are seq_len(nrow(df)),
  # because df can have duplicate rows, and we can't remove
  # the duplicate rows in df, because it changes the behaviour
  # for accuracy < 1.
  df_uniq <- df_unique(lookup)
  simple_keys <- nonfixed[vapply(
    df_uniq[nonfixed],
    Negate(anyDuplicated),
    logical(1)
  )]
  determinant_keys <- intersect(simple_keys, valid_determinant_attrs_prekeys)
  dependant_keys <- intersect(simple_keys, valid_dependant_attrs)
  if (length(simple_keys) > 0) {
    report$stat(paste("single-attribute keys:", toString(attr_names[simple_keys])))
    valid_determinant_attrs <- setdiff(valid_determinant_attrs, simple_keys)
    if (skip_bijections) {
      valid_dependant_attrs <- setdiff(valid_dependant_attrs, dependant_keys[-1])
    }
  }

  if (length(valid_determinant_attrs) < n_cols) {
    report$stat(
      paste(
        "attributes not considered as determinants:",
        toString(attr_names[-valid_determinant_attrs])
      )
    )
  }
  valid_determinant_nonfixed_indices <- match(valid_determinant_attrs, nonfixed)

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
  bijections <- list()

  # main search
  if (max_n_lhs_attrs > 0) {
    powerset <- report$op(
      max_n_lhs_attrs,
      nonempty_powerset,
      "constructing powerset",
      use_visited = TRUE
    )
    # cache generated powerset and reductions, otherwise we spend a lot
    # of time duplicating reduction work
    all_powersets <- stats::setNames(list(powerset), max_n_lhs_attrs)
    compute_partitions <- partition_computer(
      unname(lookup[, nonfixed, drop = FALSE]),
      accuracy,
      full_cache
    )
    for (rhs in which(nonfixed %in% valid_dependant_attrs)) {
      report$stat(paste("dependant", attr_names[nonfixed][rhs]))
      lhs_nonfixed_indices <- setdiff(valid_determinant_nonfixed_indices, rhs)
      n_lhs_attrs <- length(lhs_nonfixed_indices)
      expected_n_lhs_attrs <- max_n_lhs_attrs -
        (n_dependant_only > 0 && is.element(rhs, valid_determinant_nonfixed_indices))
      stopifnot(n_lhs_attrs == expected_n_lhs_attrs)
      bijection_candidate_nonfixed_indices <- if (skip_bijections)
        match(
          names(dependencies)[
            vapply(
              dependencies,
              \(x) any(vapply(x, identical, logical(1), attr_names[nonfixed][[rhs]])),
              logical(1)
            )
          ],
          attr_names[nonfixed]
        ) |>
        intersect(lhs_nonfixed_indices)
      else
        integer()
      if (n_lhs_attrs > 0) {
        if (n_lhs_attrs %in% names(all_powersets))
          nodes <- all_powersets[[as.character(n_lhs_attrs)]]
        else{
          nodes <- reduce_powerset(powerset, n_lhs_attrs)
          all_powersets[[as.character(n_lhs_attrs)]] <- nodes
        }
        lhss <- report$op(
          rhs,
          find_LHSs_dfd,
          "determinants available, starting search",
          lhs_nonfixed_indices,
          nodes,
          n_lhs_attrs,
          partitions,
          compute_partitions,
          bijection_candidate_nonfixed_indices,
          detset_limit,
          store_cache
        )
        if (lhss[[2 + store_cache]]) {
          stopifnot(
            is.element(lhss[[1]], bijection_candidate_nonfixed_indices),
            lhss[[1]] < rhs
          )
          bij_ind <- match(lhss[[1]], names(bijections))
          if (is.na(bij_ind))
            bijections <- c(
              bijections,
              stats::setNames(list(c(lhss[[1]], rhs)), lhss[[1]])
            )
          else{
            bijections[[bij_ind]] <- c(
              bijections[[bij_ind]],
              rhs
            )
          }
          valid_determinant_nonfixed_indices <- setdiff(
            valid_determinant_nonfixed_indices,
            rhs
          )
          max_n_lhs_attrs <- max_n_lhs_attrs - 1L
          if (max_n_lhs_attrs %in% names(all_powersets))
            powerset <- all_powersets[[as.character(max_n_lhs_attrs)]]
          else{
            powerset <- reduce_powerset(powerset, max_n_lhs_attrs)
            all_powersets[[as.character(max_n_lhs_attrs)]] <- powerset
          }
        }else
          dependencies[[attr_names[nonfixed][rhs]]] <- c(
            dependencies[[attr_names[nonfixed][rhs]]],
            lapply(lhss[[1]], \(x) attr_names[nonfixed][x])
          )
        if (store_cache)
          partitions <- lhss[[2]]
      }
    }
  }

  report$stat("DFD complete")
  dependencies <- add_simple_key_deps(
    dependencies,
    attr_names[determinant_keys],
    attr_names[dependant_keys],
    attr_names[valid_dependant_attrs]
  )
  if (skip_bijections) {
    dependencies <- add_deps_implied_by_bijections(
      dependencies,
      bijections,
      attr_names[nonfixed],
      attr_names
    )
    dependencies <- add_deps_implied_by_simple_keys(
      dependencies,
      attr_names[determinant_keys],
      attr_names[dependant_keys],
      attr_names[valid_dependant_attrs]
    )
  }
  flatten(
    filter_nonflat_dependencies(dependencies, detset_limit),
    attr_names
  )
}

find_LHSs_dfd <- function(
  rhs,
  lhs_nonfixed_indices,
  nodes,
  n_lhs_attrs,
  partitions,
  compute_partitions,
  bijection_candidate_nonfixed_indices,
  detset_limit,
  store_cache = FALSE
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
  bijection_nodes <- to_nodes(
    match(
      bijection_candidate_nonfixed_indices,
      lhs_nonfixed_indices
    ),
    nodes
  )

  while (length(seeds) != 0) {
    node <- seeds[sample.int(length(seeds), 1)]
    while (!is.na(node)) {
      if (nodes$visited[node]) {
        if (nodes$category[node] == 3) { # dependency
          min_infer <- is_minimal(node, nodes)
          if (isTRUE(min_infer)) {
            nodes$category[node] <- 2L
            min_deps <- c(min_deps, node)
            if (is.element(node, bijection_nodes)) {
              lhs_index <- lhs_nonfixed_indices[nodes$bits[[node]]]
              stopifnot(is.element(
                lhs_index,
                bijection_candidate_nonfixed_indices
              ))
              if (store_cache)
                return(list(lhs_index, partitions, TRUE))
              else
                return(list(lhs_index, TRUE))
            }
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
          cp <- compute_partitions(
            rhs,
            lhs_set,
            partitions
          )
          result <- cp[[1]]
          partitions <- cp[[2]]
          if (result) {
            min_infer <- is_minimal(node, nodes)
            if (isFALSE(min_infer))
              nodes$category[node] <- 1L
            if (isTRUE(min_infer)) {
              min_deps <- c(min_deps, node)
              nodes$category[node] <- 2L
              if (is.element(node, bijection_nodes)) {
                lhs_index <- lhs_nonfixed_indices[nodes$bits[[node]]]
                stopifnot(is.element(
                  lhs_index,
                  bijection_candidate_nonfixed_indices
                ))
                if (store_cache)
                  return(list(lhs_index, partitions, TRUE))
                else
                  return(list(lhs_index, TRUE))
              }
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
  if (store_cache)
    list(
      lapply(min_deps, \(md) lhs_nonfixed_indices[nodes$bits[[md]]]),
      partitions,
      FALSE
    )
  else
    list(
      lapply(min_deps, \(md) lhs_nonfixed_indices[nodes$bits[[md]]]),
      FALSE
    )
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

partition_computer <- function(df, accuracy, cache) {
  # A greatly-overcomplicated function generator.
  # Using the data lookup table to create partitions, and compare those
  # partitions to determine whether an FD is satisfied, requires a lot of
  # different information. The idea is to abstract away the information that
  # doesn't change, leaving FD validation as a function only of the determinant
  # set, the dependant, and a cache of already-computed partitions. This hides
  # the following non-changing information, leaving:
  # - The lookup table the partitions are based on (df)
  # - The accuracy threshold for FD correctness (accuracy)
  # - Whether partitions are cached at all (cache)
  # For DFD, this is the only part of the main search code where these arguments
  # are required, so compartmentalising them away in this manner makes the
  # search code a little more concise, and gets rid of a lot of repeated
  # branching conditional on non-changing information.

  threshold <- ceiling(nrow(df)*accuracy)

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

  check_FD_partition <- if (cache)
    function(attr_indices, df, partitions)
      check_FD_partition_stripped(attr_indices, df, partitions, partitions_ui)
  else
    function(attr_indices, df, partitions)
      check_FD_partition_nclass(attr_indices, df, partitions, partitions_ui)

  if (threshold < nrow(df)) {
    check_AD <- if (cache)
      function(df, rhs, lhs_set, partitions, threshold, limit)
        check_AD_cache(df, rhs, lhs_set, partitions, threshold, limit, partitions_ui)
    else
      function(df, rhs, lhs_set, partitions, threshold, limit)
        check_AD_nocache(df, rhs, lhs_set, partitions, threshold, limit, partitions_ui)
    function(rhs, lhs_set, partitions) {
      approximate_dependencies(
        df,
        rhs,
        lhs_set,
        partitions,
        threshold,
        check_FD_partition,
        check_AD
      )
    }
  }else
    function(rhs, lhs_set, partitions) {
      exact_dependencies(
        df,
        rhs,
        lhs_set,
        partitions,
        check_FD_partition
      )
    }
}

exact_dependencies <- function(
  df,
  rhs,
  lhs_set,
  partitions,
  check_FD_partition
) {
  res1 <- check_FD_partition(lhs_set, df, partitions)
  part_lhs <- res1[[1]]
  partitions <- res1[[2]]
  if (part_lhs == 0)
    return(list(TRUE, partitions))
  res2 <- check_FD_partition(union(lhs_set, rhs), df, partitions)
  part_union <- res2[[1]]
  partitions <- res2[[2]]
  list(part_union == part_lhs, partitions)
}

approximate_dependencies <- function(
  df,
  rhs,
  lhs_set,
  partitions,
  threshold,
  check_FD_partition,
  check_AD
) {
  limit <- nrow(df) - threshold
  # cheaper bounds checks:
  # nrow(df) - (part_lhs - part_union) <= majorities_total <= nrow(df) - part_lhs
  res1 <- check_FD_partition(lhs_set, df, partitions)
  part_lhs <- res1[[1]]
  partitions <- res1[[2]]
  if (part_lhs <= limit)
    return(list(TRUE, partitions))
  res2 <- check_FD_partition(union(lhs_set, rhs), df, partitions)
  part_union <- res2[[1]]
  partitions <- res2[[2]]
  if (part_lhs - part_union > limit)
    return(list(FALSE, partitions))

  check_AD(df, rhs, lhs_set, partitions, threshold, limit)
}

check_FD_partition_nclass <- function(
  attr_indices,
  df,
  partitions,
  partitions_ui
) {
  # This only returns the number |p| of equivalence classes in the partition p,
  # not its contents. This is less demanding on memory than storing stripped
  # partitions, but we cannot efficiently calculate the partition for supersets.
  partition_node <- to_partition_node(attr_indices)
  partkey <- partitions_ui$lookup_node(partition_node, partitions)
  if (!is.na(partkey)) {
    return(list(partitions_ui$get_with_index(partkey, partitions), partitions))
  }
  df_attrs_only <- df[, attr_indices, drop = FALSE]
  n_remove <- sum(duplicated(df_attrs_only))
  partitions <- partitions_ui$add_partition(partition_node, n_remove, partitions)
  list(n_remove, partitions)
}

check_FD_partition_stripped <- function(
  attr_indices,
  df,
  partitions,
  partitions_ui
) {
  attr_nodes <- to_partition_nodes(attr_indices)
  partition_node <- sum(attr_nodes)
  partkey <- partitions_ui$lookup_node(partition_node, partitions)
  if (!is.na(partkey)) {
    sp <- partitions_ui$get_with_index(partkey, partitions)
    return(list(partition_rank(sp), partitions))
  }
  subset_nodes <- partition_node - attr_nodes
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
    if (sum(!is.na(subsets_match)) == 1) {
      index <- which(!is.na(subsets_match))
      small_subset <- attr_indices[[index]]
      small_subset_node <- attr_nodes[[index]]
      main_partition <- partitions_ui$get_with_index(
        subsets_match[index],
        partitions
      )
      subres <- check_FD_partition_stripped(
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
      sp <- fsplit_rows(df, attr_indices)
      sp <- unname(sp[lengths(sp) > 1])
    }
  }
  partitions <- partitions_ui$add_partition(partition_node, sp, partitions)
  list(partition_rank(sp), partitions)
}

check_AD_cache <- function(
  df,
  rhs,
  lhs_set,
  partitions,
  threshold,
  limit,
  partitions_ui
) {
  # e(lhs_set -> rhs)
  lhs_set_partition_node <- to_partition_node(lhs_set)
  rhs_partition_node <- to_partition_nodes(rhs)
  ind_lhs <- partitions_ui$lookup_node(lhs_set_partition_node, partitions)
  stopifnot(!is.na(ind_lhs))
  classes_lhs <- partitions_ui$get_with_index(ind_lhs, partitions)
  classes_lhs_node <- lhs_set_partition_node + rhs_partition_node
  ind_union <- partitions_ui$lookup_node(classes_lhs_node, partitions)
  stopifnot(!is.na(ind_union))
  classes_union <- partitions_ui$get_with_index(ind_union, partitions)
  e <- 0L
  Ts <- integer()
  for (c in classes_union) {
    Ts[c[1]] <- length(c)
  }
  for (c in classes_lhs) {
    m <- 1L
    for (ts in c) {
      m <- max(m, Ts[ts], na.rm = TRUE)
    }
    e <- e + length(c) - m
  }
  list(e <= limit, partitions)
}

check_AD_nocache <- function(
  df,
  rhs,
  lhs_set,
  partitions,
  threshold,
  limit,
  partitions_ui
) {
  # This is a quick working version I put together to replace the non-working
  # original. The quicker version from Tane requires cache = TRUE for stripped
  # partition information.
  majority_size <- function(x) {
    max(tabulate(x))
  }
  splitted <- df[[rhs]]
  splitter <- df[, lhs_set, drop = FALSE]
  rhs_split <- fsplit(splitted, splitter)
  majorities_total <- sum(vapply(
    rhs_split,
    majority_size,
    integer(1)
  ))
  list(majorities_total >= threshold, partitions)
}

to_partition_node <- function(element_indices) {
  as.integer(sum(2^(element_indices - 1L)))
}

to_partition_nodes <- function(element_indices) {
  as.integer(2^(element_indices - 1L))
}

fsplit_rows <- function(df, attr_indices) {
  fsplit(seq_len(nrow(df)), df[, attr_indices, drop = FALSE])
}

fsplit <- function(splitted, splitter) {
  # Column contents are known to be integer, so we paste them together before
  # calling split. This is much faster than the iterated pasting of multiple f
  # elements done by interaction().
  # If there's known to be only one splitter, we're best off just calling
  # split, which calls as.factor, which has special handling for integers.
  # splitter is unnamed in case any attributes have names like "sep"
  # that would be used as arguments for paste
  single_splitter <- do.call(paste, unname(splitter))
  # determine levels manually to skip factor()'s default level sorting
  f <- ffactor1(single_splitter)
  split(splitted, f)
}

add_simple_key_deps <- function(
  dependencies,
  determinant_keys,
  dependant_keys,
  valid_dependant_attrs
) {
  nonkey_dependants <- setdiff(valid_dependant_attrs, dependant_keys)
  dependencies[nonkey_dependants] <- lapply(
    dependencies[nonkey_dependants],
    \(dets) c(as.list(determinant_keys), dets)
  )
  dependencies[dependant_keys] <- lapply(
    dependant_keys,
    \(key) c(as.list(setdiff(determinant_keys, key)), dependencies[[key]])
  )
  dependencies
}

add_deps_implied_by_bijections <- function(
  dependencies,
  bijections,
  nonfixed,
  column_names
) {
  for (b in bijections) {
    first_index <- nonfixed[[b[[1]]]]
    # add the bijection
    for (nonfixed_index in b[-1]) {
      replacement <- nonfixed[[nonfixed_index]]
      dependencies[[replacement]] <- c(
        first_index,
        setdiff(dependencies[[first_index]], replacement)
      )
      stopifnot(!anyDuplicated(dependencies[[nonfixed_index]]))
    }
    # add dependencies implied by the bijection
    # only needed when bijection attribute is earlier than dependant, since
    # later ones were added before the bijection was known
    for (rhs in setdiff(seq_along(dependencies), match(nonfixed[b], column_names))) {
      for (nonfixed_index in b[-1]) {
        replacement <- nonfixed[[nonfixed_index]]
        if (match(replacement, column_names) < rhs) {
          dependencies[[rhs]] <- union(
            dependencies[[rhs]],
            lapply(
              Filter(\(d) is.element(first_index, d), dependencies[[rhs]]),
              \(d) c(setdiff(d, first_index), replacement)
            )
          )
        }
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
  # transfer determinants of kept dependant key to others
  if (length(dependant_keys) > 0) {
    first_dep <- dependant_keys[[1]]
    deps <- setdiff(dependencies[[first_dep]], as.list(determinant_keys))
    for (key in dependant_keys) {
      replacements <- setdiff(determinant_keys, key)
      dependencies[[key]] <- c(deps, as.list(replacements))
      stopifnot(!anyDuplicated(dependencies[[key]]))
    }
  }

  # swap determinant keys around in compound determinants
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

flatten <- function(dependencies, attributes) {
  result <- list()
  for (i in seq_along(dependencies)) {
    rhs <- names(dependencies)[i]
    result <- c(
      result,
      lapply(dependencies[[i]], \(lhs) list(lhs, rhs))
    )
  }
  functional_dependency(result, attributes)
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
