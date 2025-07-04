refineable_partition_handler <- function(lookup, key_class) {
  # Gives a list of functions that handle lookup and calculation of stripped
  # partitions, encapsulating dependence on the partitions cache and the
  # original lookup table. The intention is for all use of lookup to be done
  # through the "interface" provided by these functions, so the main code can
  # focus on implementing the search.

  # The partitions UI encapsulates interacting with the partition cache.
  partitions_ui <- partitions_ui(lookup, key_class = key_class)
  critical_ui <- critical_ui(lookup, key_class = key_class)

  initial_cache <- list(
    key = vapply(
      seq_along(lookup),
      \(set) partitions_ui$hash(partitions_ui$key(set)),
      character(1)
    ),
    value = lapply(unname(as.list(lookup)), pli)
  )
  partition_cache <- initial_cache
  diffset_cache <- list()
  trace_cache <- list(list(
    # element is indices of diffsets not covered by current S
    # empty attribute set covers none of the zero starting diffsets
    uncov = integer(),
    critical = matrix(
      list(),
      nrow = 0,
      ncol = ncol(lookup),
      dimnames = list(
        character(),
        vapply(
          partitions_ui$decompose_key(partitions_ui$full_key),
          partitions_ui$hash,
          character(1)
        )
      )
    )
  ))

  # These functions encapsulate the cache itself, including modification.
  reset_cache <- function(initial_cache) {
    partition_cache <<- initial_cache
  }
  fetch_partition <- function(key, lookup) {
    res <- fetch_stripped_partition_full_cache(
      partitions_ui$unkey(key),
      key,
      lookup,
      partition_cache,
      partitions_ui
    )
    partition_cache <<- res[[2]]
    res[[1]]
  }

  # These functions encapsulate the difference sets.
  add_diffsets <- function(diffsets) {
    # known to be new non-redundant diffsets
    len <- length(diffset_cache)
    diffset_cache <<- c(diffset_cache, diffsets)
    trace_cache <<- lapply(
      trace_cache,
      \(cache) {
        cache$uncov <- c(cache$uncov, len + seq_along(diffsets))
        cache
      }
    )
  }
  push_node <- function(S_key, W_key, new_S_element, removed_W) {
    # prepares a node's cache information, making use of its parent's info
    # if new_S is empty, this is a W reduction (mu_0)
    # otherwise, it's an S growth (mu_i)
    tlen <- length(trace_cache)
    new_W_key <- partitions_ui$subkey_difference(W_key, removed_W)

    new_cache <- trace_cache[[tlen]]
    old_uncov_cache <- new_cache$uncov

    # remove any critical edge info for removed W elements
    new_cache$critical <- new_cache$critical[
      ,
      vapply(partitions_ui$decompose_key(new_W_key), partitions_ui$hash, character(1)),
      drop = FALSE
    ]

    if (any(new_S_element != 0)) {
      # remove any uncovered edges now covered by new_S, or irrelevant for
      # remaining W
      new_cache$uncov <- new_cache$uncov[vapply(
        new_cache$uncov,
        \(int) {
          ds <- diffset_cache[[int]]
          any((ds & new_W_key) > 0) && all((ds & new_S_element) == 0)
        },
        logical(1)
      )]

      # expand critical matrix for new S
      new_cache$critical <- rbind(
        new_cache$critical,
        matrix(
          list(integer()),
          nrow = 1,
          ncol = ncol(new_cache$critical),
          dimnames = list(
            partitions_ui$hash(new_S_element),
            colnames(new_cache$critical)
          )
        )
      )

      for (A_key in partitions_ui$decompose(new_W_key)) {
        for (S_element_key in partitions_ui$decompose_key(S_key)) {
          # remove old critical diffsets that include new S element, if any
          old <- new_cache$critical[[
            partitions_ui$hash(S_element_key),
            partitions_ui$hash(A_key)
          ]]
          new <- old[vapply(
            diffset_cache[old],
            \(ds) all((ds & new_S_element) == 0),
            logical(1)
          )]
          new_cache$critical[[
            partitions_ui$hash(S_element_key),
            partitions_ui$hash(A_key)
          ]] <- new
        }
        # store version with new_S_element as S_element
        old <- get_uncovered_indices_bitset_pure(
          A_key,
          diffset_cache,
          old_uncov_cache
        )
        new <- get_uncovered_indices_bitset_pure(
          A_key,
          diffset_cache,
          new_cache$uncov
        )
        new_cache$critical[[
          partitions_ui$hash(new_S_element),
          partitions_ui$hash(A_key)
        ]] <- old[!is.element(old, new)]
      }
    }

    trace_cache <<- c(trace_cache, list(new_cache))
    invisible(NULL)
  }
  get_diffsets <- function() {
    diffset_cache
  }
  stopifnot(key_class == "bitset")
  get_uncovered_keys_bitset <- function(S_key, W_key, diffsets = diffset_cache) {
    get_uncovered_keys_bitset_pure(
      W_key,
      diffsets,
      trace_cache[[length(trace_cache)]]$uncov
    )
  }
  get_critical_bitset <- function(S_element_key, A_key, S_key, diffsets) {
    trace_cache[[length(trace_cache)]]$critical[[
      partitions_ui$hash(S_element_key),
      partitions_ui$hash(A_key)
    ]]
  }
  truncate <- function(n) {
    trace_cache <<- trace_cache[seq_len(min(n, length(trace_cache)))]
  }

  list(
    truncate = function(n) truncate(n),
    cache_size = function() length(partition_cache$key),
    reset = function() reset_cache(initial_cache),
    key = partitions_ui$key,
    empty_key = partitions_ui$key(integer()),
    full_key = partitions_ui$full_key,
    key_size = partitions_ui$key_size,
    decompose_key = partitions_ui$decompose_key,
    invert_key = partitions_ui$invert_key,
    subkey_difference = partitions_ui$subkey_difference,
    refine = function(rhs_key, lhs_key) {
      fetch_refined_partition(
        lookup,
        partitions_ui$unkey(rhs_key),
        lhs_key,
        fetch_partition
      )
    },
    add_diffset_keys = function(diffset_keys) add_diffsets(diffset_keys),
    get_diffset_keys = function() get_diffsets(),
    fetch_uncovered_keys = function(S_key, W_key, ...) {
      get_uncovered_keys_bitset(S_key, W_key, ...)
    },
    fetch_critical_indices = function(S_element, A, S) {
      get_critical_bitset(S_element, A, S, diffset_cache)
    },
    fetch_critical_diffsets = function(S_element, A, S) {
      diffset_cache[get_critical_bitset(S_element, A, S, diffset_cache)]
    },
    prepare_growS = push_node,
    current_cache = function() trace_cache[[length(trace_cache)]],
    trace_size = function() length(trace_cache)
  )
}

checkable_partition_handler <- function(lookup, key_class, accuracy, full_cache) {
  # Gives a list of functions that handle lookup and calculation of stripped
  # partitions, encapsulating dependence on the partitions cache and the
  # original lookup table. The intention is for all use of lookup to be done
  # through the "interface" provided by these functions, so the main code can
  # focus on implementing the search.

  # The checkable partitions handler takes some additional arguments on top of
  # lookup, since its use in DFD has some further requirements.
  # accuracy is a threshold for FD correctness.
  # cache is a logical indicating whether partitions are cached at all.
  # As with lookup, the intention is all use of these arguments to be done
  # through the resulting interface.

  # The partitions UI encapsulates interacting with the partition cache.
  partitions_ui <- partitions_ui(lookup, key_class = key_class)

  initial_cache <- list()
  partition_cache <- initial_cache

  # These functions encapsulate the cache itself, including modification.
  reset_cache <- function(initial_cache) {
    partition_cache <<- initial_cache
  }
  fetch_rank <- if (full_cache)
    function(set, lookup) {
      result <- fetch_stripped_partition_full_cache(
        set,
        partitions_ui$key(set),
        lookup,
        partition_cache,
        partitions_ui
      )
      partition_cache <<- result[[2]]
      partition_rank(result[[1]])
    }
  else
    function(set, lookup) {
      result <- fetch_rank_rank_cache(
        set,
        lookup,
        partition_cache,
        partitions_ui
      )
      partition_cache <<- result[[2]]
      result[[1]]
    }

  threshold <- ceiling(nrow(lookup)*accuracy)
  limit <- nrow(lookup) - threshold
  if (limit == 0L)
    # exact dependences have no need to calculate FD error (e(X -> Y))
    return(list(
      reset = function() reset_cache(initial_cache),
      check = function(rhs_set, lhs_set) {
        exact_dependencies(
          lookup,
          rhs_set,
          lhs_set,
          fetch_rank
        )
      },
      cache_size = function() length(partition_cache$key)
    ))
  fetch_error <- if (full_cache)
    function(lookup, rhs_set, lhs_set) {
      fetch_error_full_cache(lookup, rhs_set, lhs_set, partition_cache, partitions_ui)
    }
  else
    function(lookup, rhs_set, lhs_set)
      fetch_error_no_cache(lookup, rhs_set, lhs_set, partition_cache, partitions_ui)

  list(
    cache_size = function() length(partition_cache$key),
    reset = function() reset_cache(initial_cache),
    check = function(rhs_set, lhs_set) {
      approximate_dependencies(
        lookup,
        rhs_set,
        lhs_set,
        limit,
        fetch_rank,
        fetch_error
      )
    }
  )
}

partitions_ui <- function(lookup, key_class = c("bitset", "integer")) {
  # The partitions UI encapsulates interacting with the partition cache.

  # General partitions UI elements:
  # Set: original values, that subset the original lookup table
  # Key: transformed set; what is usually passed around
  # Hash: key transformed into a format for subsetting (lookup uses hash, not key)

  # Functions, in Haskell-like notation:
  # key: Set -> Key
  # component_keys: Set -> [Key] (equivalent to key >> decompose_key)
  # hash: Key -> Hash
  # unkey: Key -> Set
  # key_size: Key -> Int (equivalent to unkey >> length)
  # decompose_key: Key -> [Key] (per atomic element; unkey >> component_keys)
  # invert_key: Key -> Key (bitwise negation on the used bits only)
  # key_union: Key -> Key -> Key
  # distinct_key_union: Key -> Key -> Key (faster key_union for distinct keys)
  # key_difference: Key -> Key -> Key
  # subkey_difference: Key -> Key -> Key (faster key_difference for subkey)
  # lookup_hash: Hash -> Partitions -> Option<Index>
  # get_with_index: Index -> Partitions -> StrippedPartition
  # calculate: Set -> StrippedPartition (calculate from lookup)
  # add: Hash -> Partition -> Partitions -> Partitions
  key_class <- match.arg(key_class)
  switch(
    key_class,
    bitset = bitset_partitions_ui(lookup),
    integer = integer_partitions_ui(lookup)
  )
}

bitset_partitions_ui <- function(lookup) {
  # Bitset partitions UI types:
  # Set: a no-duplicate integer vector, for which columns of lookup to use
  # Key: a bitset
  # Hash: the bitset, with its bytes pasted together to make a string
  bitlen <- 8*ceiling(ncol(lookup)/8)
  key <- function(set) {
    bools <- rep(FALSE, bitlen)
    bools[set] <- TRUE
    fpack(bools)
  }
  full_key <- fpack(c(
    rep(TRUE, ncol(lookup)),
    rep(FALSE, bitlen - ncol(lookup))
  ))
  component_keys <- function(set) lapply(set, key)
  hash <- function(key) paste(key, collapse = "")
  unkey <- function(key) which(rawToBits(key) == 1)
  subkey_difference <- function(key, subkey) key & !subkey # xor is slower
  decompose_key <- function(key) component_keys(unkey(key))

  list(
    key = key,
    full_key = full_key,
    component_keys = component_keys,
    hash = hash,
    unkey = unkey,
    key_size = function(key) length(unkey(key)),
    decompose_key = decompose_key,
    invert_key = function(key) !key & full_key,
    key_union = function(key1, key2) key1 | key2,
    distinct_key_union = function(key1, key2) key1 | key2,
    key_difference = function(key1, key2) key1 & !key2,
    subkey_difference = subkey_difference,
    lookup_hash = function(hash, partitions) match(hash, partitions$key),
    get_with_index = function(index, partitions) partitions$value[[index]],
    calculate = function(set) {
      sp <- fsplit_rows_emptyable(lookup, set)
      unname(sp[lengths(sp) > 1])
    },
    add = function(hash, partition, partitions) {
      partitions$key <- c(partitions$key, hash)
      partitions$value <- c(partitions$value, list(partition))
      partitions
    }
  )
}

integer_partitions_ui <- function(lookup) {
  # Integer partitions UI types:
  # Set: a no-duplicate integer vector, for which columns of lookup to use
  # Key: an integer
  # Hash: an integer, for direct subsetting
  key <- function(set) as.integer(sum(2^(set - 1L)))
  full_key <- key(seq_len(ncol(lookup)))
  component_keys <- function(set) as.list(as.integer(2^(set - 1L)))
  unkey <- function(key) which(intToBits(key) == 1)
  subkey_difference <- function(key, subkey) key - subkey
  decompose_key <- function(key) component_keys(unkey(key))

  list(
    key = key,
    component_keys = component_keys,
    hash = identity,
    unkey = unkey,
    key_size = function(key) length(unkey(key)),
    decompose_key = decompose_key,
    invert_key = function(key) bitwXor(key, full_key),
    key_union = function(key1, key2) bitwOr(key1, key2),
    distinct_key_union = function(key1, key2) key1 + key2,
    key_difference = function(key1, key2) bitwAnd(key1, bitwNot(key2)),
    subkey_difference = subkey_difference,
    lookup_hash = function(hash, partitions) match(hash, partitions$key),
    get_with_index = function(index, partitions) partitions$value[[index]],
    calculate = function(set) {
      sp <- fsplit_rows(lookup, set)
      unname(sp[lengths(sp) > 1])
    },
    add = function(hash, partition, partitions) {
      partitions$key <- c(partitions$key, hash)
      partitions$value <- c(partitions$value, list(partition))
      partitions
    }
  )
}

critical_ui <- function(lookup, key_class = c("bitset")) {
  key_class <- match.arg(key_class)
  switch(
    key_class,
    bitset = bitset_critical_ui(lookup)
  )
}

bitset_critical_ui <- function(lookup) {
  bitlen <- 8*ceiling(ncol(lookup)/8)
  key <- function(set) {
    bools <- rep(FALSE, bitlen)
    bools[set] <- TRUE
    fpack(bools)
  }
  unkey <- function(key) which(rawToBits(key) == 1)
  hash <- function(key) paste(key, collapse = "")
  component_keys <- function(set) lapply(set, key)
  decompose_key <- function(key) component_keys(unkey(key))
  list(
    key = key,
    unkey = unkey,
    decompose_key = decompose_key,
    hash = hash,
    lookup_hash = function(hash, cache) match(hash, cache$key),
    get_with_index = function(index, cache) cache$value[[index]],
    calculate = function(S_element_key, A_key, S_key, diffsets) {
      critical(S_element_key, A_key, S_key, diffsets)
    },
    add = function(hash, value, cache) {
      cache$key <- c(cache$key, hash)
      cache$value <- c(cache$value, list(value))
      cache
    },
    add_new = function(hash, value, cache) {
      stopifnot(is.na(match(hash, cache$key)))
      cache$key <- c(cache$key, hash)
      cache$value <- c(cache$value, list(value))
      cache
    },
    remove = function(hash, cache) {
      index <- match(hash, cache$key)
      stopifnot(!is.na(index))
      cache$key <- cache$key[-index]
      cache$value <- cache$value[-index]
      cache
    },
    modify = function(hash, value, cache) {
      index <- match(hash, cache$key)
      stopifnot(!is.na(index))
      cache$value[[index]] <- value
      cache
    }
  )
}

exact_dependencies <- function(
  lookup,
  rhs_set,
  lhs_set,
  fetch_rank
) {
  # compare to approximate_dependencies:
  # limit = 0 simplifies the early exit conditions,
  # and if lhs_rank - union_rank > 0 then they're equal,
  # so error = 0.
  lhs_result <- fetch_rank(lhs_set, lookup)
  lhs_rank <- lhs_result[[1]]
  if (lhs_rank == 0)
    return(TRUE)
  union_rank <- fetch_rank(union(lhs_set, rhs_set), lookup)
  union_rank == lhs_rank
}

approximate_dependencies <- function(
  lookup,
  rhs_set,
  lhs_set,
  limit,
  fetch_rank,
  fetch_error
) {
  # cheaper bounds checks:
  # lhs_rank - union_rank <= error <= lhs_rank is always true,
  # (paper version: e(X) - e(X /\ Y) <= e(X -> Y) <= e(X))
  # and LHS -> RHS is approximately true if error <= limit,
  # so if lhs_rank <= limit or limit < lhs_rank - union_rank
  # then we can skip the calculation of error.
  lhs_rank <- fetch_rank(lhs_set, lookup)
  if (lhs_rank <= limit)
    return(TRUE)
  union_rank <- fetch_rank(union(lhs_set, rhs_set), lookup)
  if (lhs_rank - union_rank > limit)
    return(FALSE)

  error <- fetch_error(lookup, rhs_set, lhs_set)
  error <= limit
}

fetch_stripped_partition_full_cache <- function(
  set,
  key,
  lookup,
  partitions,
  partitions_ui
) {
  hash <- partitions_ui$hash(key)
  input <- list(set, key, lookup, partitions, partitions_ui)
  calculate <- function(set, key, lookup, partitions, partitions_ui) {
    key_elements <- partitions_ui$component_keys(set)
    child_keys <- lapply(key_elements, partitions_ui$subkey_difference, key = key)
    child_hashes <- lapply(child_keys, partitions_ui$hash)
    child_indices <- vapply(
      child_hashes,
      partitions_ui$lookup_hash,
      integer(1L),
      partitions
    )
    n_children <- sum(!is.na(child_indices))

    if (n_children == 0)
      return(partitions_ui$calculate(set))
    if (n_children > 1) {
      chosen_indices <- which(!is.na(child_indices))[1:2]
      return(stripped_partition_product(
        partitions_ui$get_with_index(child_indices[[chosen_indices[[1]]]], partitions),
        partitions_ui$get_with_index(child_indices[[chosen_indices[[2]]]], partitions),
        nrow(lookup)
      ))
    }
    existing_child_position <- which(!is.na(child_indices))
    remainder_element <- set[[existing_child_position]]
    remainder_key <- key_elements[[existing_child_position]]
    existing_child_index <- child_indices[[existing_child_position]]
    existing_child_sp <- partitions_ui$get_with_index(
      existing_child_index,
      partitions
    )
    remainder_result <- fetch_stripped_partition_full_cache(
      remainder_element,
      remainder_key,
      lookup,
      partitions,
      partitions_ui
    )
    remainder_sp <- remainder_result[[1]]
    partitions <- remainder_result[[2]]
    stripped_partition_product(
      existing_child_sp,
      remainder_sp,
      nrow(lookup)
    )
  }
  fetch_pure(hash, input, partitions, partitions_ui, calculate)
}

fetch_refined_partition <- function(
  lookup,
  rhs_set,
  lhs_key,
  fetch_partition
) {
  lhs_partition <- fetch_partition(lhs_key, lookup)
  rhs_lookups <- lapply(rhs_set, \(r) lookup[[r]])

  rhs_joint_lookup <- if (length(rhs_lookups) == 1)
    rhs_lookups[[1]]
  else {
    do.call(paste, unname(rhs_lookups)) |>
      lookup_indices()
  }
  relevant_lhs_partition <- filter_partition(lhs_partition, rhs_joint_lookup)
  if (partition_rank(relevant_lhs_partition) == 0)
    return(list(rep(list(list()), length(rhs_lookups)), list()))
  list(
    lapply(
      rhs_lookups,
      refine_partition_by_lookup,
      relevant_partition = relevant_lhs_partition
    ),
    relevant_lhs_partition
  )
}

fetch_rank_rank_cache <- function(
  set,
  lookup,
  partitions,
  partitions_ui
) {
  # This only returns the number |p| of equivalence classes in the partition p,
  # not its contents. This is less demanding on memory than storing stripped
  # partitions, but we cannot efficiently calculate the partition for supersets.
  hash <- partitions_ui$hash(partitions_ui$key(set))
  input <- list(set, lookup)
  calculate <- function(set, lookup) {
    lookup_set_only <- lookup[, set, drop = FALSE]
    sum(duplicated(lookup_set_only))
  }
  fetch_pure(hash, input, partitions, partitions_ui, calculate)
}

fetch_error_full_cache <- function(
  lookup,
  rhs_set,
  lhs_set,
  partitions,
  partitions_ui
) {
  # e(lhs_set -> rhs)
  lhs_key <- partitions_ui$key(lhs_set)
  lhs_hash <- partitions_ui$hash(lhs_key)
  rhs_keys <- partitions_ui$component_keys(rhs_set)
  stopifnot(length(rhs_keys) == 1)
  rhs_key <- rhs_keys[[1]]
  lhs_index <- partitions_ui$lookup_hash(lhs_hash, partitions)
  stopifnot(!is.na(lhs_index))
  lhs_sp <- partitions_ui$get_with_index(lhs_index, partitions)
  union_key <- partitions_ui$distinct_key_union(lhs_key, rhs_key)
  union_index <- partitions_ui$lookup_hash(partitions_ui$hash(union_key), partitions)
  stopifnot(!is.na(union_index))
  union_sp <- partitions_ui$get_with_index(union_index, partitions)
  stripped_partition_error(lhs_sp, union_sp, nrow(lookup))
}

fetch_error_no_cache <- function(
  lookup,
  rhs_set,
  lhs_set,
  partitions,
  partitions_ui
) {
  # This is a quick working version I put together to replace the non-working
  # original. The quicker version from Tane requires cache = TRUE for stripped
  # partition information.
  splitted <- lookup[[rhs_set]]
  splitter <- lookup[, lhs_set, drop = FALSE]
  rhs_value_lhs_partition <- fsplit(splitted, splitter)
  value_partition_error(rhs_value_lhs_partition, nrow(lookup))
}

fsplit_rows_emptyable <- function(lookup, set) {
  if (length(set) == 0)
    return(list(seq_len(nrow(lookup))))
  fsplit_rows(lookup, set)
}

fsplit_rows <- function(lookup, set) {
  fsplit(seq_len(nrow(lookup)), lookup[, set, drop = FALSE])
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

get_uncovered_indices_bitset_pure <- function(
  W_key,
  diffsets,
  uncov_cache
) {
  has_any_W <- vapply(diffsets[uncov_cache], \(ds) any((W_key & ds) > 0), logical(1))
  uncov_cache[has_any_W]
}

get_uncovered_keys_bitset_pure <- function(
  W_key,
  diffsets,
  uncov_cache
) {
  has_any_W <- vapply(diffsets[uncov_cache], \(ds) any((W_key & ds) > 0), logical(1))
  diffsets[uncov_cache][has_any_W]
}

fetch_pure <- function(hash, input, cache, ui, calculate) {
  index <- ui$lookup_hash(hash, cache)
  if (!is.na(index))
    return(list(ui$get_with_index(index, cache), cache))
  result <- do.call(calculate, input)
  cache <- ui$add(hash, result, cache)
  list(result, cache)
}

# match.arg is quickest when passed NULL as the argument
fpack <- function(x) packBits(x, NULL)
