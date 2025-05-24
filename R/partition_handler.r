bitset_partition_handler <- function(lookup) {
  # Gives a list of functions that handle lookup and calculation of stripped
  # partitions, encapsulating dependence on the partitions cache and the
  # original lookup table. The intention is for all use of lookup to be done
  # through the "interface" provided by these functions, so the main code can
  # focus on implementing the search.

  # The partitions UI encapsulates the partition cache.
  partitions_ui <- partitions_ui(lookup, key_class = "bitset")
  fetch_partition <- function(attrs_bitset, lookup, partitions) {
    fetch_partition_stripped(attrs_bitset, lookup, partitions, partitions_ui)
  }
  list(
    key = partitions_ui$key,
    key_size = partitions_ui$key_size,
    decompose_key = partitions_ui$decompose_key,
    refine = function(rhs_bitset, lhs_bitset, partitions) {
      fetch_refined_partition(
        lookup,
        partitions_ui$unkey(rhs_bitset),
        lhs_bitset,
        partitions,
        fetch_partition
      )
    }
  )
}

integer_partition_handler <- function(lookup, accuracy, cache) {
  # Gives a list of functions that handle lookup and calculation of stripped
  # partitions, encapsulating dependence on the partitions cache and the
  # original lookup table. The intention is for all use of lookup to be done
  # through the "interface" provided by these functions, so the main code can
  # focus on implementing the search.

  # The integer partitions handler takes some additional arguments on top of
  # lookup, since its use in DFD has some further requirements.
  # accuracy is a threshold for FD correctness.
  # cache is a logical indicating whether partitions are cached at all.
  # As with lookup, the intention is all use of these arguments to be done
  # through the resulting interface.

  # The partitions UI encapsulates the partition cache.
  partitions_ui <- partitions_ui(lookup, key_class = "integer")

  fetch_rank <- if (cache)
    function(attr_indices, lookup, partitions)
      fetch_rank_full_cache(attr_indices, lookup, partitions, partitions_ui)
  else
    function(attr_indices, lookup, partitions)
      fetch_rank_rank_cache(attr_indices, lookup, partitions, partitions_ui)

  threshold <- ceiling(nrow(lookup)*accuracy)
  limit <- nrow(lookup) - threshold
  if (limit > 0L) {
    fetch_error <- if (cache)
      function(lookup, rhs, lhs_set, partitions)
        fetch_error_withcache(lookup, rhs, lhs_set, partitions, partitions_ui)
    else
      function(lookup, rhs, lhs_set, partitions)
        fetch_error_nocache(lookup, rhs, lhs_set, partitions, partitions_ui)
    function(rhs, lhs_set, partitions) {
      approximate_dependencies(
        lookup,
        rhs,
        lhs_set,
        partitions,
        limit,
        fetch_rank,
        fetch_error
      )
    }
  }else
    function(rhs, lhs_set, partitions) {
      exact_dependencies(
        lookup,
        rhs,
        lhs_set,
        partitions,
        fetch_rank
      )
    }
}

partitions_ui <- function(lookup, key_class = c("bitset", "integer")) {
  # The partitions UI encapsulates the partition cache.

  # General partitions UI elements:
  # Set: original values, that subset the original lookup table
  # Key: transformed set; what is usually passed around
  # Hash: key transformed into a format for subsetting (lookup uses hash, not key)

  # Functions, in Haskell-like notation:
  # key: Set -> Key
  # component_keys: Set -> [Key] (equivalent to key >> decompose_key)
  # hash: Key -> Hash
  # unkey: Key -> Set
  # key_size: Key -> Int
  # decompose_key: Key -> [Key] (per atomic element; unkey >> component_keys)
  # key_children: Key -> [Key] (invert_key on atomic elements' keys, in order)
  # invert_key: Key -> Key (bitwise negation on the used bits only)
  # key_union: Key -> Key -> Key
  # distinct_key_union: Key -> Key -> Key (faster key_union for distinct keys)
  # key_difference: Key -> Key -> Key
  # subkey_difference: Key -> Key -> Key (faster key_difference for subkey)
  # lookup_hash: Hash -> Partitions -> Option<Index>
  # get_with_index: Index -> Partitions -> StrippedPartition
  # calculate_partition: Key -> StrippedPartition (calculate from lookup)
  # add_partition: Hash -> Partition -> Partitions -> Partitions
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
  full_key <- packBits(c(
    rep(TRUE, ncol(lookup)),
    rep(FALSE, bitlen - ncol(lookup))
  ))
  list(
    key = function(set) bitset(set, bitlen),
    component_keys = function(set) lapply(set, bitset, bitlen),
    hash = function(key) to_partition_node_char(key),
    unkey = function(key) unbitset(key),
    key_size = function(key) length(unbitset(key)),
    decompose_key = function(key) individual_bitsets(key),
    key_children = function(key) lapply(individual_bitsets(key), xor, key),
    invert_key = function(key) !key & full_key,
    key_union = function(key1, key2) key1 & key2,
    distinct_key_union = function(key1, key2) key1 & key2,
    key_difference = function(key1, key2) key1 & !key2,
    subkey_difference = function(key, subkey) key & !subkey, # xor is slower
    lookup_hash = function(hash, partitions) match(hash, partitions$key),
    get_with_index = function(index, partitions) partitions$value[[index]],
    calculate_partition = function(key) {
      attr_indices <- unbitset(key)
      sp <- fsplit_rows_emptyable(lookup, attr_indices)
      unname(sp[lengths(sp) > 1])
    },
    add_partition = function(hash, partition, partitions) {
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
  full_key <- to_partition_node(seq_len(ncol(lookup)))
  component_keys <- function(set) as.list(to_partition_nodes(set))
  unkey <- function(key) which(intToBits(key) == 1)
  subkey_difference <- function(key, subkey) key - subkey
  list(
    # we could use the partkey directly as an index into a list of
    # pre-allocated length, but this often requires a very large list that is
    # slow to assign elements in, so we stick to matching on a growing list
    # here.
    # It would also require the partkey to be representable as an integer,
    # rather than a double, which introduces a tighter constraint on the maximum
    # number of columns lookup can have (nonfixed attrs instead of just LHS
    # attrs).
    # "Partition node" in this UI refers to an ID within the partition cache,
    # not to the nodes used in find_LHSs and powersets.
    key = function(set) to_partition_node(set),
    component_keys = function(set) component_keys(set),
    hash = function(key) key,
    unkey = function(key) unkey(key),
    key_size = function(key) length(unkey(key)),
    decompose_key = function(key) component_keys(unkey(key)),
    key_children = function(key) {
      lapply(component_keys(unkey(key)), subkey_difference, key = key)
    },
    invert_key = function(key) bitwXor(key, full_key),
    key_union = function(key1, key2) bitwOr(key1, key2),
    distinct_key_union = function(key1, key2) key1 + key2,
    key_difference = function(key1, key2) bitwAnd(key1, bitwNot(key2)),
    subkey_difference = function(key, subkey) subkey_difference(key, subkey),
    lookup_hash = function(hash, partitions) match(hash, partitions$key),
    get_with_index = function(index, partitions) partitions$value[[index]],
    calculate_partition = function(key) stop("not implemented"),
    add_partition = function(hash, partition, partitions) {
      partitions$key <- c(partitions$key, hash)
      partitions$value <- c(partitions$value, list(partition))
      partitions
    }
  )
}

unbitset <- function(key) which(rawToBits(key) == 1)

fetch_partition_stripped <- function(
  key,
  lookup,
  partitions,
  partitions_ui
) {
  key_elements <- partitions_ui$decompose_key(key)
  element_hashes <- vapply(key_elements, partitions_ui$hash, character(1))
  hash <- partitions_ui$hash(key)
  partition_index <- partitions_ui$lookup_hash(hash, partitions)
  if (!is.na(partition_index)) {
    sp <- partitions_ui$get_with_index(partition_index, partitions)
    return(list(sp, partitions))
  }
  child_keys <- partitions_ui$key_children(key)
  child_hashes <- vapply(child_keys, partitions_ui$hash, character(1))
  child_indices <- vapply(
    child_hashes,
    partitions_ui$lookup_hash,
    integer(1L),
    partitions
  )
  if (sum(!is.na(child_indices)) >= 2) {
    chosen_indices <- which(!is.na(child_indices))[1:2]
    sp <- stripped_partition_product(
      partitions_ui$get_with_index(child_indices[[chosen_indices[[1]]]], partitions),
      partitions_ui$get_with_index(child_indices[[chosen_indices[[2]]]], partitions),
      nrow(lookup)
    )
  }else{
    if (sum(!is.na(child_indices)) == 1 && partitions_ui$key_size(key) > 1) {
      existing_child_position <- which(!is.na(child_indices))
      remainder_element <- key_elements[[existing_child_position]]
      remainder_hash <- element_hashes[[existing_child_position]]
      existing_child_index <- child_indices[[existing_child_position]]
      existing_child_sp <- partitions_ui$get_with_index(
        existing_child_index,
        partitions
      )
      remainder_result <- fetch_partition_stripped(
        remainder_element,
        lookup,
        partitions,
        partitions_ui
      )
      remainder_sp <- remainder_result[[1]]
      partitions <- remainder_result[[2]]
      sp <- stripped_partition_product(
        existing_child_sp,
        remainder_sp,
        nrow(lookup)
      )
    }else{
      sp <- partitions_ui$calculate_partition(key)
    }
  }
  partitions <- partitions_ui$add_partition(hash, sp, partitions)
  list(sp, partitions)
}

to_partition_node_char <- function(attrs_bitset) {
  paste(attrs_bitset, collapse = "")
}

individual_bitsets <- function(attrs_bitset) {
  bitlen <- 8L*length(attrs_bitset)
  bits <- unbitset(attrs_bitset)
  lapply(bits, bitset, bitlen)
}

bitset <- function(attr_indices, bitlen) {
  bools <- rep(FALSE, bitlen)
  bools[attr_indices] <- TRUE
  packBits(bools)
}

fsplit_rows_emptyable <- function(lookup, attr_indices) {
  if (length(attr_indices) == 0)
    return(list(seq_len(nrow(lookup))))
  fsplit_rows(lookup, attr_indices)
}

fetch_refined_partition <- function(
  lookup,
  rhs_set,
  lhs_key,
  partition_cache,
  fetch_partition
) {
  res1 <- fetch_partition(lhs_key, lookup, partition_cache)
  lhs_partition <- res1[[1]]
  partition_cache <- res1[[2]]
  individual_rhs_lookup_indices <- lapply(rhs_set, \(r) lookup[[r]])
  rhs_lookup_indices <- if (length(rhs_set) == 1)
    individual_rhs_lookup_indices[[1]]
  else {
    do.call(paste, unname(lookup[rhs_set])) |>
      (\(x) match(x, x))()
  }
  relevant_lhs_partition <- filter_partition(lhs_partition, rhs_lookup_indices)
  if (partition_rank(relevant_lhs_partition) == 0)
    return(list(rep(list(list()), length(rhs_set)), list(), partition_cache))
  list(
    lapply(
      individual_rhs_lookup_indices,
      refine_partition_by_lookup,
      relevant_partition = relevant_lhs_partition
    ),
    relevant_lhs_partition,
    partition_cache
  )
}

exact_dependencies <- function(
  lookup,
  rhs,
  lhs_set,
  partitions,
  fetch_rank
) {
  lhs_result <- fetch_rank(lhs_set, lookup, partitions)
  lhs_rank <- lhs_result[[1]]
  partitions <- lhs_result[[2]]
  if (lhs_rank == 0)
    return(list(TRUE, partitions))
  union_result <- fetch_rank(union(lhs_set, rhs), lookup, partitions)
  union_rank <- union_result[[1]]
  partitions <- union_result[[2]]
  list(union_rank == lhs_rank, partitions)
}

approximate_dependencies <- function(
  lookup,
  rhs,
  lhs_set,
  partitions,
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
  lhs_result <- fetch_rank(lhs_set, lookup, partitions)
  lhs_rank <- lhs_result[[1]]
  partitions <- lhs_result[[2]]
  if (lhs_rank <= limit)
    return(list(TRUE, partitions))
  union_result <- fetch_rank(union(lhs_set, rhs), lookup, partitions)
  union_rank <- union_result[[1]]
  partitions <- union_result[[2]]
  if (lhs_rank - union_rank > limit)
    return(list(FALSE, partitions))

  error <- fetch_error(lookup, rhs, lhs_set, partitions)
  list(error <= limit, partitions)
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
  index <- partitions_ui$lookup_hash(hash, partitions)
  if (!is.na(index)) {
    return(list(partitions_ui$get_with_index(index, partitions), partitions))
  }
  lookup_set_only <- lookup[, set, drop = FALSE]
  set_rank <- sum(duplicated(lookup_set_only))
  partitions <- partitions_ui$add_partition(hash, set_rank, partitions)
  list(set_rank, partitions)
}

fetch_rank_full_cache <- function(
  set,
  lookup,
  partitions,
  partitions_ui
) {
  key <- partitions_ui$key(set)
  key_elements <- as.integer(unlist(partitions_ui$component_keys(set)))
  hash <- partitions_ui$hash(key)
  index <- partitions_ui$lookup_hash(hash, partitions)
  if (!is.na(index)) {
    sp <- partitions_ui$get_with_index(index, partitions)
    return(list(partition_rank(sp), partitions))
  }
  child_keys <- lapply(key_elements, partitions_ui$subkey_difference, key = key)
  child_hashes <- lapply(child_keys, partitions_ui$hash)
  child_indices <- vapply(
    child_hashes,
    partitions_ui$lookup_hash,
    integer(1L),
    partitions
  )
  if (sum(!is.na(child_indices)) >= 2) {
    chosen_indices <- which(!is.na(child_indices))[1:2]
    sp <- stripped_partition_product(
      partitions_ui$get_with_index(child_indices[chosen_indices[[1]]], partitions),
      partitions_ui$get_with_index(child_indices[chosen_indices[[2]]], partitions),
      nrow(lookup)
    )
  }else{
    if (sum(!is.na(child_indices)) == 1) {
      existing_child_position <- which(!is.na(child_indices))
      remainder_element <- set[[existing_child_position]]
      remainder_key <- key_elements[[existing_child_position]]
      existing_child_sp <- partitions_ui$get_with_index(
        child_indices[existing_child_position],
        partitions
      )
      remainder_result <- fetch_rank_full_cache(
        remainder_element,
        lookup,
        partitions,
        partitions_ui
      )
      remainder_rank <- remainder_result[[1]]
      partitions <- remainder_result[[2]]
      remainder_sp <- partitions_ui$get_with_index(
        partitions_ui$lookup_hash(remainder_key, partitions),
        partitions
      )
      stopifnot(identical(remainder_rank, partition_rank(remainder_sp)))
      sp <- stripped_partition_product(
        existing_child_sp,
        remainder_sp,
        nrow(lookup)
      )
    }else{
      sp <- fsplit_rows(lookup, set)
      sp <- unname(sp[lengths(sp) > 1])
    }
  }
  partitions <- partitions_ui$add_partition(hash, sp, partitions)
  list(partition_rank(sp), partitions)
}

fetch_error_withcache <- function(
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

fetch_error_nocache <- function(
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

to_partition_node <- function(element_indices) {
  as.integer(sum(2^(element_indices - 1L)))
}

to_partition_nodes <- function(element_indices) {
  as.integer(2^(element_indices - 1L))
}

fsplit_rows <- function(lookup, attr_indices) {
  fsplit(seq_len(nrow(lookup)), lookup[, attr_indices, drop = FALSE])
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
