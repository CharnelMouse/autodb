bitset_partition_handler <- function(df) {
  # Gives a list of functions that handle lookup and calculation of stripped
  # partitions, encapsulating dependence on the partitions cache and the
  # original lookup table df. The intention is for all use of df to be done
  # through the "interface" provided by these functions, so the main code can
  # focus on implementing the search.

  # The partitions UI encapsulates the partition cache.
  partitions_ui <- partitions_ui(df, key_class = "bitset")
  fetch_partition <- function(attrs_bitset, df, partitions) {
    fetch_partition_stripped(attrs_bitset, df, partitions, partitions_ui)
  }
  list(
    key = partitions_ui$key,
    key_size = partitions_ui$key_size,
    decompose_key = partitions_ui$decompose_key,
    refine = function(rhs_bitset, lhs_bitset, partitions) {
      fetch_refined_partition(
        df,
        partitions_ui$unkey(rhs_bitset),
        lhs_bitset,
        partitions,
        fetch_partition
      )
    }
  )
}

unbitset <- function(key) which(rawToBits(key) == 1)

fetch_partition_stripped <- function(
  key,
  df,
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
      nrow(df)
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
        df,
        partitions,
        partitions_ui
      )
      remainder_sp <- remainder_result[[1]]
      partitions <- remainder_result[[2]]
      sp <- stripped_partition_product(
        existing_child_sp,
        remainder_sp,
        nrow(df)
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

fsplit_rows_emptyable <- function(df, attr_indices) {
  if (length(attr_indices) == 0)
    return(list(seq_len(nrow(df))))
  fsplit_rows(df, attr_indices)
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

integer_partition_handler <- function(df, accuracy, cache) {
  # Gives a list of functions that handle lookup and calculation of stripped
  # partitions, encapsulating dependence on the partitions cache and the
  # original lookup table df. The intention is for all use of df to be done
  # through the "interface" provided by these functions, so the main code can
  # focus on implementing the search.

  # The integer partitions handler takes some additional arguments on top of df,
  # since its use in DFD has some further requirements.
  # accuracy is a threshold for FD correctness.
  # cache is a logical indicating whether partitions are cached at all.
  # As with df, the intention is all use of these arguments to be done through
  # the resulting interface.

  # The partitions UI encapsulates the partition cache.
  partitions_ui <- partitions_ui(df, key_class = "integer")

  check_FD_partition <- if (cache)
    function(attr_indices, df, partitions)
      check_FD_partition_stripped(attr_indices, df, partitions, partitions_ui)
  else
    function(attr_indices, df, partitions)
      check_FD_partition_nclass(attr_indices, df, partitions, partitions_ui)

  threshold <- ceiling(nrow(df)*accuracy)
  limit <- nrow(df) - threshold
  if (limit > 0L) {
    check_AD <- if (cache)
      function(df, rhs, lhs_set, partitions, limit)
        check_AD_cache(df, rhs, lhs_set, partitions, limit, partitions_ui)
    else
      function(df, rhs, lhs_set, partitions, limit)
        check_AD_nocache(df, rhs, lhs_set, partitions, limit, partitions_ui)
    function(rhs, lhs_set, partitions) {
      approximate_dependencies(
        df,
        rhs,
        lhs_set,
        partitions,
        limit,
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

partitions_ui <- function(df, key_class = c("bitset", "integer")) {
  # The partitions UI encapsulates the partition cache.

  # General partitions UI elements:
  # Set: original values, that subset the original lookup table df
  # Key: transformed set; what is usually passed around
  # Hash: key transformed into a format for subsetting (lookup uses hash, not key)

  # functions:
  # key: Set -> Key
  # component_keys: Set -> [Key] (equivalent to key >> decompose_key)
  # hash: Key -> Hash
  # unkey: Key -> Set
  # key_size: Key -> Int
  # decompose_key: Key -> [Key] (per atomic element; unkey >> component_keys)
  # key_children: Key -> [Key] (remove one element from the key each time)
  # invert_key: Key -> Key (bitwise negation on the used bits only)
  # lookup_hash: Hash -> Partitions -> Option<Index>
  # get_with_index: Index -> Partitions -> StrippedPartition
  # calculate_partition: Key -> StrippedPartition (calculate from df)
  # add_partition: Hash -> Partition -> Partitions -> Partitions
  key_class <- match.arg(key_class)
  switch(
    key_class,
    bitset = bitset_partitions_ui(df),
    integer = integer_partitions_ui(df)
  )
}

bitset_partitions_ui <- function(df) {
  # Bitset partitions UI types:
  # Set: a no-duplicate integer vector, for which columns of df to use
  # Key: a bitset
  # Hash: the bitset, with its bytes pasted together to make a string
  bitlen <- 8*ceiling(ncol(df)/8)
  list(
    key = function(set) bitset(set, bitlen),
    component_keys = function(set) lapply(set, bitset, bitlen),
    hash = function(key) to_partition_node_char(key),
    unkey = function(key) unbitset(key),
    key_size = function(key) length(unbitset(key)),
    decompose_key = function(key) individual_bitsets(key),
    key_children = function(key) lapply(individual_bitsets(key), xor, key),
    invert_key = function(key) {
      df_mask <- packBits(c(rep(TRUE, ncol(df)), rep(FALSE, bitlen - ncol(df))))
      !key & df_mask
    },
    lookup_hash = function(hash, partitions) match(hash, partitions$key),
    get_with_index = function(index, partitions) partitions$value[[index]],
    calculate_partition = function(key) {
      attr_indices <- unbitset(key)
      sp <- fsplit_rows_emptyable(df, attr_indices)
      unname(sp[lengths(sp) > 1])
    },
    add_partition = function(hash, partition, partitions) {
      partitions$key <- c(partitions$key, hash)
      partitions$value <- c(partitions$value, list(partition))
      partitions
    }
  )
}

integer_partitions_ui <- function(df) {
  # Integer partitions UI types:
  # Set: a no-duplicate integer vector, for which columns of df to use
  # Key: ???
  # Hash: an integer, for direct subsetting.
  full_key <- to_partition_node(seq_len(ncol(df)))
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
    # number of columns df can have (nonfixed attrs instead of just LHS attrs).
    # "Partition nodes" in this UI refer to IDs within the partition: these are
    # different to those used in find_LHSs and powersets.
    key = function(set) to_partition_node(set),
    component_keys = function(set) component_keys(set),
    hash = function(key) key,
    unkey = function(key) unkey(key),
    key_size = function(key) length(unkey(key)),
    decompose_key = function(key) component_keys(unkey(key)),
    invert_key = function(key) bitwXor(key, full_key),
    key_union = function(key1, key2) bitwOr(key1, key2),
    distinct_key_union = function(key1, key2) key1 + key2,
    key_difference = function(key1, key2) bitwAnd(key1, bitwNot(key2)),
    subkey_difference = function(key, subkey) subkey_difference(key, subkey),
    key_children = function(key) {
      lapply(component_keys(unkey(key)), subkey_difference, key = key)
    },
    lookup_hash = function(hash, partitions) match(hash, partitions$key),
    get_with_index = function(index, partitions) partitions$value[[index]],
    add_partition = function(hash, partition, partitions) {
      partitions$key <- c(partitions$key, hash)
      partitions$value <- c(partitions$value, list(partition))
      partitions
    }
  )
}

exact_dependencies <- function(
  df,
  rhs,
  lhs_set,
  partitions,
  check_FD_partition
) {
  lhs_result <- check_FD_partition(lhs_set, df, partitions)
  lhs_rank <- lhs_result[[1]]
  partitions <- lhs_result[[2]]
  if (lhs_rank == 0)
    return(list(TRUE, partitions))
  union_result <- check_FD_partition(union(lhs_set, rhs), df, partitions)
  union_rank <- union_result[[1]]
  partitions <- union_result[[2]]
  list(union_rank == lhs_rank, partitions)
}

approximate_dependencies <- function(
  df,
  rhs,
  lhs_set,
  partitions,
  limit,
  check_FD_partition,
  check_AD
) {
  # cheaper bounds checks:
  # lhs_rank - union_rank <= error <= lhs_rank is always true,
  # (paper version: e(X) - e(X /\ Y) <= e(X -> Y) <= e(X))
  # and LHS -> RHS is approximately true if error <= limit,
  # so if lhs_rank <= limit or limit < lhs_rank - union_rank
  # then we can skip the calculation of error.
  lhs_result <- check_FD_partition(lhs_set, df, partitions)
  lhs_rank <- lhs_result[[1]]
  partitions <- lhs_result[[2]]
  if (lhs_rank <= limit)
    return(list(TRUE, partitions))
  union_result <- check_FD_partition(union(lhs_set, rhs), df, partitions)
  union_rank <- union_result[[1]]
  partitions <- union_result[[2]]
  if (lhs_rank - union_rank > limit)
    return(list(FALSE, partitions))

  check_AD(df, rhs, lhs_set, partitions, limit)
}

check_FD_partition_nclass <- function(
  set,
  df,
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
  df_set_only <- df[, set, drop = FALSE]
  set_rank <- sum(duplicated(df_set_only))
  partitions <- partitions_ui$add_partition(hash, set_rank, partitions)
  list(set_rank, partitions)
}

check_FD_partition_stripped <- function(
  set,
  df,
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
      nrow(df)
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
      remainder_result <- check_FD_partition_stripped(
        remainder_element,
        df,
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
        nrow(df)
      )
    }else{
      sp <- fsplit_rows(df, set)
      sp <- unname(sp[lengths(sp) > 1])
    }
  }
  partitions <- partitions_ui$add_partition(hash, sp, partitions)
  list(partition_rank(sp), partitions)
}

check_AD_cache <- function(
  df,
  rhs_set,
  lhs_set,
  partitions,
  limit,
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
  error <- stripped_partition_error(lhs_sp, union_sp, nrow(df))
  list(error <= limit, partitions)
}

check_AD_nocache <- function(
  df,
  rhs_set,
  lhs_set,
  partitions,
  limit,
  partitions_ui
) {
  # This is a quick working version I put together to replace the non-working
  # original. The quicker version from Tane requires cache = TRUE for stripped
  # partition information.
  splitted <- df[[rhs_set]]
  splitter <- df[, lhs_set, drop = FALSE]
  rhs_value_lhs_partition <- fsplit(splitted, splitter)
  error <- value_partition_error(rhs_value_lhs_partition, nrow(df))
  list(error <= limit, partitions)
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
