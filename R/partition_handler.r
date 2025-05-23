bitset_partition_handler <- function(df) {
  # Gives a list of functions that handle lookup and calculation of stripped
  # partitions, encapsulating dependence on the partitions cache and the
  # original lookup table df. The intention is for all use of df to be done
  # through the "interface" provided by these functions, so the main code can
  # focus on implementing the search.

  # The partitions UI encapsulates the partition cache.
  # General partitions UI elements:
  # Set: original values, that subset the original lookup table df
  # Key: transformed set; what is usually passed around
  # Hash: key transformed into a string for lookup (lookup uses hash, not key)
  # functions:
  # key: Set -> Key
  # hash: Key -> Hash
  # unkey: Key -> Set
  # key_size: Key -> Int
  # decompose_key: Key -> [Key] (one key for each atomic element)
  # key_children: Key -> [Key] (remove one element from the key each time)
  # invert_key: Key -> Key (bitwise negation on the used bits only)
  # lookup_hash: Hash -> Partitions -> Option<Index>
  # get_with_index: Index -> Partitions -> StrippedPartition
  # calculate_partition: Key -> StrippedPartition (calculate from df)
  # add_partition: Hash -> Partition -> Partitions -> Partitions

  # Bitset partitions UI types:
  # Set: a no-duplicate integer vector, for which columns of df to use
  # Key: a bitset
  # Hash: the bitset, with its bytes pasted together to make a string
  bitlen <- 8*ceiling(ncol(df)/8)
  unkey <- function(key) which(rawToBits(key) == 1)
  bitset_partitions_ui <- list(
    key = function(set) bitset(set, bitlen),
    hash = function(key) to_partition_node_char(key),
    unkey = function(key) unkey(key),
    key_size = function(key) length(unkey(key)),
    decompose_key = function(key) individual_bitsets(key),
    key_children = function(key) lapply(individual_bitsets(key), xor, key),
    invert_key = function(key) {
      df_mask <- packBits(c(rep(TRUE, ncol(df)), rep(FALSE, bitlen - ncol(df))))
      !key & df_mask
    },
    lookup_hash = function(hash, partitions) match(hash, partitions$key),
    get_with_index = function(index, partitions) partitions$value[[index]],
    calculate_partition = function(key) {
      attr_indices <- unkey(key)
      sp <- fsplit_rows_emptyable(df, attr_indices)
      unname(sp[lengths(sp) > 1])
    },
    add_partition = function(hash, val, partitions) {
      partitions$key <- c(partitions$key, hash)
      partitions$value <- c(partitions$value, list(val))
      partitions
    }
  )
  fetch_partition <- function(attrs_bitset, df, partitions) {
    fetch_partition_stripped(attrs_bitset, df, partitions, bitset_partitions_ui)
  }
  list(
    key = bitset_partitions_ui$key,
    key_size = bitset_partitions_ui$key_size,
    decompose_key = bitset_partitions_ui$decompose_key,
    refine = function(rhs_bitset, lhs_bitset, partitions) {
      fetch_refined_partition(
        df,
        bitset_partitions_ui$unkey(rhs_bitset),
        lhs_bitset,
        partitions,
        fetch_partition
      )
    }
  )
}

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
  bits <- which(rawToBits(attrs_bitset) == 1)
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

  # The partitions UI encapsulates the partition cache.
  # General partitions UI elements:
  # Set: original values, that subset the original lookup table df
  # Key: transformed set; what is usually passed around
  # Hash: key transformed into a string for lookup (lookup uses hash, not key)
  # functions:
  # key: Set -> Key
  # hash: Key -> Hash
  # unkey: Key -> Set
  # key_size: Key -> Int
  # decompose_key: Key -> [Key] (one key for each atomic element)
  # key_children: Key -> [Key] (remove one element from the key each time)
  # invert_key: Key -> Key (bitwise negation on the used bits only)
  # lookup_hash: Hash -> Partitions -> Option<Index>
  # get_with_index: Index -> Partitions -> StrippedPartition
  # calculate_partition: Key -> StrippedPartition (calculate from df)
  # add_partition: Hash -> Partition -> Partitions -> Partitions

  # Integer partitions UI types:
  # Set: a no-duplicate integer vector, for which columns of df to use
  # Key: ???
  # Hash: an integer, for direct subsetting.

  # The integer partitions UI takes some additional arguments on top of df,
  # since its use in DFD has some further requirements.
  # accuracy is a threshold for FD correctness.
  # cache is a logical indicating whether partitions are cached at all.
  # As with df, the intention is all use of these arguments to be done through
  # the resulting interface.

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
