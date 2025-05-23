bitset_partition_handler <- function(df) {
  # handles lookup and calculation of stripped partitions, without calling code
  # having to worry about handling the store, or the lookup table df itself.
  # the partitions_ui handles the former.
  # general partitions UI elements:
  # set: original values, that subset the original lookup table df
  # key: transformed set; what is usually passed around
  # hash: key transformed into a string for lookup (lookup uses hash, not key)
  # lookup_hash: hash -> partitions -> option<index>
  # bitset partitions UI elements:
  # set: a no-duplicate integer vector, for which columns of df to use
  # key: a bitset
  # hash: the bitset, with its bytes pasted together to make a string
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
