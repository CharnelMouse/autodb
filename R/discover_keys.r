discover_keys <- function(
  df,
  keep_rownames = FALSE,
  digits = getOption("digits"),
  exclude = character(),
  exclude_class = character(),
  dependants = names(df),
  detset_limit = ncol(df),
  progress = FALSE,
  progress_file = ""
) {
  report <- reporter(progress, progress_file, new = TRUE)

  if (!isFALSE(keep_rownames)) {
    nm <- if (isTRUE(keep_rownames)) "row" else keep_rownames[[1]]
    df <- cbind(stats::setNames(data.frame(rownames(df)), nm), df)
  }

  n_cols <- ncol(df)
  if (n_cols == 0 || nrow(df) <= 1)
    return(list(character()))
  attr_names <- colnames(df)
  duplicates <- which(duplicated(attr_names))
  if (length(duplicates) > 0) {
    dup_names <- unique(attr_names[duplicates])
    sorted_dup_names <- dup_names[order(match(dup_names, attr_names))]
    stop("duplicate column names: ", toString(sorted_dup_names))
  }
  if (any(!is.element(exclude, attr_names)))
    warning("there are attribute names in exclude not present in df")
  if (any(!is.element(dependants, attr_names)))
    warning("there are attribute names in dependants not present in df")
  dependants <- intersect(attr_names, dependants)
  dependants <- match(dependants, attr_names)

  valid_determinant_name <- !is.element(attr_names, exclude)
  valid_determinant_class <- !vapply(
    df,
    inherits,
    logical(1),
    exclude_class
  )
  valid_determinant_attrs_prefixing <- which(
    valid_determinant_name & valid_determinant_class
  )

  # convert all columns to integers, since they're checked for duplicates more
  # quickly when calculating partitions
  # we must round floating-point/complex columns, since they're otherwise
  # infeasible:
  # - all.equal, i.e. equality by tolerance, isn't transient, so isn't an
  #   equivalence relation; we need such a relation for consistent partitioning
  # - ==, identical, etc., i.e. equality by bit comparison, results in different
  #   results on different machines, e.g. x86 and ARM(?) 64-bit both represent
  #   floats in 64-bit, but x86 represents in 80 bits first and then rounds,
  #   so non-representable numbers get approximated differently, resulting in
  #   different partition results
  if (!is.na(digits)) {
    report(paste(
      "formatting numerical/complex variables with",
      digits,
      "significant digits"
    ))
    df[] <- lapply(df, format_if_float, digits = digits)
  }
  report("simplifying data types")
  df <- lookup_table(df)

  MMCS(
    df,
    determinants = valid_determinant_attrs_prefixing,
    dependants = dependants,
    detset_limit = detset_limit,
    report = report
  )
}

MMCS <- function(
  lookup,
  determinants = seq_along(lookup),
  dependants = seq_along(lookup),
  detset_limit = ncol(lookup),
  report = reporter(report = FALSE, con = "", new = TRUE)
) {
  if (ncol(lookup) == 0)
    return(list(character()))
  report("calculating single-attribute PLIs")
  plis <- lapply(lookup, pli)
  report("sampling difference sets")
  D <- lapply(plis, sample_diffsets, lookup) |>
    unlist(recursive = FALSE) |>
    unique()
  report(with_number(length(D), "initial diffset", "\n", "s\n"))
  MMCS_main(lookup, determinants, dependants, detset_limit, D, report)
}

MMCS_main <- function(
  lookup,
  determinants,
  dependants,
  detset_limit,
  D,
  report
) {
  attrs <- names(lookup)
  res <- list()
  visited <- character()
  partition_handler <- refineable_partition_handler(lookup, key_class = "bitset")

  V_bitset <- partition_handler$key(determinants)
  D <- lapply(D, partition_handler$key)
  partition_handler$add_diffset_keys(D)
  empty <- partition_handler$key(integer())
  return_stack <- list(list(
    S = empty,
    V = V_bitset,
    depth = 1L,
    oldS = empty,
    addS = empty
  ))

  while (length(return_stack) > 0) {
    node <- return_stack[[1]]
    return_stack <- return_stack[-1]
    node_string <- paste0(
      paste(node$S, collapse = ""),
      paste(node$V, collapse = "")
    )
    if (is.element(node_string, visited))
      stop("node ", node_string, " already visited")
    depth <- node$depth
    partition_handler$truncate(depth)
    partition_handler$prepare_growS(
      node$oldS,
      partition_handler$full_key,
      node$addS,
      partition_handler$empty_key
    )
    attr_res <- MMCS_visit(
      node$S,
      node$V,
      node$depth,
      node_string,
      lookup,
      report = report,
      partition_handler
    )
    visited <- c(visited, node_string)
    res <- c(res, attr_res[[1]])
    new_nodes <- attr_res[[2]]
    return_stack <- c(
      new_nodes[vapply(
        new_nodes,
        \(node) partition_handler$key_size(node$S) <= detset_limit,
        logical(1)
      )],
      return_stack
    )
  }
  report(paste0(
    "MMCS complete",
    "\n",
    with_number(length(partition_handler$get_diffset_keys()), "final diffset", "", "s"),
    "\n",
    with_number(length(visited), "node", " visited", "s visited"),
    "\n",
    with_number(partition_handler$cache_size(), "partition", " cached", "s cached")
  ))
  lapply(res, \(x) attrs[as.logical(rawToBits(x))])
}

MMCS_visit <- function(
  S_bitset,
  V_bitset,
  depth,
  node_string,
  lookup,
  report,
  partition_handler
) {
  W_bitset <- partition_handler$invert_key(S_bitset)
  # pruning
  everywhere_common <- lapply(
    partition_handler$decompose_key(S_bitset),
    \(C) {
      commons <- lapply(
        partition_handler$decompose_key(partition_handler$full_key),
        \(A) {
          crit <- partition_handler$fetch_critical_diffsets(C, A, S_bitset)
          # Bs that would make C redundant WRT A
          Reduce(`&`, crit, init = partition_handler$full_key)
        }
      )
      # Bs that would make C redundant WRT all A
      Reduce(`&`, commons, init = partition_handler$full_key)
    }
  )
  # Bs that make some C redundant WRT all A
  always_common <- Reduce(`|`, everywhere_common, partition_handler$empty_key)
  V_bitset <- partition_handler$subkey_difference(V_bitset, always_common)
  # validation at the leaves
  uncovered <- partition_handler$fetch_uncovered_keys(S_bitset, W_bitset)
  if (length(uncovered) == 0) {
    refinement <- partition_handler$refine(W_bitset, S_bitset)
    refined_partitions <- refinement[[1]]
    relevant_Spli <- refinement[[2]]
    if (validate(refined_partitions, relevant_Spli))
      return(list(list(S_bitset), list()))
    stopifnot(length(relevant_Spli) > 0)
    ds <- new_diffset(relevant_Spli, refined_partitions, lookup)
    dsl <- list(ds)
    ds2 <- sample_diffsets(relevant_Spli, lookup)
    added <- setdiff(
      lapply(c(dsl, ds2), partition_handler$key),
      partition_handler$get_diffset_keys()
    )
    stopifnot(length(added) > 0)
    partition_handler$add_diffset_keys(added)
    uncovered <- partition_handler$fetch_uncovered_keys(
      S_bitset,
      partition_handler$full_key
    )
  }
  # branching
  if (length(uncovered) == 0)
    stop("edge selection impossible at ", node_string)
  E_bitset <- uncovered[[sample_minheur_MMCS(uncovered, V_bitset)]]
  Bs_bitsets <- partition_handler$decompose_key(E_bitset & V_bitset)
  res <- list()
  # rev() differs from the description in the paper, but the authors gave it as
  # a fix in private correspondence; I'll add a reference when they've published
  # the new work
  new_nodes <- lapply(
    rev(seq_along(Bs_bitsets)),
    \(n) {
      b <- Bs_bitsets[[n]]
      rem <- Reduce(`|`, Bs_bitsets[seq_len(n)])
      list(
        S = S_bitset | b,
        V = V_bitset & !rem,
        depth = depth + 1L,
        oldS = S_bitset,
        addS = b
      )
    }
  )
  list(res, new_nodes)
}

# Edge choice is as for FDHitsJoint, but we can ignore the |E \ W| term:
# W := ¬S, so E \ W = E /\ S, which is empty.
# We therefore have the same heuristic as for FDHitsSep.
sample_minheur_MMCS <- sample_minheur_sep
