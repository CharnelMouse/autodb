#' Dependency discovery with DFD
#'
#' The DFD algorithm finds all the minimal functional dependencies represented
#' in a data frame.
#'
#' Column names for \code{\link{df}} must be unique.
#'
#' This implementation differs a little from the algorithm presented in the
#' original paper:
#' \itemize{
#'   \item Some attributes, or attribute types, can be designated, ahead of
#'   time, as not being candidate members for determinant sets. This reduces the
#'   number of candidate determinant sets to be searched, saving time by not
#'   searching for determinant sets that the user would remove later anyway.
#'   \item Attributes that have a single unique value, i.e. are
#'   constant, get attributed a single empty determinant set. In the standard
#'   DFD algorithm, they would be assigned all the other non-excluded attributes
#'   as length-one determinant sets. Assigning them the empty set distinguishes
#'   them as constant, allowing for special treatment at normalisation and later
#'   steps.
#'   \item As was done in the original Python library, there is an extra case in
#'   seed generation for when there are no discovered maximal non-dependencies.
#'   In this case, we take all of the single-attribute nodes, then filter out by
#'   minimal dependencies as usual. This is equivalent to taking the empty set
#'   as the single maximal non-dependency.
#'   \item There are three results when checking whether a candidate node is
#'   minimal/maximal. TRUE indicates the node is minimal/maximal, as usual.
#'   FALSE has been split into FALSE and NA. NA indicates that we can not yet
#'   determine whether the node is minimal/maximal. FALSE indicates that we have
#'   determined that it is not minimal/maximal, and we can set its category as
#'   such. This is done by checking whether any of its adjacent
#'   subsets/supersets are dependencies/non-dependencies, instead of waiting to
#'   exhaust the adjacent subsets/supersets to visit when picking the next node
#'   to visit.
#'   \item We do not yet keep hashmaps to manage subset/superset relationships,
#'   as described in Section 3.5 of the original paper.
#'   \item Missing values (NA) are treated as a normal value, with NA = NA being
#'   true, and x = NA being false for any non-NA value of x.
#' }
#' @param df a data.frame, the relation to evaluate.
#' @param accuracy a numeric in (0, 1]: the accuracy threshold required in order
#'   to conclude a dependency.
#' @param cache a logical, indicating whether to store information about how
#'   sets of attributes group the relation records (stripped partitions). This is
#'   expected to let the algorithm run more quickly, but might be inefficient
#'   for small data frames or small amounts of memory.
#' @param exclude a character vector, containing names of attributes to not
#'   consider as members of determinant sets. If names are given that aren't
#'   present in \code{df}, the user is given a warning.
#' @param exclude_class a character vector, indicating classes of attributes to
#'   not consider as members of determinant_sets. Attributes are excluded if
#'   they inherit from any given class.
#' @inheritParams autodb
#'
#' @return A named list with two elements. \code{dependencies} is a named list,
#'   where the names give the dependent attribute, and each element is a list of
#'   character vectors. Each character vector is a set of determinant attributes
#'   for that dependent attribute. \code{attrs} is the attribute names of
#'   \code{df}, in order. This is kept to serve as a default priority order for
#'   the attributes during normalisation.
#' @references
#' Abedjan Z., Schulze P., Naumann F. (2014) DFD: efficient functional
#' dependency discovery. *Proceedings of the 23rd ACM International Conference
#' on Conference on Information and Knowledge Management (CIKM '14). New York,
#' U.S.A.*, 949???958.
#' @export
dfd <- function(
  df,
  accuracy,
  cache = TRUE,
  exclude = character(),
  exclude_class = character(),
  progress = FALSE,
  progress_file = ""
) {
  report <- reporter(progress, progress_file, new = TRUE)

  n_cols <- ncol(df)
  column_names <- colnames(df)
  duplicates <- which(duplicated(column_names))
  if (length(duplicates) > 0) {
    dup_names <- unique(column_names[duplicates])
    sorted_dup_names <- dup_names[order(match(dup_names, column_names))]
    stop("duplicate column names: ", toString(sorted_dup_names))
  }
  if (n_cols == 0)
    return(list(
      dependencies = stats::setNames(list(), character()),
      attrs = character()
    ))
  if (any(!is.element(exclude, column_names)))
    warning("there are attribute names in exclude not present in df")
  valid_determinant_name <- !is.element(column_names, exclude)
  valid_determinant_class <- !vapply(
    df,
    inherits,
    logical(1),
    exclude_class
  )
  valid_determinant <- valid_determinant_name & valid_determinant_class
  valid_determinant_attrs_prefixing <- column_names[valid_determinant]
  # convert all columns to integers, since they're checked for duplicates more
  # quickly when calculating partitions
  df <- report$exp(
    data.frame(lapply(df, \(x) as.integer(factor(x, exclude = NULL)))) |>
      stats::setNames(column_names),
    "simplifying data types"
  )
  partitions <- list()
  dependencies <- stats::setNames(rep(list(list()), ncol(df)), column_names)
  fixed <- character()
  nonfixed <- column_names
  for (i in seq_along(column_names)) {
    attr <- column_names[i]
    if (all(is.na(df[[attr]])) || all(df[[attr]] == df[[attr]][1])) {
      fixed <- report$op(fixed, c, paste(attr, "is fixed"), attr)
      nonfixed <- setdiff(nonfixed, attr)
      dependencies[[attr]] <- list(character())
    }
  }
  if (progress && any(!valid_determinant)) {
    cat(
      paste(
        "attributes not considered as determinants:",
        toString(column_names[!valid_determinant]),
        "\n"
      ),
      file = progress_file,
      append = TRUE
    )
  }
  # For nonfixed attributes, all can be dependents, but
  # might not all be valid determinants.
  # Maximum size of determinant set for a dependent is number
  # of other valid determinants.
  # If there are dependents that aren't valid determinants,
  # this is number of valid determinant attributes. If there
  # aren't, subtract one.
  valid_determinant_attrs <- intersect(nonfixed, valid_determinant_attrs_prefixing)
  n_dependent_only <- length(nonfixed) - length(valid_determinant_attrs)
  max_n_lhs_attrs <- length(valid_determinant_attrs) -
    as.integer(n_dependent_only == 0)
  # using 0 would allow for one more column, but that's for a later date
  lhs_attrs_limit <- floor(log(.Machine$integer.max, 2))
  if (max_n_lhs_attrs > lhs_attrs_limit)
    stop(paste(
      "only data.frames with up to", lhs_attrs_limit, "columns currently supported"
    ))
  if (max_n_lhs_attrs > 0) {
    powerset <- report$op(
      max_n_lhs_attrs,
      powerset_nodes,
      "constructing powerset"
    )
    for (rhs in nonfixed) {
      report$stat(paste("dependent", rhs))
      lhs_attrs <- setdiff(valid_determinant_attrs, rhs)
      n_lhs_attrs <- length(lhs_attrs)
      expected_n_lhs_attrs <- max_n_lhs_attrs -
        (n_dependent_only > 0 && is.element(rhs, valid_determinant_attrs))
      stopifnot(n_lhs_attrs == expected_n_lhs_attrs)
      if (n_lhs_attrs > 0) {
        nodes <- reduce_powerset(powerset, n_lhs_attrs)
        simple_nodes <- as.integer(2^(seq.int(n_lhs_attrs) - 1))
        lhss <- report$op(
          rhs,
          find_LHSs,
          "determinants available, starting search",
          lhs_attrs,
          nodes,
          simple_nodes,
          df,
          partitions,
          accuracy,
          cache,
          progress,
          progress_file
        )
        dependencies[[rhs]] <- c(dependencies[[rhs]], lhss)
      }
    }
  }
  report$stat("DFD complete")
  list(dependencies = dependencies, attrs = column_names)
}

find_LHSs <- function(
  rhs,
  lhs_attrs,
  nodes,
  simple_nodes,
  df,
  partitions,
  accuracy,
  cache = FALSE,
  progress = 0L,
  progress_file = ""
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
  # since we don't need to include the dependent.
  # Node categories:
  # -3 = candidate maximal non-dependency
  # -2 = maximal non-dependency
  # -1 = non-maximal non-dependency
  #  0 = unresolved
  #  1 = non-minimal dependency
  #  2 = minimal dependency
  #  3 = candidate minimal dependency
  nodes$bits <- lapply(nodes$bits, as.logical)
  seeds <- simple_nodes
  min_deps <- integer()
  max_non_deps <- integer()
  trace <- integer()

  while (length(seeds) != 0) {
    node <- sample(seeds, 1)
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
          lhs_set <- lhs_attrs[as.logical(intToBits(node))]
          cp <- compute_partitions(df, rhs, lhs_set, partitions, accuracy, cache = cache)
          result <- cp[[1]]
          partitions <- cp[[2]]
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
    new_seeds <- generate_next_seeds(max_non_deps, min_deps, simple_nodes, nodes)
    seeds <- new_seeds
  }
  lapply(min_deps, \(md) lhs_attrs[as.logical(intToBits(md))])
}

powerset_nodes <- function(n) {
  if (n == 0)
    return(list(
      bits = logical(),
      children = list(),
      parents = list(),
      category = integer(),
      visited = logical()
    ))
  if (n == 1)
    return(list(
      bits = list(TRUE),
      children = list(integer()),
      parents = list(integer()),
      category = 0L,
      visited = FALSE
    ))
  n_nonempty_subsets <- 2^n - 1
  node_bits <- lapply(seq.int(n_nonempty_subsets), \(i) as.logical(intToBits(i))[1:n])
  children <- rep(list(integer()), n_nonempty_subsets)
  parents <- rep(list(integer()), n_nonempty_subsets)
  for (x in seq.int(n_nonempty_subsets - 1)) {
    zeroes <- which(!node_bits[[x]][1:n])
    ys <- as.integer(x + 2^(zeroes - 1))
    parents[[x]] <- c(parents[[x]], ys)
    children[ys] <- lapply(children[ys], c, x)
  }
  list(
    bits = node_bits,
    children = children,
    parents = parents,
    category = rep(0L, n_nonempty_subsets),
    visited = rep(FALSE, n_nonempty_subsets)
  )
}

reduce_powerset <- function(powerset, n) {
  n_nodes <- length(powerset$category)
  boundary <- 2^n - 1
  if (n_nodes == boundary)
    return(powerset)
  if (n_nodes < boundary)
    stop("n is larger than size of set")
  trimmed <- lapply(
    powerset,
    `[`,
    seq_len(boundary)
  )
  trimmed$parents <- lapply(trimmed$parents, \(x) x[x <= boundary])
  trimmed$bits <- lapply(trimmed$bits, \(x) x[1:n])
  trimmed
}

is_minimal <- function(node, nodes) {
  children <- nodes$children[[node]]
  if (any(nodes$category[children] > 0))
    return(FALSE)
  if (all(nodes$category[children] < 0))
    return(TRUE)
  NA
}

is_maximal <- function(node, nodes) {
  parents <- nodes$parents[[node]]
  if (any(nodes$category[parents] < 0))
    return(FALSE)
  if (all(nodes$category[parents] > 0))
    return(TRUE)
  NA
}

update_dependency_type <- function(node, nodes, min_deps, max_non_deps) {
  children <- nodes$children[[node]]
  if (any(children %in% min_deps))
    return(1L)
  parents <- nodes$parents[[node]]
  if (any(parents %in% max_non_deps))
    return(-1L)
  nodes$category[node]
}

infer_type <- function(node, nodes) {
  # Attempts to infer the category of self by checking if any subsets are a
  # dependency, or if any supersets are a non-dependency.
  category <- NA
  if (has_dependency_subset(node, nodes))
    category <- 1L
  if (has_nondependency_superset(node, nodes))
    category <- -1L
  category
}

has_dependency_subset <- function(node, nodes) {
  node_bits <- nodes$bits[[node]]
  dependencies <- nodes$category > 0
  dependencies[node] <- FALSE
  dependency_bitsets <- nodes$bits[dependencies]
  any(vapply(
    dependency_bitsets,
    is_subset,
    logical(1),
    node_bits
  ))
}

has_nondependency_superset <- function(node, nodes) {
  node_bits <- nodes$bits[[node]]
  nondependencies <- nodes$category < 0
  nondependencies[node] <- FALSE
  nondependency_bitsets <- nodes$bits[nondependencies]
  any(vapply(
    nondependency_bitsets,
    is_superset,
    logical(1),
    node_bits
  ))
}

pick_next_node <- function(node, nodes, trace, min_deps, max_non_deps) {
  if (nodes$category[node] == 3) { # candidate dependency
    s <- unchecked_subsets(node, nodes)
    s <- remove_pruned_subsets(s, min_deps, nodes$bits)
    if (length(s) == 0) {
      min_deps <- c(min_deps, node)
      nodes$category[node] <- 2L
    }else{
      trace <- c(node, trace)
      return(list(min(s), trace, nodes, min_deps, max_non_deps))
    }
  }else if (nodes$category[node] == -3) { # candidate non-dependency
    s <- unchecked_supersets(node, nodes)
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

unchecked_subsets <- function(index, nodes) {
  visited <- which(nodes$visited)
  children <- nodes$children[[index]]
  setdiff(children, visited)
}

unchecked_supersets <- function(index, nodes) {
  visited <- which(nodes$visited)
  parents <- nodes$parents[[index]]
  setdiff(parents, visited)
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

is_subset <- function(bits1, bits2) all(bits2[bits1])
is_superset <- function(bits1, bits2) all(bits1[bits2])

generate_next_seeds <- function(max_non_deps, min_deps, lhs_attr_nodes, nodes) {
  if (length(max_non_deps) == 0) {
    # original DFD paper doesn't mention case where no maximal non-dependencies
    # found yet, so this approach could be inefficient
    attrs_not_in_min_deps <- remove_pruned_subsets(
      lhs_attr_nodes,
      min_deps,
      nodes$bits
    )
    candidate_categories <- nodes$category[attrs_not_in_min_deps]
    seeds <- attrs_not_in_min_deps[candidate_categories >= 0]
  }else{
    seeds <- integer()
    for (nfd in max_non_deps) {
      max_non_dep_c <- remove_pruned_subsets(lhs_attr_nodes, nfd, nodes$bits)
      if (length(seeds) == 0)
        seeds <- max_non_dep_c
      else {
        seeds <- cross_intersection(seeds, max_non_dep_c, nodes$bits)
      }
    }
  }
  remove_pruned_supersets(seeds, min_deps, nodes$bits)
}

pack_vals <- as.integer(2^(0:30))
int_from_bits <- function(bits) {
  l <- as.logical(bits)[1:31]
  sum(l*pack_vals)
}

cross_intersection <- function(seeds, max_non_dep, bitsets) {
  new_seeds <- integer()
  for (dep in seeds) {
    seed_bitset <- intToBits(dep)
    for (set in max_non_dep) {
      set_bit_index <- intToBits(set)
      new_seed <- int_from_bits(seed_bitset | set_bit_index)
      new_seeds <- c(new_seeds, new_seed)
    }
  }
  minimise_seeds(new_seeds, bitsets)
}

minimise_seeds <- function(seeds, bitsets) {
  # remove duplicates first instead of comparing identical bitsets
  unique_seeds <- unique(seeds)
  n_seeds <- length(unique_seeds)
  include <- rep(TRUE, length(unique_seeds))
  for (n in seq_len(n_seeds - 1)) {
    if (include[n]) {
      for (m in seq.int(n + 1L, n_seeds)) {
        if (
          include[m] &&
          is_subset(bitsets[[unique_seeds[n]]], bitsets[[unique_seeds[m]]])
        )
          include[m] <- FALSE
      }
    }
  }
  unique_seeds[include]
}

compute_partitions <- function(df, rhs, lhs_set, partitions, accuracy, cache = FALSE) {
  n_rows <- nrow(df)
  threshold <- ceiling(n_rows*accuracy)
  if (threshold < n_rows)
    approximate_dependencies(lhs_set, rhs, df, partitions, threshold, cache = cache)
  else
    exact_dependencies(df, rhs, lhs_set, partitions, cache = cache)
}

exact_dependencies <- function(df, rhs, lhs_set, partitions, cache = FALSE) {
  partition <- if (cache) partition_stripped else partition_nclass
  res1 <- partition(lhs_set, df, partitions)
  part_lhs <- res1[[1]]
  partitions <- res1[[2]]
  if (part_lhs == 0)
    return(list(TRUE, partitions))
  res2 <- partition(union(lhs_set, rhs), df, partitions)
  part_union <- res2[[1]]
  partitions <- res2[[2]]
  list(part_union == part_lhs, partitions)
}

approximate_dependencies <- function(lhs_set, rhs, df, partitions, threshold, cache = FALSE) {
  partition <- if (cache) partition_stripped else partition_nclass

  # cheaper bounds checks:
  # nrow(df) - (part_lhs - part_union) <= majorities_total <= nrow(df) - part_lhs
  limit <- nrow(df) - threshold
  res1 <- partition(lhs_set, df, partitions)
  part_lhs <- res1[[1]]
  partitions <- res1[[2]]
  if (part_lhs <= limit)
    return(list(TRUE, partitions))
  res2 <- partition(union(lhs_set, rhs), df, partitions)
  part_union <- res2[[1]]
  partitions <- res2[[2]]
  if (part_lhs - part_union > limit)
    return(list(FALSE, partitions))

  if (cache) {
    # e(lhs_set -> rhs)
    ind_lhs <- match(list(sort(lhs_set)), partitions$set)
    stopifnot(!is.na(ind_lhs))
    classes_lhs <- partitions$value[[ind_lhs]]
    ind_union <- match(list(sort(union(lhs_set, rhs))), partitions$set)
    stopifnot(!is.na(ind_union))
    classes_union <- partitions$value[[ind_union]]
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
  }else{
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
}

partition_nclass <- function(attrs, df, partitions) {
  # This only returns the number |p| of equivalence classes in the partition p,
  # not its contents. This is less demanding on memory than storing stripped
  # partitions, but we cannot efficiently calculate the partition for supersets.
  attrs_set <- sort(attrs)
  index <- match(list(attrs_set), partitions$set)
  if (!is.na(index)) {
    return(list(partitions$value[index], partitions))
  }
  df_attrs_only <- df[, unlist(attrs), drop = FALSE]
  n_remove <- sum(duplicated(df_attrs_only))
  partitions$set <- c(partitions$set, list(sort(attrs)))
  partitions$value <- c(partitions$value, n_remove)
  list(n_remove, partitions)
}

partition_stripped <- function(attrs, df, partitions) {
  attrs_set <- sort(attrs)
  index <- match(list(attrs_set), partitions$set)
  if (!is.na(index)) {
    sp <- partitions$value[[index]]
    return(list(sum(lengths(sp)) - length(sp), partitions))
  }
  attr_indices <- seq_along(attrs_set)
  attrs_subsets <- lapply(attr_indices, \(n) attrs_set[-n])
  subsets_match <- match(attrs_subsets, partitions$set)
  if (sum(!is.na(subsets_match)) >= 2) {
    indices <- which(!is.na(subsets_match))[1:2]
    sp <- stripped_partition_product(
      partitions$value[[subsets_match[indices[1]]]],
      partitions$value[[subsets_match[indices[2]]]],
      nrow(df)
    )
  }else{
    if (sum(!is.na(subsets_match)) == 1) {
      index <- which(!is.na(subsets_match))
      main_subset <- attrs_subsets[[index]]
      small_subset <- setdiff(attrs, main_subset)
      main_partition <- partitions$value[[subsets_match[index]]]
      subres <- partition_stripped(small_subset, df, partitions)
      partitions <- subres[[2]]
      small_partition <- partitions$value[[match(small_subset, partitions$set)]]
      sp <- stripped_partition_product(
        main_partition,
        small_partition,
        nrow(df)
      )
    }else{
      sp <- fsplit_rows(df, attrs)
      sp <- unname(sp[lengths(sp) > 1])
    }
  }
  partitions$set <- c(partitions$set, list(sort(attrs)))
  partitions$value <- c(partitions$value, list(sp))
  list(sum(lengths(sp)) - length(sp), partitions)
}

stripped_partition_product <- function(sp1, sp2, n_rows) {
  # vectorised union of stripped partitions to replace algorithm from Tane
  tab <- rep(NA_integer_, n_rows)
  tab[unlist(sp1)] <- rep(seq_along(sp1), lengths(sp1))
  tab2 <- rep(NA_integer_, n_rows)
  tab2[unlist(sp2)] <- rep(seq_along(sp2), lengths(sp2))
  tab_both <- rep(NA_integer_, n_rows)
  in_both <- which(!is.na(tab) & !is.na(tab2))
  tab_both[in_both] <- paste(tab[in_both], tab2[in_both])
  sp <- split(seq_len(n_rows), tab_both)
  unname(sp[lengths(sp) >= 2])
}

fsplit <- function(splitted, splitter) {
  # Column contents are known to be integer, so we paste them together before
  # calling split. This is much faster than the iterated pasting of multiple f
  # elements done by interaction().
  single_splitter <- do.call(paste, splitter)
  split(splitted, single_splitter, drop = TRUE)
}

fsplit_rows <- function(df, attrs) {
  fsplit(seq_len(nrow(df)), df[, unlist(attrs), drop = FALSE])
}
