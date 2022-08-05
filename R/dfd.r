# Scratch pad
# - make.names docs: R didn't support underscores in names until 1.9.0, I need
# to set a limit for R version in DESCRIPTION if I remove the 4.1.0 pipe usage.
# - add vertical bar sets in tables plot, showing candidate key sets
# - add partitions

#' DFD algorithm
#'
#' The DFD algorithm finds all the minimal functional dependencies represented
#' in a relation/table, represented here in a data.frame. Checks each column to
#' see if it's unique. If it is unique, it is added as the LHS of a dependency
#' for every other element. It then loops through all the other non-unique
#' columns and determines all the LHS that the column depends on. (LHS -->
#' column)
#'
#' This implementation differs a little from the algorithm presented in the
#' original paper:
#' \itemize{
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
#'   \item Some attributes, or attribute types, can be designated, ahead of
#'   time, as not being candidate members for determinant sets. This reduces the
#'   number of candidate determinant sets to be searched, saving time by not
#'   searching for determinant sets that the user would remove later anyway.
#'   \item We do not yet preserve partitions, just their calculated sizes.
#'   \item We do not yet keep hashmaps to manage subset/superset relationships,
#'   as described in Section 3.5 of the original paper.
#' }
#' @param df a data.frame, the relation to evaluate.
#' @param accuracy a numeric in (0, 1]: the accuracy threshold required in order
#'   to conclude a dependency.
#' @param exclude a character vector, containing names of attributes to not
#'   consider as members of determinant sets. If names are given that aren't
#'   present in \code{df}, the user is given a warning.
#' @param exclude_class a character vector, indicating classes of attributes to
#'   not consider as members of determinant_sets. Attributes are excluded if
#'   they inherit from any given class.
#' @param progress an integer, for whether to display progress to the user. 0
#'   (default) displays nothing. 1 notes the start of finding each non-constant
#'   attribute's determinant sets. 2 also briefly describes the status of the
#'   search for an attribute's determinant sets when generating new seeds. 3
#'   also gives the status after visiting each candidate determinant set / node.
#' @param progress_file a scalar character or a connection. If \code{progress}
#'   is non-zero, determines where the progress is written to, in the same way
#'   as the \code{file} argument for \code{\link[base]{cat}}.
#'
#' @return a named list with two elements. \code{dependencies} is a named list,
#'   where the names give the dependent attribute, and each element is a list of
#'   character vectors. Each character vector is a set of determinant attributes
#'   for that dependent attribute. \code{attrs} is the column names of
#'   \code{df}, in order. This is kept to serve as a default priority order for
#'   the attributes during normalisation.
#' @export
dfd <- function(
  df,
  accuracy,
  exclude = character(),
  exclude_class = character(),
  progress = 0L,
  progress_file = ""
) {
  n_cols <- ncol(df)
  column_names <- colnames(df)
  if (n_cols == 0)
    return(list())
  if (n_cols == 1)
    return(stats::setNames(list(list()), column_names))
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
  df <- data.frame(lapply(df, \(x) as.integer(factor(x)))) |>
    stats::setNames(column_names)
  partitions <- list()
  dependencies <- stats::setNames(rep(list(list()), ncol(df)), column_names)
  fixed <- character()
  nonfixed <- column_names
  if (progress)
    cat("starting DFD\n", file = progress_file, append = FALSE)
  for (i in seq_along(column_names)) {
    attr <- column_names[i]
    if (all(is.na(df[[attr]])) || all(df[[attr]] == df[[attr]][1])) {
      if (progress)
        cat(paste(attr, "is fixed\n"), file = progress_file, append = TRUE)
      fixed <- c(fixed, attr)
      nonfixed <- setdiff(nonfixed, attr)
      dependencies[[attr]] <- as.list(setdiff(
        valid_determinant_attrs_prefixing,
        attr
      ))
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
    powerset <- powerset_nodes(max_n_lhs_attrs)
    for (rhs in nonfixed) {
      if (progress)
        cat(paste("dependent", rhs, "\n"), file = progress_file, append = TRUE)
      lhs_attrs <- setdiff(valid_determinant_attrs, rhs)
      n_lhs_attrs <- length(lhs_attrs)
      expected_n_lhs_attrs <- max_n_lhs_attrs -
        (n_dependent_only > 0 && is.element(rhs, valid_determinant_attrs))
      stopifnot(n_lhs_attrs == expected_n_lhs_attrs)
      if (n_lhs_attrs > 0) {
        nodes <- reduce_powerset(powerset, n_lhs_attrs)
        simple_nodes <- as.integer(2^(seq.int(n_lhs_attrs) - 1))
        if (progress)
          cat(
            "determinants available, starting search\n",
            file = progress_file,
            append = TRUE
          )
        lhss <- find_LHSs(
          rhs,
          lhs_attrs,
          nodes,
          simple_nodes,
          df,
          partitions,
          accuracy,
          progress,
          progress_file
        )
        dependencies[[rhs]] <- c(dependencies[[rhs]], lhss)
      }
    }
  }
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
        nodes$category[node] <- nodes$category[node]
      }else{
        inferred_type <- infer_type(node, nodes)
        if (!is.na(inferred_type)) {
          nodes$category[node] <- inferred_type
        }
        if (nodes$category[node] == 0L) {
          lhs_set <- lhs_attrs[as.logical(intToBits(node))]
          cp <- compute_partitions(df, rhs, lhs_set, partitions, accuracy)
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
        max_non_deps,
        colnames(df)
      )
      trace <- res[[2]]
      nodes <- res[[3]]
      min_deps <- res[[4]]
      max_non_deps <- res[[5]]
      if (progress >= 3L)
        cat(
          paste0(
            "node: ", node, ", ",
            "visited: ", sum(nodes$visited), ", ",
            "not visited: ", sum(!nodes$visited), ", ",
            "#seeds: ", length(seeds), ", ",
            "#min_deps: ", length(min_deps), ", ",
            "#max_non_deps: ", length(max_non_deps), ", ",
            "trace: ", length(trace), ", ",
            "category: ", nodes$category[node], "\n"
          ),
          file = progress_file,
          append = TRUE
        )
      node <- res[[1]]
    }
    new_seeds <- generate_next_seeds(max_non_deps, min_deps, simple_nodes, nodes)
    if (progress >= 2L)
      cat(
        paste0(
          "generate: 0, ",
          "visited: ", sum(nodes$visited), ", ",
          "not visited: ", sum(!nodes$visited), ", ",
          "#seeds: ", length(new_seeds), ", ",
          "#min_deps: ", length(min_deps), ", ",
          "#max_non_deps: ", length(max_non_deps), ", ",
          "trace: ", length(trace), ", ",
          "category: ", 0, "\n"
        ),
        file = progress_file,
        append = TRUE
      )
    if (progress >= 4L && setequal(seeds, new_seeds))
      cat(
        paste0(
          "seed status:\n",
          paste(
            vapply(
              seeds,
              \(s) paste0(
                "node: ", s, ", ",
                "children: ", toString(nodes$children[[s]]), ", ",
                "parents: ", toString(nodes$parents[[s]]), ", ",
                "category: ", nodes$category[s], ", ",
                "visited: ", nodes$visited[s], "\n"
              ),
              character(1)
            ),
            collapse = "\n"
          )
        ),
        file = progress_file,
        append = TRUE
      )
    seeds <- new_seeds
  }
  lapply(min_deps, \(md) lhs_attrs[as.logical(intToBits(md))])
}

powerset_nodes <- function(n) {
  if (n == 0)
    return(list(
      bits = raw(),
      children = list(),
      parents = list(),
      category = integer(),
      visited = logical()
    ))
  if (n == 1)
    return(list(
      bits = list(intToBits(1)),
      children = list(integer()),
      parents = list(integer()),
      category = 0L,
      visited = FALSE
    ))
  n_nonempty_subsets <- 2^n - 1
  node_bits <- lapply(seq.int(n_nonempty_subsets), intToBits)
  children <- rep(list(integer()), n_nonempty_subsets)
  parents <- rep(list(integer()), n_nonempty_subsets)
  for (x in seq.int(n_nonempty_subsets - 1)) {
    zeroes <- which(node_bits[[x]][1:n] == 0)
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

pick_next_node <- function(node, nodes, trace, min_deps, max_non_deps, attrs) {
  # Picks the next node to look at. If current node is a candidate minimum
  # dependency looks for unchecked subsets. If no unchecked subsets that could
  # be a dependency, current node must be a minimum dependency. Otherwise,
  # check an unchecked subset.
  # If current node is a candidate maximal dependnecy, look for unchecked
  # supersets. If no unchecked supersets that could be a non-dependency,
  # it must be a maximum non-dependency. Otherwise, check an unchecked superset.
  # If not a candidate, return last node on trace (go back down in graph)
  # Arguments:
  #     node (int) : current node just visited
  #     trace (list[int]) : stack of past nodes visited
  #     min_deps (list[int]) : discovered minimum dependencies
  #     max_non_deps (list[int]) : discovered maximum non-dependencies
  # Returns:
  #     next_node (int or NA) : next node to look at, NA if none left
  #     to check in currrent part of graph
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
  # Removes all pruned subsets. A subset can be pruned when it is a
  # subset of an existing discovered minimum dependency (because we
  # thus know it is a non-dependency)
  # Arguments:
  #     subsets (list[int]) : list of subset nodes
  #     min_deps (list[int]) : discovered minimal dependencies
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
  # Removes all pruned supersets. A superset can be pruned when it is
  # a superset of an existing discovered maximal non-dependency (because
  # we thus know it is a dependency)
  # Arguments:
  #     supersets (list[int]) : list of superset nodes
  #     max_non_deps (list[int]) : discovered maximal non-dependencies
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

is_subset <- function(bits1, bits2) identical(bits1 & bits2, bits1)
is_superset <- function(bits1, bits2) identical(bits1 & bits2, bits2)

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

cross_intersection <- function(seeds, max_non_dep, bitsets) {
  new_seeds <- integer()
  for (dep in seeds) {
    seed_bitset <- intToBits(dep)
    for (set in max_non_dep) {
      set_bit_index <- intToBits(set)
      new_seed <- packBits(seed_bitset | set_bit_index, "integer")
      new_seeds <- c(new_seeds, new_seed)
    }
  }
  minimise_seeds(new_seeds, bitsets)
}

minimise_seeds <- function(seeds, bitsets) {
  minimised <- integer()
  for (n in seq_along(seeds)) {
    current_seed <- seeds[n]
    other_seeds <- seeds[-n]
    minimised <- c(
      minimised,
      remove_pruned_subsets(current_seed, other_seeds, bitsets)
    )
  }
  minimised
}

compute_partitions <- function(df, rhs, lhs_set, partitions, accuracy) {
  if (accuracy < 1)
    return(c(
      approximate_dependencies(lhs_set, rhs, df, accuracy),
      list(partitions)
    ))
  res1 <- partition(union(lhs_set, rhs), df, partitions)
  part_rhs <- res1[[1]]
  partitions <- res1[[2]]
  res2 <- partition(lhs_set, df, partitions)
  part_lhs <- res2[[1]]
  partitions <- res2[[2]]
  list(part_rhs == part_lhs, partitions)
}

partition <- function(attrs, df, partitions) {
  # This only returns |C| for the equivalence class C, not its contents. This is
  # less demanding on memory, but we cannot efficiently calculate the
  # equivalence class for supersets.
  attrs_set <- sort(attrs)
  index <- match(list(attrs_set), partitions$set)
  if (!is.na(index)) {
    return(list(partitions$value[index], partitions))
  }
  df_attrs_only <- df[, unlist(attrs), drop = FALSE]
  shape <- nrow(df_attrs_only) - sum(duplicated(df_attrs_only))
  partitions$set <- c(partitions$set, list(sort(attrs)))
  partitions$value <- c(partitions$value, shape)
  list(shape, partitions)
}

approximate_dependencies <- function(lhs_set, rhs, df, accuracy) {
  # This is a quick working version I put together to replace the non-working
  # original. There's a known better way to do this, see TANE section 2.3s.
  rows <- nrow(df)
  limit <- rows * (1 - accuracy)
  n_remove <- function(x) {
    length(x) - max(tabulate(x))
  }
  splitted <- df[[rhs]]
  splitter <- df[, lhs_set, drop = FALSE]
  total_to_remove <- split(splitted, splitter, drop = TRUE) |>
    Reduce(f = function(n, df) n + n_remove(df), init = 0)
  total_to_remove <= limit
}
