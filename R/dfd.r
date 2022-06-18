# Scratch pad
# - add removal of transitive dependencies back in
# - make.names docs: R didn't support underscores in names until 1.9.0, I need
# to set a limit for R version in DESCRIPTION.
# - data.frames split off from parents aren't being checked properly for best
# choice of index, i.e. columns to leave behind as foreign keys in the parent.
# This might be causing the plotting problems.
# - keep merged attribute determinant sets when normalising dependencies, so can
# pick by priority once given a data.frame

#' DFD algorithm
#'
#' The DFD algorithm finds all the minimal functional dependencies represented
#' in a relation/table, represented here in a data.frame. Checks each column to
#' see if it's unique. If it is unique, it is added as the LHS of a dependency
#' for every other element. It then loops through all the other non-unique
#' columns and determines all the LHS that the column depends on. (LHS -->
#' column)
#' @param df a data.frame, the relation to evaluate.
#' @param accuracy a numeric in (0, 1]: the accuracy threshold required in order
#'   to conclude a dependency.
#' @param progress a logical, for whether to display progress to the user.
#'
#' @return a named list, where the names give the dependent attribute, and each
#'   element is a list of character vectors. Each character vector is a set of
#'   determinant attributes for that dependent attribute.
#' @export
dfd <- function(df, accuracy, progress = FALSE) {
  n_cols <- ncol(df)
  column_names <- colnames(df)
  if (n_cols == 0)
    return(list())
  if (n_cols == 1)
    return(stats::setNames(list(list()), column_names))
  # convert all columns to integers, since they're checked for duplicates more
  # quickly when calculating partitions
  df <- data.frame(lapply(df, \(x) as.integer(factor(x)))) |>
    stats::setNames(column_names)
  partitions <- list()
  dependencies <- stats::setNames(rep(list(list()), ncol(df)), column_names)
  fixed <- character()
  nonfixed <- column_names
  for (i in seq_along(column_names)) {
    attr <- column_names[i]
    if (progress)
      cat(paste("checking if", attr, "is fixed\n"))
    if (all(is.na(df[[attr]])) || all(df[[attr]] == df[[attr]][1])) {
      fixed <- c(fixed, attr)
      nonfixed <- setdiff(nonfixed, attr)
      dependencies[[attr]] <- as.list(setdiff(column_names, attr))
    }
  }
  n_lhs_attrs <- length(nonfixed) - 1L
  # using 0 would allow for one more column, but that's for a later date
  max_attrs <- floor(log(.Machine$integer.max, 2))
  if (n_lhs_attrs > max_attrs)
    stop(paste(
      "only data.frames with up to", max_attrs, "columns currently supported"
    ))
  if (n_lhs_attrs > 0) {
    nodes <- powerset_nodes(n_lhs_attrs)
    simple_nodes <- as.integer(2^(seq.int(n_lhs_attrs) - 1))
    for (rhs in nonfixed) {
      if (progress)
        cat(paste("dependent", rhs, "\n"))
      lhs_attrs <- setdiff(nonfixed, rhs)
      stopifnot(length(lhs_attrs) == n_lhs_attrs)
      lhss <- find_LHSs(rhs, lhs_attrs, nodes, simple_nodes, df, partitions, accuracy, progress)
      dependencies[[rhs]] <- c(dependencies[[rhs]], lhss)
    }
  }
  dependencies
}

find_LHSs <- function(
  rhs,
  lhs_attrs,
  nodes,
  simple_nodes,
  df,
  partitions,
  accuracy,
  progress = FALSE
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
          if (is_minimal(node, nodes)) {
            nodes$category[node] <- 2L
            min_deps <- c(min_deps, node)
          }
        }
        if (nodes$category[node] == -3) { # non-dependency
          if (is_maximal(node, nodes)) {
            nodes$category[node] <- -2L
            max_non_deps <- c(max_non_deps, node)
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
            if (is_minimal(node, nodes)) {
              min_deps <- c(min_deps, node)
              nodes$category[node] <- 2L
            }else{
              nodes$category[node] <- 3L
            }
          }else{
            if (is_maximal(node, nodes)) {
              max_non_deps <- c(max_non_deps, node)
              nodes$category[node] <- -2L
            }else{
              nodes$category[node] <- -3L
            }
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
      if (progress)
        cat(paste0(
          "node: ", node, ", ",
          "visited: ", sum(nodes$visited), ", ",
          "not visited: ", sum(!nodes$visited), ", ",
          "#seeds: ", length(seeds), ", ",
          "#min_deps: ", length(min_deps), ", ",
          "#max_non_deps: ", length(max_non_deps), ", ",
          "trace: ", length(trace), "\n"
        ))
      node <- res[[1]]
    }
    new_seeds <- generate_next_seeds(max_non_deps, min_deps, simple_nodes, nodes)
    if (progress)
      cat(paste0(
        "generate: ",
        "visited: ", sum(nodes$visited), ", ",
        "not visited: ", sum(!nodes$visited), ", ",
        "#seeds: ", length(seeds), ", ",
        "#min_deps: ", length(min_deps), ", ",
        "#max_non_deps: ", length(max_non_deps), ", ",
        "trace: ", length(trace), "\n"
      ))
    if (progress && setequal(seeds, new_seeds))
      cat(paste0(
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
      ))
    seeds <- new_seeds
  }
  lapply(min_deps, \(md) lhs_attrs[as.logical(intToBits(md))])
}

powerset_nodes <- function(n) {
  if (n == 0)
    return(list(
      bits = raw(),
      logicals = logical(),
      children = list(),
      parents = list(),
      category = integer(),
      visited = logical()
    ))
  if (n == 1)
    return(list(
      bits = intToBits(1),
      logicals = list(TRUE),
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
    for (y in seq(x + 1L, n_nonempty_subsets)) {
      # x | y: minimal join of sets
      # x & y: maximal intersection of sets
      # xor(x, y): non-shared elements
      # x | y == x: x superset of y
      # x & y == x: x subset of y
      # xor(x, y) == x: y is empty
      # sum(x != y) == 1: differ by one element
      node_x <- node_bits[[x]]
      node_y <- node_bits[[y]]
      if (sum(node_x != node_y) == 1) { # differ by one element
        if (all((node_x & node_y) == node_x)) { # x is subset
          children[[y]] <- c(children[[y]], x)
          parents[[x]] <- c(parents[[x]], y)
        }else{
          children[[x]] <- c(children[[x]], y)
          parents[[y]] <- c(parents[[y]], x)
        }
      }
    }
  }
  list(
    bits = node_bits,
    logicals = lapply(node_bits, \(x) as.logical(x)[seq.int(n)]),
    children = children,
    parents = parents,
    category = rep(0L, n_nonempty_subsets),
    visited = rep(FALSE, n_nonempty_subsets)
  )
}

is_minimal <- function(node, nodes) {
  children <- nodes$children[[node]]
  all(nodes$category[children] < 0)
}

is_maximal <- function(node, nodes) {
  parents <- nodes$parents[[node]]
  all(nodes$category[parents] > 0)
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
  # TO DO: optimize, this is inefficient (or it's helper functions are)
  category <- NA
  if (dep_subset(node, nodes))
    category <- 1L
  if (non_dep_superset(node, nodes))
    category <- -1L
  category
}

dep_subset <- function(node, nodes) {
  children <- nodes$children[[node]]
  any(nodes$category[children] > 0)
}

non_dep_superset <- function(node, nodes) {
  parents <- nodes$parent[[node]]
  any(nodes$category[parents] < 0)
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
      return(list(NA, trace, nodes))
    }else{
      trace <- c(node, trace)
      return(list(min(s), trace, nodes))
    }
  }else if (nodes$category[node] == -3) { # candidate non-dependency
    s <- unchecked_supersets(node, nodes)
    s <- remove_pruned_supersets(s, max_non_deps, nodes$bits)
    if (length(s) == 0) {
      max_non_deps <- c(max_non_deps, node)
      nodes$category[node] <- -2L
      return(list(NA, trace, nodes))
    }else{
      trace <- c(node, trace)
      return(list(min(s), trace, nodes))
    }
  }
  if (length(trace) == 0)
    return(list(NA, trace, nodes))
  list(trace[1], trace[-1], nodes)
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
  splitted <- factor(df[[rhs]], exclude = character())
  splitter <- lapply(df[, lhs_set, drop = FALSE], factor, exclude = character())
  total_to_remove <- split(splitted, splitter, drop = TRUE) |>
    Reduce(f = function(n, df) n + n_remove(df), init = 0)
  total_to_remove <= limit
}
