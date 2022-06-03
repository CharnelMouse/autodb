#' DFD algorithm
#'Main loop of DFD algorithm. It returns all the dependencies represented in the
#'data in dataframe df. Refer to section 3.2 of paper for literature. Checks
#'each column to see if it's unique. If it is unique, it is added as the LHS of
#'a dependency for every other element. It then loops through all the other
#'non-unique columns and determines all the LHS that the column depends on. (LHS
#'--> column)
#' @param df a data.frame, the relation to evaluate.
#' @param accuracy a numeric in (0, 1]: the accuracy threshold required in order
#'   to conclude a dependency.
#' @param index a character, the name of the attribute in the
#'   pre-determined index for the relation. Defaults to NA for no index. This
#'   attribute is marked as unique on the rows, and therefore a determinant of
#'   the other attributes. It is excluded when searching for functional
#'   dependencies.
#' @param progress a logical, for whether to display progress to the user.
#'
#' @return a named list, where the names give the dependent attribute, and each
#'   element is a list of character vectors. Each character vector is a set of
#'   determinant attributes for that dependent attribute.
#' @export
dfd <- function(df, accuracy, index = NA, progress = FALSE) {
  partitions <- list()
  column_names <- colnames(df)
  non_uniq <- column_names
  unique_attrs <- list()
  dependencies <- stats::setNames(rep(list(list()), ncol(df)), colnames(df))
  for (i in seq_along(column_names)) {
    attr <- column_names[i]
    if (progress)
      cat(paste("checking if", attr, "is unique\n"))
    if (!anyDuplicated(df[[attr]]) || (!is.na(index) && attr == index)) {
      unique_attrs <- c(unique_attrs, attr)
      non_uniq <- setdiff(non_uniq, attr)
      dependencies[-i] <- lapply(dependencies[-i], c, list(attr))
    }
  }
  for (i in non_uniq) {
    if (progress)
      cat(paste("dependent", i, "\n"))
    lhss <- find_LHSs(i, non_uniq, df, partitions, accuracy)
    dependencies[[i]] <- c(dependencies[[i]], lhss)
  }
  dependencies
}

find_LHSs <- function(rhs, attrs, df, partitions, accuracy) {
  # Node categories:
  # -3 = candidate maximal non-dependency
  # -2 = maximal non-dependency
  # -1 = non-maximal non-dependency
  #  0 = unresolved
  #  1 = non-minimal dependency
  #  2 = minimal dependency
  #  3 = candidate minimal dependency
  lhs_attrs <- setdiff(attrs, rhs)
  seeds <- lhs_attrs
  nodes <- nodes_from_seeds(seeds)
  seeds <- as.list(seeds)
  min_deps <- list()
  max_non_deps <- list()
  trace <- list()
  while (length(seeds) != 0) {
    node <- sample(seeds, 1)[[1]]
    while (!is.na(node[1])) {
      index <- node_index(node, nodes)
      if (nodes$visited[index]) {
        if (nodes$category[index] == 3) { # dependency
          if (is_minimal(node, nodes)) {
            nodes$category[index] <- 2L
            min_deps <- c(min_deps, list(node))
          }
        }
        if (nodes$category[index] == -3) { # non-dependency
          if (is_maximal(node, nodes)) {
            nodes$category[index] <- -2L
            max_non_deps <- c(max_non_deps, list(node))
          }
        }
        nodes$category[index] <- update_dependency_type(
          node,
          nodes,
          min_deps,
          max_non_deps
        )
      }else{
        inferred_type <- infer_type(node, nodes)
        if (!is.na(inferred_type))
          nodes$category[index] <- inferred_type
        if (nodes$category[index] == 0L) {
          cp <- compute_partitions(df, rhs, node, partitions, accuracy)
          result <- cp[[1]]
          partitions <- cp[[2]]
          if (result) {
            if (is_minimal(node, nodes)) {
              min_deps <- c(min_deps, list(node))
              nodes$category[index] <- 2L
            }else{
              nodes$category[index] <- 3L
            }
          }else{
            if (is_maximal(node, nodes)) {
              max_non_deps <- c(max_non_deps, list(node))
              nodes$category[index] <- -2L
            }else{
              nodes$category[index] <- -3L
            }
          }
        }
        nodes$visited[index] <- TRUE
      }
      res <- pick_next_node(
        node,
        nodes,
        trace,
        min_deps,
        max_non_deps,
        colnames(df)
      )
      node <- res[[1]]
      trace <- res[[2]]
      nodes <- res[[3]]
    }
    seeds <- generate_next_seeds(max_non_deps, min_deps, lhs_attrs, nodes)
  }
  min_deps
}

is_minimal <- function(node, nodes) {
  index <- node_index(node, nodes)
  children <- nodes$children[[index]]
  children_indices <- vapply(children, node_index, integer(1), nodes)
  all(nodes$category[children_indices] < 0)
}

is_maximal <- function(node, nodes) {
  index <- node_index(node, nodes)
  parents <- nodes$parents[[index]]
  parent_indices <- vapply(parents, node_index, integer(1), nodes)
  all(nodes$category[parent_indices] > 0)
}

update_dependency_type <- function(node, nodes, min_deps, max_non_deps) {
  index <- node_index(node, nodes)
  children <- nodes$children[[index]]
  children_minimal <- vapply(
    children,
    \(c) any(vapply(min_deps, identical, logical(1), c)),
    logical(1)
  )
  if (any(children_minimal))
    return(1L)
  parents <- nodes$parents[[index]]
  parents_maximal <- vapply(
    parents,
    \(p) any(vapply(max_non_deps, identical, logical(1), p)),
    logical(1)
  )
  if (any(parents_maximal))
    return(-1L)
  nodes$category[index]
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
  index <- node_index(node, nodes)
  children <- nodes$children[[index]]
  children_indices <- vapply(children, node_index, integer(1), nodes)
  any(nodes$category[children_indices] > 0)
}

non_dep_superset <- function(node, nodes) {
  index <- node_index(node, nodes)
  parents <- nodes$parent[[index]]
  parent_indices <- vapply(parents, node_index, integer(1), nodes)
  any(nodes$category[parent_indices] < 0)
}

node_index <- function(node, nodes) {
  matches <- vapply(nodes$node, identical, logical(1), node)
  ind <- which(matches)
  if (length(ind) != 1)
    stop(paste0(
      "missing index: ", toString(node),
      "\nnode class: ", toString(class(node)),
      "\nnodes: ", toString(nodes$node),
      "\nmatches: ", toString(matches)
    ))
  ind
}

nodes_from_seeds <- function(seeds) {
  # Returns nodes from a list of seeds. Creates nodes for each seed, and
  # connects them, forming the lattice graph.
  # Arguments:
  #     seeds (set[str]) : set of column names of seeds
  # Returns:
  #     nodes (list[Node]) : list of base nodes for lattice graph
  lattice <- list()
  lattice_children <- list()
  lattice_parents <- list()

  this_level <- as.list(seeds)
  this_children <- rep(list(list()), length(seeds))
  this_parents <- rep(list(list()), length(seeds))

  for (level in seq_along(seeds)[-1]) {
    next_level <- utils::combn(seeds, level, simplify = FALSE)
    next_children <- rep(list(list()), length(next_level))
    next_parents <- rep(list(list()), length(next_level))
    for (this_index in seq_along(this_level)) {
      for (next_index in seq_along(next_level)) {
        next_set <- next_level[[next_index]]
        if (all(this_level[[this_index]] %in% next_set)) {
          next_children[[next_index]] <- c(
            next_children[[next_index]],
            list(this_level[[this_index]])
          )
          this_parents[[this_index]] <- c(
            this_parents[[this_index]],
            list(next_set)
          )
        }
      }
    }
    lattice <- c(lattice, this_level)
    lattice_children <- c(lattice_children, this_children)
    lattice_parents <- c(lattice_parents, this_parents)
    this_level <- next_level
    this_children <- next_children
    this_parents <- next_parents
  }
  lattice <- c(lattice, this_level)
  lattice_children <- c(lattice_children, this_children)
  lattice_parents <- c(lattice_parents, this_parents)
  list(
    node = lattice,
    children = lattice_children,
    parents = lattice_parents,
    category = rep(0L, length(lattice)),
    visited = rep(FALSE, length(lattice))
  )
}

sort_key <- function(node, sorted_attrs) {
  # Sort key for sorting lists of nodes.
  # MY NOTES: This returns a bitset for presence in node,
  # I don't know why it's named sort_key.
  powers <- seq.int(length(sorted_attrs)) - 1L
  sum(10^(powers[sorted_attrs %in% node]))
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
  #     node (Node) : current node just visited
  #     trace (list[Node]) : stack of past nodes visited
  #     min_deps (LHSs) : discovered minimum dependencies
  #     max_non_deps (LHSs) : discovered maximum non-dependencies
  # Returns:
  #     next_node (Node or None) : next node to look at, None if none left
  #     to check in currrent part of graph
  srt_key <- \(s) sort_key(s, sort(attrs)) # partial curries functions
  index <- node_index(node, nodes)
  if (nodes$category[index] == 3) { # candidate dependency
    s <- unchecked_subsets(index, nodes)
    s <- remove_pruned_subsets(s, min_deps)
    if (length(s) == 0) {
      min_deps <- c(min_deps, list(node))
      nodes$category[index] <- 2L
      return(list(NA, trace, nodes))
    }else{
      trace <- c(list(node), trace)
      return(list(
        s[order(vapply(s, srt_key, numeric(1)))][[1]], # calls srt_key to decide the order
        trace,
        nodes
      ))
    }
  }else if (nodes$category[index] == -3) { # candidate non-dependency
    s <- unchecked_supersets(index, nodes)
    s <- remove_pruned_supersets(s, max_non_deps)
    if (length(s) == 0) {
      max_non_deps <- c(max_non_deps, list(node))
      nodes$category[index] <- -2L
      return(list(NA, trace, nodes))
    }else{
      trace <- c(list(node), trace)
      return(list(
        s[order(vapply(s, srt_key, numeric(1)))][[1]],
        trace,
        nodes
      ))
    }
  }
  if (length(trace) == 0)
    return(list(NA, trace, nodes))
  list(trace[[1]], trace[-1], nodes)
}

unchecked_subsets <- function(index, nodes) {
  visited <- nodes$node[nodes$visited]
  children <- nodes$children[[index]]
  setdiff(children, visited)
}

unchecked_supersets <- function(index, nodes) {
  visited <- nodes$node[nodes$visited]
  parents <- nodes$parents[[index]]
  setdiff(parents, visited)
}

remove_pruned_subsets <- function(subsets, min_deps) {
  # Removes all pruned subsets. A subset can be pruned when it is a
  # subset of an existing discovered minimum dependency (because we
  # thus know it is a non-dependency)
  # Arguments:
  #     subsets (list[Node]) : list of subset nodes
  #     min_deps (LHSs) : discovered minimal dependencies
  for (subset in subsets) {
    if (any(vapply(
      min_deps,
      \(min_dep) all(subset %in% min_dep),
      logical(1)
    )))
      subsets <- setdiff(subsets, subset)
  }
  subsets
}

remove_pruned_supersets <- function(supersets, max_non_deps) {
  # Removes all pruned supersets. A superset can be pruned when it is
  # a superset of an existing discovered maximal non-dependency (because
  # we thus know it is a dependency)
  # Arguments:
  #     supersets (list[Node]) : list of superset nodes
  #     max_non_deps (LHSs) : discovered maximal non-dependencies
  for (superset in supersets) {
    if (any(vapply(
      max_non_deps,
      \(max_dep) all(superset %in% max_dep),
      logical(1)
    )))
      supersets <- setdiff(supersets, superset)
  }
  supersets
}

generate_next_seeds <- function(max_non_deps, min_deps, lhs_attrs, nodes) {
  # Generates seeds for the nodes that are still unchecked due to pruning.
  # This is done based off of the knowledge that once every possibility has
  # been considered, the compliment of hte maximal non-dependencies, minus
  # the existing minimum dependencies is 0 (the two are equal). Thus, if this
  # is not satisfied, the new seeds are the remaining elements.
  # Arguments:
  #     max_non_deps (LHSs) : discovered maximal non-dependencies
  #     min_deps (LHSs) : discovered minimal dependencies
  #     lhs_attrs (set[str]) : attributes being considered as parts of LHSs
  # Returns:
  #     seed_attributes (list[str]) : list of seeds that need to be visited
  if (length(max_non_deps) == 0) {
    candidate_node_lattice <- setdiff(lhs_attrs, unlist(min_deps)) |>
      nodes_from_seeds()
    candidate_indices <- vapply(
      candidate_node_lattice$node,
      node_index,
      integer(1),
      nodes
    )
    candidate_categories <- nodes$category[candidate_indices]
    candidate_nodes <- candidate_node_lattice$node[candidate_categories >= 0]
    seeds <- candidate_nodes
  }else{
    seeds <- list()
    for (nfd in max_non_deps) {
      nfd_compliment <- setdiff(lhs_attrs, nfd)
      if (length(seeds) == 0)
        seeds <- as.list(nfd_compliment)
      else
        seeds <- cross_intersection(seeds, nfd_compliment)
    }
    # MY NOTES: minimise new deps here
  }
  not_min_superset <- vapply(
    seeds,
    \(seed) all(vapply(min_deps, \(md) !all(md %in% seed), logical(1))),
    logical(1)
  )
  seeds[not_min_superset]
}

cross_intersection <- function(seeds, new_set) {
  new_seeds <- list()
  for (seed in seeds) {
    for (new_el in new_set) {
      sd <- sort(unique(c(seed, new_el)))
      new_seeds <- c(new_seeds, list(sd))
    }
  }
  unique(new_seeds)
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
  # if part_rhs > df.shape[1] * rep_percent:
  #     return FALSE
  res2 <- partition(lhs_set, df, partitions)
  part_lhs <- res2[[1]]
  partitions <- res2[[2]]
  list(part_rhs == part_lhs, partitions)
}

partition <- function(attrs, df, partitions) {
# Returns the number of equivalence classes for the columns represented in attrs
# for dataframe df.
  attrs_set <- sort(attrs)
  if (list(attrs_set) %in% partitions$set) {
    index <- which(vapply(partitions$set, identical, logical(1), attrs_set))
    return(list(partitions$value[index], partitions))
  }
  shape <- nrow(unique(df[, unlist(attrs), drop = FALSE]))
  partitions$set <- c(partitions$set, list(sort(attrs)))
  partitions$value <- c(partitions$value, shape)
  list(shape, partitions)
}

approximate_dependencies <- function(lhs_set, rhs, df, accuracy) {
  # This is a quick working version I put together to replace the non-working
  # original.
  # There's probably a better way to do this, see TANE section 2.3s.
  rows <- nrow(df)
  limit <- rows * (1 - accuracy)
  n_remove <- function(x) {
    length(x) - max(tabulate(x))
  }
  total_to_remove <- tapply(
    factor(df[[rhs]], exclude = character()),
    lapply(df[, lhs_set, drop = FALSE], factor, exclude = character()),
    n_remove,
    default = 0L
  ) |>
    sum()
  total_to_remove <= limit
}
