normalize <- function(x, ...) {
  UseMethod("normalize")
}

#' Normalizes dependency relationships
#'
#' Normalizes the dependency relationships in dependencies into new
#' groups by breaking up all partial and transitive dependencies.
#'
#' @param x a Dependencies object, containing the dependencies to be normalised.
#' @inheritParams auto_entityset
#'
#' @return a list of Dependencies objects, containing the normalised
#'   dependencies.
#' @export
normalize.Dependencies <- function(x, df) {
  # Fully breaks up the dependencies, so all normalized instead of just the
  # original.
  # Work flow:
  # - remove extroneous LHS attributes
  # - remove partial dependencies
  #   - split_then_remove on all found partial dependencies
  #     - find most common set of LHS attributes in partial deps, prefer shortest
  #     - split_on_dep with most common on Dependencies
  #     - remove partial dependencies in resulting parent, then result child, concatenate
  #     - return resulting Dependencies list
  # - for each non-partial dependency in list, remove transitive deps
  #   - split_then_remove on all found transitive dependencies
  #     - find most common set of LHS attributes in transitive deps, prefer shortest
  #     - split_on_dep with most common on Dependencies
  #     - remove transitive dependencies in resulting parent, then result child, concatenate
  #     - return resulting Dependencies list
  #   - concatenate lists
  # - return resulting Dependencies list
  # After partial removal, tables are sorted by reverse order in which extracted
  # from parent table for partial dependencies, depth-first.
  # After transitive removal, same thing again.
  x <- remove_extraneous_attributes(x)
  no_part_deps <- remove_part_deps(x, df)
  no_trans_deps <- list()
  for (grp in no_part_deps)
    no_trans_deps <- c(no_trans_deps, remove_trans_deps(grp, df))
  no_trans_deps
}

#' Normalise a given entity set
#'
#' @param x an EntitySet object, containing a single data.frame to be
#'   normalised.
#' @inheritParams auto_entityset
#'
#' @return The created EntitySet, containing the tables from normalising the
#'   original data.frame.
#' @export
normalize.EntitySet <- function(x, accuracy) {
  # TO DO: add option to pass an EntitySet with more than one dataframe, and
  # specify which one to normalize while preserving existing relationships
  if (length(x$dataframes) > 1)
    stop('There is more than one dataframe in this EntitySet')
  if (length(x$dataframes) == 0)
    stop('This EntitySet is empty')

  df <- x$dataframes[[1]]
  auto_entityset(
    df$df,
    accuracy,
    index = df$index,
    name = x$name,
    time_index = df$time_index
  )
}

normalize2 <- function(dependencies, df) {
  remove_extraneous_attributes(dependencies) |>
    remove_extraneous_dependencies() |>
    partition_dependencies() |>
    merge_equivalent_keys() |>
    remove_transitive_dependencies() |>
    add_bijections() |>
    construct_relations()
}

remove_extraneous_dependencies <- function(dependencies) {
  old_deps <- NULL
  new_deps <- dependencies$dependencies
  while (!identical(old_deps, new_deps)) {
    old_deps <- new_deps
    for (n in seq_along(new_deps)) {
      LHSs <- new_deps[[n]]
      RHS <- names(new_deps)[n]
      other_deps <- new_deps[-n]
      rem_left <- rep(FALSE, length(LHSs))
      for (m in seq_along(LHSs)) {
        LHS <- LHSs[[m]]
        closure <- find_closure(
          tuple_relations(Dependencies(c(
            other_deps,
            stats::setNames(list(LHSs[-m][!rem_left[-m]]), RHS)
          ))),
          LHS
        )
        rem_left[m] <- (RHS %in% closure)
      }
      new_deps[[n]] <- LHSs[!rem_left]
    }
  }
  Dependencies(new_deps, primary_key = dependencies$primary_key)
}

partition_dependencies <- function(dependencies) {
  tr <- tuple_relations(dependencies)
  LHS_list <- lapply(tr, `[[`, 1)
  LHSs <- unique(LHS_list)
  partition <- list()
  for (LHS in LHSs) {
    matches <- vapply(LHS_list, identical, logical(1), LHS)
    partition <- c(partition, list(tr[matches]))
  }
  list(
    dependencies = dependencies,
    relations = tr,
    partition = partition,
    LHSs = LHSs
  )
}

merge_equivalent_keys <- function(dep_partition) {
  partition <- dep_partition$partition
  LHSs <- dep_partition$LHSs
  if (length(partition) <= 1)
    return(list(
      partition = partition,
      keys = lapply(LHSs, list),
      bijections = list()
    ))
  dependencies <- dep_partition$dependencies
  relations <- dep_partition$relations

  keys <- LHSs
  bijection_rels <- list()
  for (n in seq.int(length(partition) - 1)) {
    grp <- partition[[n]]
    if (length(grp) > 0) {
      LHS <- LHSs[[n]]
      key1 <- keys[[n]]
      for (m in (n + 1):length(partition)) {
        grp2 <- partition[[m]]
        if (length(grp2) > 0) {
          LHS2 <- LHSs[[m]]
          key2 <- keys[[m]]
          closure1 <- find_closure(relations, LHS)
          closure2 <- find_closure(relations, LHS2)

          if (all(key1 %in% closure2) && all(key2 %in% closure1)) {

            new_bijections <- list()
            new_bijections <- c(
              new_bijections,
              list(
                list(key1, key2),
                list(key2, key1)
              )
            )
            bijection_rels <- c(bijection_rels, new_bijections)

            obsolete <- vapply(
              partition[[m]],
              \(r) r[[2]] %in% c(key1, key2),
              logical(1)
            )
            partition[[n]] <- unique(c(
              partition[[n]],
              lapply(partition[[m]][!obsolete], \(r) list(key1, r[[2]]))
            ))

            partition[[m]] <- list()
          }
        }
      }
    }
  }
  nonempty <- lengths(partition) > 0
  list(
    partition = partition[nonempty],
    keys = keys[nonempty],
    bijections = bijection_rels
  )
}

remove_transitive_dependencies <- function(lst) {
  lst
}

add_bijections <- function(lst) {
  partition <- lst$partition
  keys <- lst$keys
  bijections <- lst$bijections
  for (n in seq_along(keys)) {
    key <- keys[[n]]
    matches <- vapply(
      bijections,
      \(b) identical(key, b[[1]]) || identical(key, b[[2]]),
      logical(1)
    )
    partition[[n]] <- c(partition[[n]], bijections[matches])
    bijections <- bijections[!matches]
  }
  partition
}

construct_relations <- function(partition) {
  lapply(
    partition,
    \(rels) {
      LHSs <- unique(lapply(rels, `[[`, 1))
      RHSs <- unique(vapply(rels, `[[`, character(1), 2))
      all_attrs <- union(unlist(LHSs), RHSs)
      list(attrs = all_attrs, keys = LHSs)
    }
  )
}

Dependencies <- function(dependencies, primary_key = NULL) {
  lst <- list(
    dependencies = dependencies,
    primary_key = primary_key
  )
  class(lst) <- c("Dependencies", class(lst))
  lst
}

make_indexes <- function(depdfs) {
  # Goes through depdf, and all of its descendents, and if any have primary keys
  # of more than one attribute, creates a new index column, and replaces the old
  # primary key columns with the new column in the parent df.
  #
  # Arguments:
  #     depdf (DepDF) : depDF to make indexes for
  depdfs
  # depdf <- depdfs[[1]]
  # prim_key <- get_prim_key(depdf$deps)
  # prim_key_snake <- paste(prim_key, collapse = "_")
  #
  # if (length(prim_key) > 1) {
  #   depdf$df.insert(0, prim_key_snake, range(0, length(depdf$df)))
  #   depdf$index <- prim_key_snake
  #
  #   # now need to replace it in the parent df...
  #   if (!is.na(depdf$parent)) {
  #     add <- rep(NA, length(depdf$parent$df))
  #     indices <- match(prim_key, colnames(depdf$parent$df))
  #
  #     for (name in indices) {
  #       mask <- NA
  #       for (i in range(length(prim_key))) {
  #         m <- depdf$df[prim_key[i]] == name[i]
  #         if (is.na(mask))
  #           mask <- m
  #         else
  #           mask <- mask & m
  #       }
  #       new_val <- depdf$df[mask][prim_key_snake][1]
  #
  #       for (index in indices[name])
  #         add[index] <- new_val
  #     }
  #     depdf$parent$df.drop(columns = prim_key, inplace = TRUE)
  #     depdf$parent$df.insert(
  #       ncol(depdf$parent$df),
  #       prim_key_snake,
  #       add
  #     )
  #   }
  # }
  # for (child in depdf$children)
  #   make_indexes(child)
}

remove_part_deps <- function(dependencies, df) {
  UseMethod("remove_part_deps")
}

#' @export
remove_part_deps.Dependencies <- function(dependencies, df) {
  # Breaks up the dependency relations in dependencies into new groups of
  # relations so that there are no more partial dependencies.
  #
  # Arguments:
  #     dependencies (Dependncies) : the dependencies to be split up
  #
  # Returns:
  #     new_groups (list[Dependencies]) : list of new dependencies objects
  #     representing the new groups with no partial depenencies
  part_deps <- find_filtered_partial_deps(dependencies, df)
  if (length(part_deps) == 0)
    return(list(dependencies))
  split_then_remove(dependencies, df, part_deps, remove_part_deps)
}

remove_trans_deps <- function(dependencies, df) {
  UseMethod("remove_trans_deps")
}

#' @export
remove_trans_deps.Dependencies <- function(dependencies, df) {
  # Breaks up the dependency relations in dependencies into new groups of
  # relations so that there are no more transitive dependencies.
  #
  # Arguments:
  #     dependencies (Dependencies) : the dependencies to be split up
  #
  # Returns:
  #     new_groups (list[Dependencies]): list of new dependencies objects
  #     representing the new groups with no transitive depenencies
  trans_deps <- find_filtered_trans_deps(dependencies, df)
  if (length(trans_deps) == 0)
    return(list(dependencies))
  split_then_remove(dependencies, df, trans_deps, remove_trans_deps)
}

split_then_remove <- function(dependencies, df, unwanted_deps, remove_fn) {
  split_on <- find_most_comm(unwanted_deps, dependencies)
  new_deps <- split_on_dep(split_on, dependencies)
  c(
    remove_fn(new_deps[[1]], df),
    remove_fn(new_deps[[2]], df)
  )
}

tuple_relations <- function(dependencies) {
  UseMethod("tuple_relations")
}

#' @export
tuple_relations.Dependencies <- function(dependencies) {
  # Returns the relationships stored in self as a list.
  # Returns:
  #     relations (list[(list[str], str)]) : relations stored in self
  result <- list()
  for (i in seq_along(dependencies$dependencies)) {
    rhs <- names(dependencies$dependencies)[i]
    result <- c(
      result,
      lapply(dependencies$dependencies[[i]], \(lhs) list(lhs, rhs))
    )
  }
  result
}

find_candidate_keys <- function(dependencies) {
  UseMethod("find_candidate_keys")
}

#' @export
find_candidate_keys.Dependencies <- function(dependencies) {
# Returns all candidate keys in self. A candidate key is a minimal
# set of attributes whose closure is all attributes in the table.
  # Returns:
  #   cand_keys (list[set[str]]) : list of candidate keys for self

  all_attrs <- names(dependencies$dependencies)
  rhs_attrs <- all_attrs[
    lengths(dependencies$dependencies) > 0
  ]
  lhs_attrs <- unique(unlist(dependencies$dependencies, use.names = FALSE))
  lhs_only <- setdiff(lhs_attrs, rhs_attrs)
  rhs_only <- setdiff(rhs_attrs, lhs_attrs)
  lhs_and_rhs <- setdiff(all_attrs, union(lhs_only, rhs_only))
  rels <- tuple_relations(dependencies)
  if (length(rels) == 0)
    return(list())

  if (setequal(find_closure(rels, lhs_only), all_attrs))
    return(lhs_only)

  cand_keys <- list()

  for (i in seq_along(lhs_and_rhs)) {
    remaining <- setdiff(lhs_and_rhs, unlist(cand_keys))
    if (length(remaining) < i)
      break
    keys <- utils::combn(unlist(remaining), i, simplify = FALSE)
    for (key in keys) {
      lhs_only_or_key <- unique(union(lhs_only, key))
      if (
        setequal(
          unlist(find_closure(rels, lhs_only_or_key)),
          all_attrs
        ) &&
        !any(vapply(cand_keys, \(x) all(x %in% lhs_only_or_key), logical(1)))
      )
        cand_keys <- c(cand_keys, list(lhs_only_or_key))
    }
  }
  cand_keys
}
