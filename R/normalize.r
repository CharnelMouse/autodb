#' Normalizes dependency relationships
#'
#' Normalizes the dependency relationships in dependencies into new
#' groups by breaking up all partial and transitive dependencies.
#'
#' @param dependencies a list of functional dependencies, each composed of a
#'   list, with an element for the left-hand size and one for the right-hand
#'   side.
#'
#' @return a list of lists. Each such list contains two named elements:
#'   \code{attrs} contains the normalised dependencies, and \code{keys} contains
#'   a list of candidate keys.
#' @export
normalize_dependencies <- function(dependencies) {
  dependencies$dependencies |>
    convert_to_integer_attributes(dependencies$attrs) |>
    convert_to_vectors() |>
    remove_extraneous_attributes() |>
    remove_extraneous_dependencies() |>
    partition_dependencies() |>
    convert_to_list() |>
    merge_equivalent_keys() |>
    remove_transitive_dependencies() |>
    add_bijections() |>
    construct_relations() |>
    convert_to_character_attributes(dependencies$attrs)
}

convert_to_integer_attributes <- function(dependencies, attrs) {
  lapply(
    dependencies,
    \(fd) list(match(fd[[1]], attrs), match(fd[[2]], attrs))
  )
}

convert_to_vectors <- function(dependencies) {
  list(
    determinant_sets = lapply(dependencies, `[[`, 1),
    dependents = vapply(dependencies, `[[`, integer(1), 2)
  )
}

remove_extraneous_attributes <- function(vecs) {
  for (n in seq_along(vecs$dependents)) {
    lhs <- vecs$determinant_sets[[n]]
    rhs <- vecs$dependents[n]
    for (attr in lhs) {
      y_ <- setdiff(vecs$determinant_sets[[n]], attr)
      if (rhs %in% find_closure_vec(y_, vecs$determinant_sets, vecs$dependents)) {
        vecs$determinant_sets[[n]] <- y_
      }
    }
  }
  vecs
}

remove_extraneous_dependencies <- function(vecs) {
  old_det_sets <- NULL
  new_det_sets <- vecs$determinant_sets
  old_deps <- NULL
  new_deps <- vecs$dependents
  while (!identical(old_deps, new_deps)) {
    old_det_sets <- new_det_sets
    old_deps <- new_deps
    rem <- rep(FALSE, length(new_deps))
    for (n in seq_along(new_deps)) {
      det_set <- new_det_sets[[n]]
      dep <- new_deps[n]
      other_det_sets <- new_det_sets[-n]
      other_deps <- new_deps[-n]
      other_rem <- rem[-n]
      closure <- find_closure_vec(
        det_set,
        other_det_sets[!other_rem],
        other_deps[!other_rem]
      )
      rem[n] <- (dep %in% closure)
    }
    new_det_sets <- new_det_sets[!rem]
    new_deps <- new_deps[!rem]
  }
  vecs$determinant_sets <- new_det_sets
  vecs$dependents <- new_deps
  vecs
}

partition_dependencies <- function(vecs) {
  det_sets <- vecs$determinant_sets
  unique_det_sets <- unique(det_sets)
  partition_det_sets <- list()
  partition_deps <- list()
  for (det_set in unique_det_sets) {
    matches <- vapply(det_sets, identical, logical(1), det_set)
    partition_det_sets <- c(partition_det_sets, list(det_sets[matches]))
    partition_deps <- c(partition_deps, list(vecs$dependents[matches]))
  }
  list(
    determinant_sets = vecs$determinant_sets,
    dependents = vecs$dependents,
    partition_determinant_sets = partition_det_sets,
    partition_dependents = partition_deps,
    unique_determinant_sets = unique_det_sets
  )
}

convert_to_list <- function(vecs) {
  list(
    fds = Map(list, vecs$determinant_sets, vecs$dependents),
    partition = Map(
      \(det_sets, deps) Map(list, det_sets, deps),
      vecs$partition_determinant_sets,
      vecs$partition_dependents
    ),
    determinant_sets = vecs$unique_determinant_sets
  )
}

merge_equivalent_keys <- function(lst) {
  partition <- lst$partition
  determinant_sets <- lst$determinant_sets
  if (length(partition) <= 1)
    return(list(
      partition = partition,
      keys = lapply(determinant_sets, list),
      bijections = list()
    ))
  fds <- lst$fds

  keys <- lapply(determinant_sets, list)
  bijection_fds <- list()
  bijection_groups <- keys
  for (n in seq.int(length(partition) - 1)) {
    grp <- partition[[n]]
    if (length(grp) > 0) {
      LHS <- determinant_sets[[n]]
      key1 <- keys[[n]][[1]]
      for (m in (n + 1):length(partition)) {
        grp2 <- partition[[m]]
        if (length(grp2) > 0) {
          LHS2 <- determinant_sets[[m]]
          key2 <- keys[[m]][[1]]
          closure1 <- find_closure(fds, LHS)
          closure2 <- find_closure(fds, LHS2)

          if (all(key1 %in% closure2) && all(key2 %in% closure1)) {

            new_bijections <- list()
            new_bijections <- c(
              new_bijections,
              list(
                list(key1, key2),
                list(key2, key1)
              )
            )
            bijection_fds <- c(bijection_fds, new_bijections)
            bijection_groups[[n]] <- c(
              bijection_groups[[n]],
              bijection_groups[[m]]
            )

            obsolete <- vapply(
              partition[[m]],
              \(fd) fd[[2]] %in% c(key1, key2),
              logical(1)
            )
            partition[[n]] <- unique(c(
              partition[[n]],
              lapply(partition[[m]][!obsolete], \(fd) list(key1, fd[[2]]))
            ))
            keys[[n]] <- c(keys[[n]], keys[[m]])

            partition[[m]] <- list()
            bijection_groups[[m]] <- list()
          }
        }
      }
    }
  }
  nonempty <- lengths(partition) > 0
  list(
    partition = partition[nonempty],
    keys = keys[nonempty],
    bijections = bijection_fds,
    bijection_groups = bijection_groups[lengths(bijection_groups) > 1]
  )
}

remove_transitive_dependencies <- function(lst) {
  # DFD theorem 3: eliminate every functional dependency h in H such that the
  # right hand side is not in any of the group's keys, and is in the closure for
  # (H + J - {h}), where J is the bijections.
  # partition format: list[list[list[key, dependent]]]
  # keys format: list[list[attrs]], giving key list for each partition group
  # bijections: list[list[key1, key2]]
  flat_partition <- unlist(lst$partition, recursive = FALSE)
  flat_groups <- rep(seq_along(lst$partition), lengths(lst$partition))
  singular_bijections <- lapply(
    lst$bijections,
    \(b) lapply(b[[2]], \(r) list(b[[1]], r))
  ) |>
    unlist(recursive = FALSE)
  transitive <- rep(FALSE, length(flat_partition))
  attrs_in_keys <- lapply(lst$keys, \(k) unique(unlist(k)))
  for (n in seq_along(flat_partition)) {
    dependency <- flat_partition[[n]]
    RHS <- dependency[[2]]
    key_attrs <- unique(unlist(lst$keys[[flat_groups[n]]]))
    if (!is.element(RHS, key_attrs)) {
      closure_without <- find_closure(
        c(flat_partition[-n][!transitive[-n]], singular_bijections),
        key_attrs
      )
      if (is.element(RHS, closure_without))
        transitive[n] <- TRUE
    }
  }
  new_flat_partition <- flat_partition[!transitive]
  new_flat_groups <- flat_groups[!transitive]
  new_partition <- unname(split(new_flat_partition, new_flat_groups))
  list(
    partition = new_partition,
    keys = lst$keys,
    bijections = singular_bijections,
    bijection_groups = lst$bijection_groups
  )
}

add_bijections <- function(lst) {
  partition <- lst$partition
  keys <- lst$keys
  bijections <- lst$bijections
  bijection_groups <- lst$bijection_groups
  for (n in seq_along(keys)) {
    key_list <- keys[[n]]
    matches <- vapply(
      bijections,
      \(b) is.element(b[1], key_list) || is.element(b[2], key_list),
      logical(1)
    )
    partition[[n]] <- c(partition[[n]], bijections[matches])
    bijections <- bijections[!matches]
  }
  list(
    partition = partition,
    bijection_groups = bijection_groups
  )
}

construct_relations <- function(lst) {
  bijection_groups <- lst$bijection_groups
  primaries <- lapply(bijection_groups, choose_index)
  lapply(
    lst$partition,
    \(fds) {
      LHSs <- unique(lapply(fds, `[[`, 1))
      RHSs <- unique(lapply(fds, `[[`, 2))
      keys <- LHSs
      all_attrs <- union(unlist(keys), unlist(RHSs))
      nonkeys <- setdiff(all_attrs, unlist(keys))
      for (n in seq_along(bijection_groups)) {
        grp <- bijection_groups[[n]]
        primary <- primaries[[n]]

        if (!any(vapply(keys, \(k) all(primary %in% k), logical(1)))) {
          for (bijection_set in setdiff(grp, list(primary))) {
            for (m in seq_along(keys)) {
              if (all(bijection_set %in% keys[[m]])) {
                keys[[m]] <- setdiff(keys[[m]], bijection_set)
                keys[[m]] <- union(keys[[m]], primary)
                keys[[m]] <- keys[[m]][order(keys[[m]])]
              }
            }
          }
        }
        key_matches <- match(keys, grp)
        if (any(!is.na(key_matches))) {
          primary_loc <- match(list(primary), keys)
          keys <- c(list(primary), keys[-primary_loc])
        }

        for (bijection_set in setdiff(grp, list(primary))) {
          if (all(bijection_set %in% nonkeys)) {
            nonkeys <- setdiff(nonkeys, bijection_set)
            nonkeys <- union(nonkeys, primary)
          }
        }
      }
      nonkeys <- nonkeys[order(nonkeys)]
      list(attrs = union(unlist(keys), nonkeys), keys = keys)
    }
  )
}

convert_to_character_attributes <- function(lst, attrs) {
  lapply(
    lst,
    \(rel) {
      list(attrs = attrs[rel$attrs], keys = lapply(rel$keys, \(key) attrs[key]))
    }
  )
}

find_closure <- function(fds, attrs) {
  if (!is.integer(attrs))
    stop(paste("attr is", toString(class(attrs))))
  if (length(fds) == 0)
    return(attrs)
  for (n in seq_along(fds)) {
    fd <- fds[[n]]
    det_set <- fd[[1]]
    dep <- fd[[2]]
    if (length(dep) != 1)
      stop(paste(toString(dep), length(dep), toString(lengths(dep)), toString(r)))
    if (all(is.element(det_set, attrs))) {
      if (!is.element(dep, attrs))
        attrs <- c(attrs, dep)
      return(find_closure(fds[-n], attrs))
    }
  }
  attrs
}

find_closure_vec <- function(attrs, determinant_sets, dependents) {
  if (!is.integer(attrs))
    stop(paste("attr is", toString(class(attrs))))
  if (length(dependents) == 0)
    return(attrs)
  for (n in seq_along(dependents)) {
    det_set <- determinant_sets[[n]]
    dep <- dependents[n]
    if (length(dep) != 1)
      stop(paste(toString(dep), length(dep), toString(lengths(dep)), toString(r)))
    if (all(is.element(det_set, attrs))) {
      if (!is.element(dep, attrs))
        attrs <- c(attrs, dep)
      return(find_closure_vec(attrs, determinant_sets[-n], dependents[-n]))
    }
  }
  attrs
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
  # Replaces any composite keys with synthetic keys.
  # Currently not implemented, because I want to think through how to implement
  # it nicely, without removing other dependencies in the parent table.
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

flatten <- function(dependencies) {
  # Takes dependencies grouped by dependent attribute, and returns the
  # dependencies in a flat list with (parent table, parent attr, child table,
  # child attr) format.
  result <- list()
  for (i in seq_along(dependencies)) {
    rhs <- names(dependencies)[i]
    result <- c(
      result,
      lapply(dependencies[[i]], \(lhs) list(lhs, rhs))
    )
  }
  result
}
