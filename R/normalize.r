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
    merge_equivalent_keys() |>
    remove_transitive_dependencies() |>
    add_bijections() |>
    construct_relations() |>
    convert_to_character_attributes(dependencies$attrs) |>
    convert_to_list()
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
  partition_det_set <- list()
  partition_deps <- list()
  for (det_set in unique_det_sets) {
    matches <- vapply(det_sets, identical, logical(1), det_set)
    partition_det_set <- c(partition_det_set, list(det_set))
    partition_deps <- c(partition_deps, list(vecs$dependents[matches]))
  }
  list(
    determinant_sets = vecs$determinant_sets,
    dependents = vecs$dependents,
    partition_determinant_set = partition_det_set,
    partition_dependents = partition_deps,
    unique_determinant_sets = unique_det_sets
  )
}

merge_equivalent_keys <- function(vecs) {
  partition_determinant_set <- vecs$partition_determinant_set
  partition_dependents <- vecs$partition_dependents
  unique_determinant_sets <- vecs$unique_determinant_sets

  keys <- lapply(unique_determinant_sets, list)
  bijection_determinant_sets <- list()
  bijection_dependent_sets <- list()
  for (n in seq_len(length(partition_dependents) - 1)) {
    if (length(partition_dependents[[n]]) > 0) {
      LHS <- unique_determinant_sets[[n]]
      key1 <- keys[[n]][[1]]
      for (m in (n + 1):length(partition_dependents)) {
        if (length(partition_dependents[[m]]) > 0) {
          LHS2 <- unique_determinant_sets[[m]]
          key2 <- keys[[m]][[1]]
          closure1 <- find_closure_vec(
            LHS,
            vecs$determinant_sets,
            vecs$dependents
          )
          closure2 <- find_closure_vec(
            LHS2,
            vecs$determinant_sets,
            vecs$dependents
          )

          if (all(key1 %in% closure2) && all(key2 %in% closure1)) {

            bijection_determinant_sets <- c(
              bijection_determinant_sets,
              list(key1, key2)
            )
            bijection_dependent_sets <- c(
              bijection_dependent_sets,
              list(setdiff(key2, key1), setdiff(key1, key2))
            )
            keys[[n]] <- c(keys[[n]], keys[[m]])

            obsolete <- vapply(
              partition_dependents[[m]],
              is.element,
              logical(1),
              c(key1, key2)
            )
            partition_dependents[[n]] <- unique(c(
              partition_dependents[[n]],
              partition_dependents[[m]][!obsolete]
            ))

            partition_determinant_set[[m]] <- integer()
            partition_dependents[[m]] <- integer()
            keys[[m]] <- list()
          }
        }
      }
    }
  }
  nonempty <- lengths(partition_dependents) > 0
  list(
    partition_determinant_set = partition_determinant_set[nonempty],
    partition_dependents = partition_dependents[nonempty],
    keys = keys[nonempty],
    bijection_determinant_sets = bijection_determinant_sets,
    bijection_dependent_sets = bijection_dependent_sets,
    bijection_groups = keys[lengths(keys) > 1]
  )
}

remove_transitive_dependencies <- function(vecs) {
  # DFD theorem 3: eliminate every functional dependency h in H such that the
  # right hand side is not in any of the group's keys, and is in the closure for
  # (H + J - {h}), where J is the bijections.
  # partition format: list[list[list[key, dependent]]]
  # keys format: list[list[attrs]], giving key list for each partition group
  # bijections: list[list[key1, key2]]
  flat_partition_determinant_set <- rep(
    vecs$partition_determinant_set,
    lengths(vecs$partition_dependents)
  )
  flat_partition_dependents <- unlist(vecs$partition_dependents)
  flat_groups <- rep(
    seq_along(vecs$partition_dependents),
    lengths(vecs$partition_dependents)
  )

  flat_bijection_determinant_sets <- rep(
    vecs$bijection_determinant_sets,
    lengths(vecs$bijection_dependent_sets)
  )
  flat_bijection_dependents <- unlist(vecs$bijection_dependent_sets)

  transitive <- rep(FALSE, length(flat_partition_dependents))
  for (n in seq_along(flat_partition_dependents)) {
    RHS <- flat_partition_dependents[n]
    key_attrs <- unique(unlist(vecs$keys[[flat_groups[n]]]))
    if (!is.element(RHS, key_attrs)) {
      closure_without <- find_closure_vec(
        key_attrs,
        c(
          flat_partition_determinant_set[-n][!transitive[-n]],
          flat_bijection_determinant_sets
        ),
        c(
          flat_partition_dependents[-n][!transitive[-n]],
          flat_bijection_dependents
        )
      )
      if (is.element(RHS, closure_without))
        transitive[n] <- TRUE
    }
  }
  list(
    flat_partition_determinant_sets = flat_partition_determinant_set[!transitive],
    flat_partition_dependents = flat_partition_dependents[!transitive],
    flat_groups = flat_groups[!transitive],
    keys = vecs$keys,
    bijection_determinant_sets = flat_bijection_determinant_sets,
    bijection_dependents = flat_bijection_dependents,
    bijection_groups = vecs$bijection_groups
  )
}

add_bijections <- function(vecs) {
  flat_partition_determinant_set <- vecs$flat_partition_determinant_set
  flat_partition_dependents <- vecs$flat_partition_dependents
  flat_groups <- vecs$flat_groups
  bijection_determinant_sets <- vecs$bijection_determinant_sets
  bijection_dependents <- vecs$bijection_dependents
  for (n in seq_along(vecs$keys)) {
    key_list <- vecs$keys[[n]]
    determinant_set_matches <- vapply(
      bijection_determinant_sets,
      \(ds) is.element(list(ds), key_list),
      logical(1)
    )
    dependent_matches <- vapply(
      bijection_dependents,
      is.element,
      logical(1),
      key_list
    )
    matches <- determinant_set_matches | dependent_matches
    flat_partition_determinant_set <- c(
      flat_partition_determinant_set,
      bijection_determinant_sets[matches]
    )
    flat_partition_dependents <- c(
      flat_partition_dependents,
      bijection_dependents[matches]
    )
    flat_groups <- c(flat_groups, rep(n, sum(matches)))
    bijection_determinant_sets <- bijection_determinant_sets[!matches]
    bijection_dependents <- bijection_dependents[!matches]
  }
  list(
    flat_partition_determinant_set = flat_partition_determinant_set,
    flat_partition_dependents = flat_partition_dependents,
    flat_groups = flat_groups,
    bijection_groups = vecs$bijection_groups
  )
}

construct_relations <- function(vecs) {
  bijection_groups <- vecs$bijection_groups
  primaries <- lapply(bijection_groups, choose_index)
  attrs <- list()
  rel_keys <- list()
  for (n in seq_len(max(vecs$flat_groups))) {
    partition_index <- vecs$flat_groups == n
    keys <- unique(vecs$flat_partition_determinant_set[partition_index])
    dependents <- unique(vecs$flat_partition_dependents[partition_index])
    all_attrs <- union(unlist(keys), dependents)
    nonprimes <- setdiff(dependents, unlist(keys))
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
        if (all(bijection_set %in% nonprimes)) {
          nonprimes <- setdiff(nonprimes, bijection_set)
          nonprimes <- union(nonprimes, primary)
        }
      }
    }
    nonprimes <- nonprimes[order(nonprimes)]
    attrs <- c(attrs, list(union(unlist(keys), nonprimes)))
    rel_keys <- c(rel_keys, list(keys))
  }
  list(attrs = attrs, keys = rel_keys)
}

convert_to_character_attributes <- function(vecs, attrs) {
  vecs$attrs <- lapply(vecs$attrs, \(a) attrs[a])
  vecs$keys <- lapply(vecs$keys, \(ks) lapply(ks, \(k) attrs[k]))
  vecs
}

convert_to_list <- function(vecs) {
  Map(list, attrs = vecs$attrs, keys = vecs$keys)
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
