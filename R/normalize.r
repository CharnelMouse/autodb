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
    convert_to_vectors() |>
    convert_to_integer_attributes(dependencies$attrs) |>
    remove_extraneous_attributes() |>
    remove_extraneous_dependencies() |>
    partition_dependencies() |>
    merge_equivalent_keys() |>
    remove_transitive_dependencies() |>
    add_bijections() |>
    construct_relations() |>
    convert_to_character_attributes(dependencies$attrs)
}

convert_to_vectors <- function(dependencies) {
  list(
    determinant_sets = lapply(dependencies, `[[`, 1),
    dependents = vapply(dependencies, `[[`, character(1), 2)
  )
}

convert_to_integer_attributes <- function(vecs, attrs) {
  vecs$determinant_sets <- lapply(vecs$determinant_sets, match, attrs)
  vecs$dependents <- match(vecs$dependents, attrs)
  vecs
}

remove_extraneous_attributes <- function(vecs) {
  for (n in seq_along(vecs$dependents)) {
    lhs <- vecs$determinant_sets[[n]]
    rhs <- vecs$dependents[n]
    for (attr in lhs) {
      y_ <- setdiff(vecs$determinant_sets[[n]], attr)
      if (rhs %in% find_closure(y_, vecs$determinant_sets, vecs$dependents)) {
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
      closure <- find_closure(
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

  partition_keys <- lapply(unique_determinant_sets, list)
  bijection_determinant_sets <- list()
  bijection_dependent_sets <- list()
  for (n in seq_len(length(partition_dependents) - 1)) {
    if (length(partition_dependents[[n]]) > 0) {
      LHS <- unique_determinant_sets[[n]]
      key1 <- partition_keys[[n]][[1]]
      for (m in (n + 1):length(partition_dependents)) {
        if (length(partition_dependents[[m]]) > 0) {
          LHS2 <- unique_determinant_sets[[m]]
          key2 <- partition_keys[[m]][[1]]
          closure1 <- find_closure(
            LHS,
            vecs$determinant_sets,
            vecs$dependents
          )
          closure2 <- find_closure(
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
            partition_keys[[n]] <- c(partition_keys[[n]], partition_keys[[m]])

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
            partition_keys[[m]] <- list()
          }
        }
      }
    }
  }
  nonempty <- lengths(partition_dependents) > 0
  list(
    partition_determinant_set = partition_determinant_set[nonempty],
    partition_dependents = partition_dependents[nonempty],
    partition_keys = partition_keys[nonempty],
    bijection_determinant_sets = bijection_determinant_sets,
    bijection_dependent_sets = bijection_dependent_sets
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
    keys <- vecs$partition_keys[[flat_groups[n]]]
    key_attrs <- unique(unlist(keys))
    if (!is.element(RHS, key_attrs)) {
      closure_without <- find_closure(
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
    partition_keys = vecs$partition_keys,
    bijection_determinant_sets = flat_bijection_determinant_sets,
    bijection_dependents = flat_bijection_dependents
  )
}

add_bijections <- function(vecs) {
  flat_partition_determinant_set <- vecs$flat_partition_determinant_set
  flat_partition_dependents <- vecs$flat_partition_dependents
  flat_groups <- vecs$flat_groups
  bijection_determinant_sets <- vecs$bijection_determinant_sets
  bijection_dependents <- vecs$bijection_dependents
  for (n in seq_along(vecs$partition_keys)) {
    keys <- vecs$partition_keys[[n]]
    matches <- vapply(
      bijection_determinant_sets,
      \(ds) is.element(list(ds), keys),
      logical(1)
    )
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
  stopifnot(length(bijection_determinant_sets) == 0)
  list(
    flat_partition_determinant_set = flat_partition_determinant_set,
    flat_partition_dependents = flat_partition_dependents,
    flat_groups = flat_groups,
    bijection_groups = vecs$partition_keys[lengths(vecs$partition_keys) > 1]
  )
}

construct_relations <- function(vecs) {
  bijection_groups <- vecs$bijection_groups
  primaries <- lapply(bijection_groups, choose_index)
  attrs <- list()
  rel_keys <- list()
  for (group_ind in seq_len(max(vecs$flat_groups))) {
    partition_index <- vecs$flat_groups == group_ind
    keys <- unique(vecs$flat_partition_determinant_set[partition_index])
    dependents <- unique(vecs$flat_partition_dependents[partition_index])
    nonprimes <- setdiff(dependents, unlist(keys))
    for (bi_grp_ind in seq_along(bijection_groups)) {
      grp <- bijection_groups[[bi_grp_ind]]
      primary <- primaries[[bi_grp_ind]]
      nonprimary_bijection_set <- setdiff(grp, list(primary))

      if (!any(vapply(keys, \(k) all(primary %in% k), logical(1)))) {
        for (bijection_set in nonprimary_bijection_set) {
          for (key_el_ind in seq_along(keys)) {
            if (all(bijection_set %in% keys[[key_el_ind]])) {
              keys[[key_el_ind]] <- keys[[key_el_ind]] |>
                setdiff(bijection_set) |>
                union(primary) |>
                sort()
            }
          }
        }
      }
      key_matches <- match(keys, grp)
      if (any(!is.na(key_matches))) {
        primary_loc <- match(list(primary), keys)
        keys <- c(list(primary), keys[-primary_loc])
      }

      for (bijection_set in nonprimary_bijection_set) {
        if (all(bijection_set %in% nonprimes)) {
          nonprimes <- setdiff(nonprimes, bijection_set)
          nonprimes <- union(nonprimes, primary)
        }
      }
    }
    key_ord <- keys_order(keys)
    sorted_keys <- keys[key_ord]
    nonprimes <- nonprimes[order(nonprimes)]
    all_attrs <- union(unlist(sorted_keys), nonprimes)
    attrs <- c(attrs, list(all_attrs))
    rel_keys <- c(rel_keys, list(sorted_keys))
  }
  list(attrs = attrs, keys = rel_keys)
}

convert_to_character_attributes <- function(vecs, attrs) {
  vecs$attrs <- lapply(vecs$attrs, \(a) attrs[a])
  vecs$keys <- lapply(vecs$keys, \(ks) lapply(ks, \(k) attrs[k]))
  vecs
}

find_closure <- function(attrs, determinant_sets, dependents) {
  if (!is.integer(attrs))
    stop(paste("attr is", toString(class(attrs))))
  if (length(dependents) == 0)
    return(attrs)
  for (n in seq_along(dependents)) {
    det_set <- determinant_sets[[n]]
    dep <- dependents[n]
    if (length(dep) != 1)
      stop(paste(toString(dep), length(dep), toString(lengths(dep)), toString(dep)))
    if (all(is.element(det_set, attrs))) {
      if (!is.element(dep, attrs))
        attrs <- c(attrs, dep)
      return(find_closure(attrs, determinant_sets[-n], dependents[-n]))
    }
  }
  attrs
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

keys_order_same_lengths <- function(keys) {
  len <- length(keys[[1]])
  stopifnot(all(lengths(keys) == len))
  if (len == 0)
    return(seq_len(keys))
  els_by_place <- do.call(Map, c(c, keys))
  do.call(order, els_by_place)
}

keys_order <- function(keys) {
  lens <- lengths(keys)
  length_order <- order(lens)
  order_within_lengths <- tapply(
    keys,
    lens,
    keys_order_same_lengths,
    simplify = FALSE
  )
  cum_lengths <- cumsum(lengths(order_within_lengths))
  starts <- c(0L, cum_lengths[-length(cum_lengths)])
  flat_order <- unlist(order_within_lengths, use.names = FALSE) +
    rep(starts, lengths(order_within_lengths))
  length_order[flat_order]
}
