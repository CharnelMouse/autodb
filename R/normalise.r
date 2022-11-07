#' Normalises dependency relationships
#'
#' Normalises the dependency relationships in dependencies into a database
#' scheme, using Bernstein's synthesis.
#'
#' Bernstein's synthesis is a synthesis algorithm for normalisation of a set of
#' dependencies into a set of relations that are in third normal form. This
#' implementation is based on the version given in the referenced paper.
#'
#' The implementation also includes a common additional step, to ensure that the
#' resulting decomposition is lossless, i.e. a relation satisfying the given
#' dependencies can be perfectly reconstructed from the relations given by the
#' decomposition. This is done by adding an additional relation, containing a
#' key for all the original attributes, if one is not already present.
#'
#' @param dependencies a list of functional dependencies, as given by
#'   \code{\link{flatten}}: each dependency is a list, contained one character
#'   vector for the left-hand size, and one unit-length character vector for the
#'   right-hand side.
#' @inheritParams autonorm
#'
#' @return A database scheme, represented by a named list of two lists of equal
#'   length, whose elements form pairs. Each pair represents a single relation
#'   scheme in the normalisation:
#'   \itemize{
#'     \item \code{attrs} elements contain the attributes present in the
#'     relation schemes, with attributes in keys given first.
#'     \item \code{keys} elements contain a list of the candidate keys for the
#'     relation schemes.
#'   }
#' @references
#' Bernstein YEAR, TITLE.
#' @export
normalise <- function(
  dependencies,
  remove_avoidable = FALSE,
  progress = 0L,
  progress_file = ""
) {
  report <- reporter(progress, progress_file)

  inter <- dependencies |>
    report$op(
      convert_to_vectors,
      "simplifying dependency format"
    ) |>
    convert_to_integer_attributes() |>
    sort_key_contents() |>
    report$op(
      remove_extraneous_attributes,
      "removing extraneuous components"
    ) |>
    remove_extraneous_dependencies() |>
    report$op(
      partition_dependencies,
      "partitioning dependencies"
    ) |>
    report$op(
      merge_equivalent_keys,
      "merging keys"
    ) |>
    report$op(
      remove_transitive_dependencies,
      "removing transitive dependencies"
    ) |>
    report$op(
      add_bijections,
      "re-adding bijections"
    ) |>
    report$op(
      construct_relation_schemes,
      "construction relation schemes"
    )
  if (remove_avoidable)
    inter <- inter |>
    report$op(
      remove_avoidable_attributes,
      "removing avoidable attributes"
    )
  inter |>
    report$op(
      convert_to_character_attributes,
      "converting to readable format"
    ) |>
    structure(class = c("database_scheme", "list"))
}

convert_to_vectors <- function(flat_dependencies) {
  list(
    determinant_sets = lapply(flat_dependencies$dependencies, `[[`, 1),
    dependents = vapply(flat_dependencies$dependencies, `[[`, character(1), 2),
    all_attrs = flat_dependencies$attrs
  )
}

convert_to_integer_attributes <- function(vecs) {
  vecs$determinant_sets <- lapply(vecs$determinant_sets, match, vecs$all_attrs)
  vecs$dependents <- match(vecs$dependents, vecs$all_attrs)
  vecs
}

sort_key_contents <- function(vecs) {
  vecs$determinant_sets <- lapply(vecs$determinant_sets, sort)
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
  new_det_sets <- vecs$determinant_sets
  old_deps <- NULL
  new_deps <- vecs$dependents
  while (!identical(old_deps, new_deps)) {
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
    unique_determinant_sets = unique_det_sets,
    all_attrs = vecs$all_attrs
  )
}

merge_equivalent_keys <- function(vecs) {
  partition_determinant_set <- vecs$partition_determinant_set
  partition_dependents <- vecs$partition_dependents
  unique_determinant_sets <- vecs$unique_determinant_sets

  partition_keys <- lapply(unique_determinant_sets, list)
  bijection_determinant_sets <- list()
  bijection_dependent_sets <- list()
  if (length(partition_dependents) >= 1) {
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
                c(partition_dependents[[n]], partition_dependents[[m]]),
                is.element,
                logical(1),
                c(key1, key2)
              )
              partition_dependents[[n]] <- unique(c(
                partition_dependents[[n]],
                partition_dependents[[m]]
              )[!obsolete])

              partition_determinant_set[[m]] <- integer()
              partition_dependents[[m]] <- integer()
              partition_keys[[m]] <- list()
            }
          }
        }
      }
    }
  }
  nonempty <- lengths(partition_dependents) > 0
  in_bijections <- partition_determinant_set %in% bijection_determinant_sets
  list(
    partition_determinant_set = partition_determinant_set[nonempty | in_bijections],
    partition_dependents = partition_dependents[nonempty | in_bijections],
    partition_keys = partition_keys[nonempty | in_bijections],
    bijection_determinant_sets = bijection_determinant_sets,
    bijection_dependent_sets = bijection_dependent_sets,
    all_attrs = vecs$all_attrs
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
  if (is.null(flat_partition_dependents))
    flat_partition_dependents <- integer()
  flat_groups <- rep(
    seq_along(vecs$partition_dependents),
    lengths(vecs$partition_dependents)
  )

  flat_bijection_determinant_sets <- rep(
    vecs$bijection_determinant_sets,
    lengths(vecs$bijection_dependent_sets)
  )
  flat_bijection_dependents <- unlist(vecs$bijection_dependent_sets)
  if (is.null(flat_bijection_dependents))
    flat_bijection_dependents <- integer()

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
    bijection_dependents = flat_bijection_dependents,
    all_attrs = vecs$all_attrs
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
    bijection_groups = vecs$partition_keys[lengths(vecs$partition_keys) > 1],
    all_attrs = vecs$all_attrs
  )
}

construct_relation_schemes <- function(vecs) {
  sorted_bijection_groups <- lapply(
    vecs$bijection_groups,
    \(bg) bg[keys_order(bg)]
  )
  primaries <- lapply(sorted_bijection_groups, `[[`, 1)
  attrs <- list()
  rel_keys <- list()
  if (length(vecs$flat_groups) > 0) {
    for (group_ind in seq_len(max(vecs$flat_groups))) {
      partition_index <- vecs$flat_groups == group_ind
      keys <- unique(vecs$flat_partition_determinant_set[partition_index])
      dependents <- unique(vecs$flat_partition_dependents[partition_index])
      nonprimes <- setdiff(dependents, unlist(keys))
      for (bi_grp_ind in seq_along(sorted_bijection_groups)) {
        grp <- sorted_bijection_groups[[bi_grp_ind]]
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
                # new_keys <- keys
                # new_keys[[key_el_ind]] <- new_keys[[key_el_ind]] |>
                #   setdiff(bijection_set) |>
                #   union(primary) |>
                #   sort()
                # vecs$flat_partition_determinant_set[partition_index] <-
                #   new_keys[match(
                #     vecs$flat_partition_determinant_set[partition_index],
                #     keys
                #   )]
                # keys <- new_keys
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
  }
  list(
    attrs = attrs,
    keys = rel_keys,
    all_attrs = vecs$all_attrs
  )
}

remove_avoidable_attributes <- function(vecs) {
  # Using the algorithm description in the original LTK paper, since I struggled
  # to understand the use of .-> in Maier's version.

  attrs <- vecs$attrs
  keys <- vecs$keys
  all_attrs <- vecs$all_attrs
  G <- synthesised_fds(attrs, keys)

  for (attr in rev(seq_along(all_attrs))) {
    for (relation in seq_along(attrs)) {
      relation_attrs <- sort(attrs[[relation]])
      if (!is.element(attr, relation_attrs))
        next
      K <- keys[[relation]]
      if (identical(K, list(relation_attrs)))
        next
      Kp <- Filter(\(k) !is.element(attr, unlist(k)), K)

      # check restorability
      if (length(Kp) == 0)
        next
      Gp <- G
      Gp[[relation]] <- Filter(\(fd) !is.element(attr, unlist(fd)), Gp[[relation]])
      X <- Kp[[1]]
      Gp_det_sets <- lapply(unlist(Gp, recursive = FALSE), `[[`, 1)
      Gp_deps <- vapply(unlist(Gp, recursive = FALSE), `[[`, integer(1), 2)
      if (!is.element(attr, find_closure(X, Gp_det_sets, Gp_deps)))
        next

      # check nonessentiality
      superfluous <- TRUE
      G_det_sets <- lapply(unlist(G, recursive = FALSE), `[[`, 1)
      G_deps <- vapply(unlist(G, recursive = FALSE), `[[`, integer(1), 2)
      for (X_i in setdiff(K, Kp)) {
        if (
          superfluous &&
          any(!is.element(
            relation_attrs,
            find_closure(X_i, Gp_det_sets, Gp_deps)
          ))
        ) {
          M <- find_closure(X_i, Gp_det_sets, Gp_deps)
          Mp <- setdiff(intersect(M, relation_attrs), attr)
          if (any(!is.element(
            relation_attrs,
            find_closure(Mp, G_det_sets, G_deps)
          )))
            superfluous <- FALSE
          else{
            # LTK paper version says to "insert into [Kp] any key of [relation]
            # contained in [Mp]". This doesn't work, e.g. key sets A<->B and AC
            # <-> BD would result in the latter losing B and only having key AC
            # remaining, when it should get AD. We therefore use a variation of
            # how a minimal replacement is found in Maier.
            replacement <- minimal_subset(Mp, relation_attrs, G_det_sets, G_deps)
            if (!is.element(list(replacement), Kp))
              Kp <- c(Kp, list(replacement))
          }
        }
      }
      if (superfluous) {
        stopifnot(all(unlist(Kp) %in% relation_attrs))
        keys[[relation]] <- Kp
        new_rel_attrs <- setdiff(relation_attrs, attr)
        attrs[[relation]] <- new_rel_attrs
        G[[relation]] <- relation_fds(new_rel_attrs, Kp)
      }
    }
  }
  vecs$keys <- keys
  vecs$attrs <- attrs
  vecs
}

synthesised_fds <- function(attrs, keys) {
  # returns nested list of functional dependencies directly represented in
  # relations
  Map(relation_fds, attrs, keys)
}

relation_fds <- function(attrs, keys) {
  # represented FDs for a single relation
  key_bijections <- list()
  key_indices <- seq_along(keys)
  for (lhs_index in key_indices) {
    for (rhs_index in key_indices[-lhs_index]) {
      key_bijections <- c(
        key_bijections,
        lapply(
          setdiff(keys[[rhs_index]], keys[[lhs_index]]),
          \(k) list(keys[[lhs_index]], k)
        )
      )
    }
  }
  nonprimes <- setdiff(attrs, unlist(keys))
  nonbijections <- unlist(
    lapply(
      keys,
      \(k) lapply(nonprimes, \(np) list(k, np))
    ),
    recursive = FALSE
  )
  c(key_bijections, nonbijections)
}

minimal_subset <- function(
  key,
  determines,
  determinant_sets,
  dependents
) {
  keep <- rep(TRUE, length(key))
  changed <- TRUE
  while (changed) {
    changed <- FALSE
    for (n in rev(seq_along(key)[keep])) {
      temp_keep <- keep
      temp_keep[n] <- FALSE
      temp_closure <- find_closure(
        key[temp_keep],
        determinant_sets,
        dependents
      )
      if (all(determines %in% temp_closure)) {
        keep <- temp_keep
        changed <- TRUE
      }
    }
  }
  sort(key[keep])
}

convert_to_character_attributes <- function(vecs) {
  vecs$attrs <- lapply(vecs$attrs, \(a) vecs$all_attrs[a])
  vecs$keys <- lapply(vecs$keys, \(ks) lapply(ks, \(k) vecs$all_attrs[k]))
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

#' @exportS3Method
print.database_scheme <- function(x, max = 10, ...) {
  n_relations <- length(x$attrs)
  cat(paste0(
    "database scheme with ",
    n_relations,
    " relation scheme",
    if (n_relations != 1) "s",
    "\n"
  ))
  for (n in seq_len(min(n_relations, max))) {
    cat(paste0("scheme ", n, ": ", toString(x$attrs[[n]]), "\n"))
    keys <- x$keys[[n]]
    n_keys <- length(keys)
    for (k in seq_len(min(n_keys, max))) {
      cat(paste0("  key ", k, ": ", toString(keys[[k]]), "\n"))
    }
    if (max < n_keys)
      cat("  ... and", n_keys - max, "other keys\n")
  }
  if (max < n_relations) {
    cat("... and", n_relations - max, "other schemes\n")
  }
  if (length(x$relationships) == 0)
    cat("no relationships\n")
  else {
    cat(paste("relationships:\n"))
    n_relationships <- length(x$relationships)
    for (r in seq_len(n_relationships)) {
      rel <- x$relationships[[r]]
      cat(paste0(rel[[1]][1], ".", rel[[2]], " -> ", rel[[1]][2], ".", rel[[2]], "\n"))
    }
    if (max < n_relationships)
      cat("... and", n_relationships - max, "other relationships\n")
  }
}
