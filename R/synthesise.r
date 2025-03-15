#' Synthesise relation schemas from functional dependencies
#'
#' Synthesises the dependency relationships in dependencies into a database
#' schema satisfying at least third normal form, using Bernstein's synthesis.
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
#' As an additional optional step, schemas are checked for "avoidable"
#' attributes, that can be removed without loss of information.
#'
#' Constant attributes, i.e. those whose only determinant set is empty, get
#' assigned to a relation with no keys.
#'
#' Output is independent of the order of the input dependencies: schemas are
#' sorted according to their simplest keys.
#'
#' Schemas are sorted before ensuring for losslessness, or removing avoidable
#' attributes. As a result, neither optional step changes the order of the
#' schemas, and ensuring losslessness can only add an extra schema to the end of
#' the output vector.
#'
#' @inheritParams normalise
#'
#' @return A \code{\link{relation_schema}} object, containing the synthesised
#'   relation schemas.
#' @references
#' 3NF synthesis algorithm: Bernstein P. A. (1976) Synthesizing third normal
#' form relations from functional dependencies. *ACM Trans. Database Syst.*,
#' **1, 4**, 277--298.
#'
#' Removal of avoidable attributes: Ling T., Tompa F. W., Kameda T. (1981) An
#' improved third normal form for relational databases. *ACM Trans. Database
#' Syst.*, **6, 2**, 329--346.
#' @examples
#' # example 6.24 from The Theory of Relational Databases by David Maier
#' # A <-> B, AC -> D, AC -> E, BD -> C
#' deps <- functional_dependency(
#'   list(
#'     list("A", "B"),
#'     list("B", "A"),
#'     list(c("A", "C"), "D"),
#'     list(c("A", "C"), "E"),
#'     list(c("B", "D"), "C")
#'   ),
#'   attrs_order = c("A", "B", "C", "D", "E")
#' )
#' synthesise(deps, remove_avoidable = FALSE)
#' synthesise(deps, remove_avoidable = TRUE)
#' @export
synthesise <- function(
  dependencies,
  ensure_lossless = TRUE,
  reduce_attributes = TRUE,
  remove_avoidable = FALSE,
  constants_name = "constants",
  progress = FALSE,
  progress_file = ""
) {
  report <- reporter(progress, progress_file)

  inter <- dependencies |>
    report$op(
      if (reduce_attributes)
        remove_extraneous
      else
        remove_extraneous_dependencies,
      "removing extraneous components"
    ) |>
    report$op(
      convert_to_vectors,
      "simplifying dependency format"
    ) |>
    convert_to_integer_attributes() |>
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
      construct_relation_schemas,
      "constructing relation schemas"
    )
  ord <- keys_order(lapply(inter$keys, \(ks) ks[[1]]))
  inter$attrs <- inter$attrs[ord]
  inter$keys <- inter$keys[ord]
  if (remove_avoidable)
    inter <- inter |>
    report$op(
      remove_avoidable_attributes,
      "removing avoidable attributes"
    )
  inter <- inter |>
    report$op(
      convert_to_character_attributes,
      "converting to readable format"
    )
  relation_names <- vapply(
    inter$keys,
    \(keys) name_dataframe(keys[[1]]),
    character(1)
  )
  relation_names[nchar(relation_names) == 0] <- constants_name
  if (!missing(constants_name) && sum(relation_names == constants_name) > 1)
    warning("constants_name appears in generated relation names, and will be changed to keep relation names unique")
  relation_names <- make.names(relation_names, unique = TRUE)
  stopifnot(!anyDuplicated(relation_names))
  schema <- relation_schema(
    stats::setNames(
      Map(list, inter$attrs, inter$keys),
      relation_names
    ),
    inter$attrs_order
  )
  if (ensure_lossless)
    schema <- ensure_lossless(schema)
  schema
}

convert_to_vectors <- function(flat_dependencies) {
  list(
    determinant_sets = detset(flat_dependencies),
    dependants = dependant(flat_dependencies),
    attrs_order = attrs_order(flat_dependencies)
  )
}

convert_to_integer_attributes <- function(vecs) {
  vecs$determinant_sets <- lapply(vecs$determinant_sets, match, vecs$attrs_order)
  vecs$dependants <- match(vecs$dependants, vecs$attrs_order)
  vecs
}

sort_key_contents <- function(vecs) {
  vecs$determinant_sets <- lapply(vecs$determinant_sets, sort)
  vecs
}

remove_extraneous <- function(deps) {
  deps |>
    remove_extraneous_attributes() |>
    remove_extraneous_dependencies()
}

remove_extraneous_attributes <- function(deps) {
  dts <- detset(deps)
  dps <- dependant(deps)
  for (n in seq_along(deps)) {
    lhs <- dts[[n]]
    rhs <- dps[[n]]
    for (attr in lhs) {
      y_ <- setdiff(dts[[n]], attr)
      if (rhs %in% find_closure(y_, dts, dps)) {
        dts[[n]] <- y_
      }
    }
  }
  detset(deps) <- dts
  deps
}

sort_dependencies <- function(vecs) {
  ord <- order(keys_rank(vecs$determinant_sets), vecs$dependants)
  vecs$determinant_sets <- vecs$determinant_sets[ord]
  vecs$dependants <- vecs$dependants[ord]
  vecs
}

remove_extraneous_dependencies <- function(fds) {
  det_inds <- lapply(detset(fds), \(k) match(k, attrs_order(fds)))
  dep_inds <- match(dependant(fds), attrs_order(fds))
  ord <- order(keys_rank(det_inds), dep_inds)
  inv_ord <- order(ord)

  new_det_sets <- detset(fds)[ord]
  old_deps <- NULL
  new_deps <- dependant(fds)[ord]
  main_rem <- rep(FALSE, length(new_deps))
  while (!identical(old_deps, new_deps)) {
    old_deps <- new_deps
    rem_ind <- which(!main_rem)
    rem <- rep(FALSE, length(new_deps))
    for (n in rev(seq_along(new_deps))) {
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
    main_rem[rem_ind] <- rem
  }
  stopifnot(identical(
    new_det_sets,
    detset(fds)[ord][!main_rem]
  ))
  fds[!main_rem[inv_ord]]
}

partition_dependencies <- function(vecs) {
  det_sets <- vecs$determinant_sets
  unique_det_sets <- unique(det_sets)
  det_set_matches <- match(det_sets, unique_det_sets)
  partition_deps <- unname(split(vecs$dependants, det_set_matches))
  c(
    vecs,
    list(
      partition_determinant_set = unique_det_sets,
      partition_dependants = partition_deps
    )
  )
}

merge_equivalent_keys <- function(vecs) {
  partition_determinant_set <- vecs$partition_determinant_set
  partition_dependants <- vecs$partition_dependants

  partition_keys <- lapply(partition_determinant_set, list)
  bijection_determinant_sets <- list()
  bijection_dependant_sets <- list()
  closures <- lapply(
    partition_determinant_set,
    find_closure,
    vecs$determinant_sets,
    vecs$dependants
  )
  if (length(partition_determinant_set) == 0) {
    merged_partition_keys <- list()
    merged_partition_dependants <- list()
    kept <- logical()
    bijection_determinant_sets2 <- list()
    bijection_dependant_sets2 <- list()
  }else{
    included <- outer(
      partition_determinant_set,
      closures,
      Vectorize(\(x, y) all(is.element(x, y)))
    )
    merge_groups <- unique(apply(
      included & t(included),
      1,
      which,
      simplify = FALSE
    ))
    merged_partition_keys <- lapply(
      merge_groups,
      \(grp) Reduce(union, partition_keys[grp]) |> (\(x) x[keys_order(x)])()
    )
    merged_partition_dependants <- lapply(
      merge_groups,
      \(grp) sort(Reduce(union, partition_dependants[grp]))
    )
    merged_partition_dependants <- Map(
      setdiff,
      merged_partition_dependants,
      lapply(merged_partition_keys, unlist)
    )
    kept <- !duplicated(apply(
      included & t(included),
      1,
      which,
      simplify = FALSE
    ))
  }
  if (length(partition_dependants) >= 1) {
    for (n in setdiff(
      vapply(merge_groups, `[[`, integer(1), 1),
      length(partition_dependants)
    )) {
      for (m in which((included & t(included))[n, ])[-1]) {
        stopifnot(included[n, m] && included[m, n])
        key1 <- partition_determinant_set[[n]]
        key2 <- partition_determinant_set[[m]]
        bijection_determinant_sets <- c(
          bijection_determinant_sets,
          list(key1, key2)
        )
        bijection_dependant_sets <- c(
          bijection_dependant_sets,
          list(setdiff(key2, key1), setdiff(key1, key2))
        )
      }
    }
  }
  list(
    partition_determinant_set = partition_determinant_set[kept],
    partition_dependants = merged_partition_dependants,
    partition_keys = merged_partition_keys,
    bijection_determinant_sets = bijection_determinant_sets,
    bijection_dependant_sets = bijection_dependant_sets,
    attrs_order = vecs$attrs_order
  )
}

remove_transitive_dependencies <- function(vecs) {
  # DFD theorem 3: eliminate every functional dependency h in H such that the
  # right hand side is not in any of the group's keys, and is in the closure for
  # (H + J - {h}), where J is the bijections.
  # partition format: list[list[list[key, dependant]]]
  # keys format: list[list[attrs]], giving key list for each partition group
  # bijections: list[list[key1, key2]]
  flat_partition_determinant_set <- rep(
    vecs$partition_determinant_set,
    lengths(vecs$partition_dependants)
  )
  flat_partition_dependants <- unlist(vecs$partition_dependants)
  if (is.null(flat_partition_dependants))
    flat_partition_dependants <- integer()
  flat_groups <- rep(
    seq_along(vecs$partition_dependants),
    lengths(vecs$partition_dependants)
  )

  flat_bijection_determinant_sets <- rep(
    vecs$bijection_determinant_sets,
    lengths(vecs$bijection_dependant_sets)
  )
  flat_bijection_dependants <- unlist(vecs$bijection_dependant_sets)
  if (is.null(flat_bijection_dependants))
    flat_bijection_dependants <- integer()

  transitive <- rep(FALSE, length(flat_partition_dependants))
  for (n in seq_along(flat_partition_dependants)) {
    RHS <- flat_partition_dependants[n]
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
          flat_partition_dependants[-n][!transitive[-n]],
          flat_bijection_dependants
        )
      )
      if (is.element(RHS, closure_without))
        transitive[n] <- TRUE
    }
  }
  list(
    flat_partition_determinant_sets = flat_partition_determinant_set[!transitive],
    flat_partition_dependants = flat_partition_dependants[!transitive],
    flat_groups = flat_groups[!transitive],
    partition_keys = vecs$partition_keys,
    bijection_determinant_sets = flat_bijection_determinant_sets,
    bijection_dependants = flat_bijection_dependants,
    attrs_order = vecs$attrs_order
  )
}

add_bijections <- function(vecs) {
  flat_partition_determinant_set <- vecs$flat_partition_determinant_set
  flat_partition_dependants <- vecs$flat_partition_dependants
  flat_groups <- vecs$flat_groups
  bijection_determinant_sets <- vecs$bijection_determinant_sets
  bijection_dependants <- vecs$bijection_dependants
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
    flat_partition_dependants <- c(
      flat_partition_dependants,
      bijection_dependants[matches]
    )
    flat_groups <- c(flat_groups, rep(n, sum(matches)))
    bijection_determinant_sets <- bijection_determinant_sets[!matches]
    bijection_dependants <- bijection_dependants[!matches]
  }
  stopifnot(length(bijection_determinant_sets) == 0)
  list(
    flat_partition_determinant_set = flat_partition_determinant_set,
    flat_partition_dependants = flat_partition_dependants,
    flat_groups = flat_groups,
    bijection_groups = vecs$partition_keys[lengths(vecs$partition_keys) > 1],
    attrs_order = vecs$attrs_order
  )
}

construct_relation_schemas <- function(vecs) {
  sorted_bijection_groups <- lapply(
    vecs$bijection_groups,
    \(bg) bg[keys_order(bg)]
  )
  primaries <- lapply(sorted_bijection_groups, `[[`, 1)
  attrs <- list()
  rel_keys <- list()
  if (length(vecs$flat_groups) > 0) {
    group_bi_grp_ind <- vapply(
      seq_len(max(vecs$flat_groups)),
      \(n) {
        keys <- unique(vecs$flat_partition_determinant_set[vecs$flat_groups == n])
        sorted_keys <- keys[keys_order(keys)]
        match(list(sorted_keys), sorted_bijection_groups)
      },
      integer(1)
    )
    stopifnot(identical(
      sort(group_bi_grp_ind[!is.na(group_bi_grp_ind)]),
      seq_along(vecs$bijection_groups)
    ))
    for (group_ind in seq_len(max(vecs$flat_groups))) {
      partition_index <- vecs$flat_groups == group_ind
      keys <- unique(vecs$flat_partition_determinant_set[partition_index])
      dependants <- unique(vecs$flat_partition_dependants[partition_index])
      nonprimes <- setdiff(dependants, unlist(keys))

      # try simplifying keys using other bijection sets
      # this is not replicated by removing avoidable attributes
      # if dependencies aren't complete, can result in duplicated keys,
      # so we have to use unique()
      # I'm not sure these simplifications actually get used, unless
      # the given FDs aren't complete for each dependant.
      # skip own bijection group
      other_bijection_groups <- setdiff(
        seq_along(sorted_bijection_groups),
        group_bi_grp_ind[group_ind]
      )
      for (bi_grp_ind in other_bijection_groups) {
        grp <- sorted_bijection_groups[[bi_grp_ind]]
        primary <- primaries[[bi_grp_ind]]
        nonprimary_bijection_set <- setdiff(grp, list(primary))

        # use bijection set to simplify if its primary isn't in the group
        # I think the intention here is more like "if the set isn't
        # the one that defines the group", which will break if the group's
        # primary gets changed to something else by an earlier bijection set.
        primary_not_in_keys <- !any(vapply(
          keys,
          \(k) all(primary %in% k),
          logical(1)
        ))
        if (primary_not_in_keys) {
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
        # above step can currently result in duplicates
        keys <- unique(keys)
        key_matches <- match(keys, grp)
        # replace any keys within the bijection group with its primary
        if (any(!is.na(key_matches))) {
          primary_loc <- vapply(keys, identical, logical(1), primary)
          keys <- c(list(primary), keys[!primary_loc])
        }

        # if replace any keys within the nonprime attributes with the primary
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
      attrs_order <- union(unlist(sorted_keys), nonprimes)
      attrs <- c(attrs, list(attrs_order))
      rel_keys <- c(rel_keys, list(sorted_keys))
    }
  }
  list(
    attrs = attrs,
    keys = rel_keys,
    attrs_order = vecs$attrs_order
  )
}

remove_avoidable_attributes <- function(vecs) {
  # Using the algorithm description in the original LTK paper, since I struggled
  # to understand the use of .-> in Maier's version.

  attrs <- vecs$attrs
  keys <- vecs$keys
  attrs_order <- vecs$attrs_order
  G <- synthesised_fds(attrs, keys)

  for (attr in rev(seq_along(attrs_order))) {
    for (relation in seq_along(attrs)) {
      relation_attrs <- attrs[[relation]]
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
            replacement <- sort(minimal_subset(
              Mp,
              relation_attrs,
              G_det_sets,
              G_deps
            ))
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
  vecs$attrs <- Map(
    \(as, ks) c(unique(unlist(ks)), setdiff(as, unlist(ks))),
    attrs,
    keys
  )
  vecs
}

ensure_lossless <- function(schema) {
  attrs_order <- attrs_order(schema)
  attrs <- attrs(schema)
  keys <- keys(schema)
  relation_names <- names(schema)

  G <- synthesised_fds(attrs, keys)
  G_det_sets <- lapply(unlist(G, recursive = FALSE), `[[`, 1)
  G_deps <- vapply(unlist(G, recursive = FALSE), `[[`, character(1), 2)
  primaries <- lapply(keys, `[[`, 1)
  closures <- lapply(primaries, find_closure, G_det_sets, G_deps)
  if (any(vapply(closures, setequal, logical(1), attrs_order)))
    return(schema)

  new_key <- minimal_subset(attrs_order, attrs_order, G_det_sets, G_deps)
  attrs <- c(attrs, list(new_key))
  keys <- c(keys, list(list(new_key)))
  new_name <- paste(new_key, collapse = "_")
  if (nchar(new_name) == 0L)
    new_name <- "constants"
  relation_names <- c(relation_names, new_name)
  stopifnot(sum(nchar(relation_names) == 0L) <= 1L)
  relation_names[nchar(relation_names) == 0L] <- "empty"
  relation_names <- make.names(relation_names, unique = TRUE)
  c(
    schema,
    relation_schema(
      stats::setNames(
        list(list(new_key, list(new_key))),
        relation_names[length(relation_names)]
      ),
      attrs_order
    )
  )
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
    key_bijections <- c(
      key_bijections,
      lapply(
        setdiff(unlist(keys[key_indices[-lhs_index]]), keys[[lhs_index]]),
        \(k) list(keys[[lhs_index]], k)
      )
    )
  }
  nonprimes <- setdiff(attrs, unlist(keys))
  nonbijections <- unlist(
    lapply(
      keys,
      \(k) lapply(nonprimes, \(np) list(k, np))
    ),
    recursive = FALSE
  )
  res <- c(key_bijections, nonbijections)
  stopifnot(!anyDuplicated(res))
  res
}

minimal_subset <- function(
  key,
  determines,
  determinant_sets,
  dependants
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
        dependants
      )
      if (all(determines %in% temp_closure)) {
        keep <- temp_keep
        changed <- TRUE
      }
    }
  }
  key[keep]
}

convert_to_character_attributes <- function(vecs) {
  vecs$attrs <- lapply(vecs$attrs, \(a) vecs$attrs_order[a])
  vecs$keys <- lapply(vecs$keys, \(ks) lapply(ks, \(k) vecs$attrs_order[k]))
  vecs
}

name_dataframe <- function(index) {
  paste(index, collapse = "_")
}

find_closure <- function(attrs, determinant_sets, dependants) {
  if (length(dependants) == 0)
    return(attrs)
  checked <- rep(FALSE, length(dependants))
  change <- TRUE
  while (change) {
    change <- FALSE
    for (n in seq_along(dependants)[!checked]) {
      det_set <- determinant_sets[[n]]
      dep <- dependants[n]
      if (length(dep) != 1)
        stop(paste(toString(dep), length(dep), toString(lengths(dep)), toString(dep)))
      if (all(is.element(det_set, attrs))) {
        checked[n] <- TRUE
        if (!is.element(dep, attrs))
          change <- TRUE
          attrs <- c(attrs, dep)
      }
    }
  }
  attrs
}

find_closure_with_used <- function(attrs, determinant_sets, dependants) {
  if (length(dependants) == 0)
    return(list(attrs, integer()))
  checked <- rep(FALSE, length(dependants))
  change <- TRUE
  ordered_use <- integer()
  while (change) {
    change <- FALSE
    for (n in seq_along(dependants)[!checked]) {
      det_set <- determinant_sets[[n]]
      dep <- dependants[n]
      if (length(dep) != 1)
        stop(paste(toString(dep), length(dep), toString(lengths(dep)), toString(dep)))
      if (all(is.element(det_set, attrs))) {
        checked[n] <- TRUE
        if (!is.element(dep, attrs)) {
          change <- TRUE
          attrs <- c(attrs, dep)
          ordered_use <- c(ordered_use, n)
        }
      }
    }
  }
  list(attrs, ordered_use)
}

keys_order_same_lengths <- function(keys) {
  len <- length(keys[[1]])
  stopifnot(all(lengths(keys) == len))
  if (len == 0)
    return(seq_along(keys))
  els_by_place <- do.call(Map, unname(c(c, keys)))
  do.call(order, unname(els_by_place))
}

keys_order <- function(keys) {
  if (length(keys) == 0L)
    return(integer())
  lens <- lengths(keys)
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
  order(lens)[flat_order]
}

keys_rank_same_lengths <- function(keys) {
  len <- length(keys[[1]])
  stopifnot(all(lengths(keys) == len))
  if (len == 0)
    return(rep((length(keys) + 1)/2, length(keys)))
  els_by_place <- do.call(Map, unname(c(c, keys)))
  ranks <- rep((length(keys) + 1)/2, length(keys))
  for (n in seq_len(len)) {
    ur <- unique(ranks)
    if (length(ur) == length(keys))
      break
    vals <- els_by_place[[n]]
    newranks <- ranks
    for (r in ur) {
      rs <- ranks == r
      rlen <- sum(rs)
      rv <- rank(vals[rs])
      newranks[rs] <- ranks[rs] + rv - (rlen + 1)/2
    }
    ranks <- newranks
  }
  ranks
}

keys_rank <- function(keys) {
  if (length(keys) == 0L)
    return(integer())
  lens <- lengths(keys)
  rank_within_lengths <- unname(tapply(
    keys,
    lens,
    keys_rank_same_lengths,
    simplify = FALSE
  ))
  cum_lengths <- cumsum(lengths(rank_within_lengths))
  starts <- c(0L, cum_lengths[-length(cum_lengths)])
  unsplit(Map("+", rank_within_lengths, starts), lens)
}
