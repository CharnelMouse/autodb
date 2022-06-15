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
  dependencies |>
    remove_extraneous_attributes() |>
    remove_extraneous_dependencies() |>
    partition_dependencies() |>
    merge_equivalent_keys() |>
    remove_transitive_dependencies() |>
    add_bijections() |>
    construct_relations()
}

remove_extraneous_dependencies <- function(relations) {
  old_rels <- NULL
  new_rels <- relations
  while (!identical(old_rels, new_rels)) {
    old_rels <- new_rels
    rem <- rep(FALSE, length(new_rels))
    for (n in seq_along(new_rels)) {
      LHS <- new_rels[[n]][[1]]
      RHS <- new_rels[[n]][[2]]
      other_rels <- new_rels[-n]
      other_rem <- rem[-n]
      closure <- find_closure(other_rels[!other_rem], LHS)
      rem[n] <- (RHS %in% closure)
    }
    new_rels <- new_rels[!rem]
  }
  new_rels
}

partition_dependencies <- function(relations) {
  LHS_list <- lapply(relations, `[[`, 1)
  LHSs <- unique(LHS_list)
  partition <- list()
  for (LHS in LHSs) {
    matches <- vapply(LHS_list, identical, logical(1), LHS)
    partition <- c(partition, list(relations[matches]))
  }
  list(
    relations = relations,
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
      RHSs <- unique(lapply(rels, `[[`, 2))
      all_attrs <- union(unlist(LHSs), unlist(RHSs))
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

tuple_relations <- function(dependencies) {
  UseMethod("tuple_relations")
}

#' @export
tuple_relations.Dependencies <- function(dependencies) {
  # Takes Dependencies and returns the dependencies in a flat list with
  # (parent table, parent attr, child table, child attr)
  # format. This throws away the index element, which I plan to remove anyway.
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
