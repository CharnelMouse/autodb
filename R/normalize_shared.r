find_filtered_partial_deps <- function(dependencies, df) {
  find_partial_deps(dependencies) |>
    filter(df)
}

find_filtered_trans_deps <- function(dependencies, df) {
  find_trans_deps(dependencies) |>
    filter(df)
}

find_most_comm <- function(deps, dependencies, df = NA) {
  # Given a list of dependency relations, finds the most common set of
  # LHS attributes. If more than one LHS set occurs the same amount of
  # times, chooses the set with the least number of attributes.
  #
  # Arguments:
  #     deps (list[(set[str], str)]) : list of tuples representing relations
  #     where the lhs is a set of attribute names, and the rhs is an attribute.
  #
  # Returns:
  #     most_comm (set[str]) : the most common lhs set of attributes
  positions <- list()
  scores <- numeric()

  # build class-agnostic frequency table, most common first (priority queue)
  for (dep in deps) {
    lhs <- dep[[1]]
    rhs <- dep[[2]]
    lhs_position <- match(list(lhs), positions)
    if (!is.na(lhs_position)) {
      score <- scores[lhs_position] + 1
      while (lhs_position != 1 && scores[lhs_position - 1] < score) {
        positions[[lhs_position]] <- positions[[lhs_position - 1]]
        scores[lhs_position] <- scores[lhs_position - 1]
        lhs_position <- lhs_position - 1
      }
      positions[[lhs_position]] <- lhs
      scores[lhs_position] <- score
    }else{
      positions <- c(positions, list(lhs))
      scores <- c(scores, 1)
    }
  }
  # if multiple most-frequent, choose one with shortest length etc.
  head_matches_sh <- scores == scores[1]
  options <- positions[head_matches_sh]
  max_lhs <- choose_index(options, df)

  for (i in seq_along(max_lhs)) {
    for (key in get_prim_key(dependencies)) {
      if (equiv_attrs(dependencies, max_lhs[i], key))
        max_lhs[i] <- key
    }
  }
  max_lhs
}

split_on_dep <- function(lhs_dep, dependencies) {
  # Given the LHS attributes of a dependency, breaks up the dependency
  # relations in dependencies into two groups so that the LHS given is
  # the primary key of the new group. The old group keeps the same
  # primary key.
  #
  # Arguments:
  #     lhs_dep (list[str]) : set of attributes to be the new group's
  #     primary key
  #     dependencies (Dependencies) : dependency relations to be split up
  #
  # Returns:
  #     new_groups ((Dependencies, Dependencies)) : the new groups
  old_deps <- dependencies$dependencies
  new_rhs <- list()
  new_deps <- old_deps[lhs_dep]

  # Move dependants that rely on any part of lhs_dep to new
  for (rhs in names(old_deps)) {
    lhs_subset_lhs_dep <- vapply(
      old_deps[[rhs]],
      \(lhs) all(lhs %in% lhs_dep),
      logical(1)
    )
    if (any(lhs_subset_lhs_dep)) {
      new_deps[[rhs]] <- old_deps[[rhs]]
      old_deps <- old_deps[names(old_deps) != rhs]
      new_rhs <- c(new_rhs, rhs)
    }
  }
  # Remove move dependants from determinant sets of remaining dependants
  for (rhs in names(old_deps)) {
    old_deps[[rhs]] <- setdiff(old_deps[[rhs]], new_rhs)
  }
  old_rhs <- setdiff(names(old_deps), lhs_dep)
  # Remove remaining dependents from determinant sets of moved dependents
  for (rhs in names(new_deps)) {
    new_deps[[rhs]] <- setdiff(new_deps[[rhs]], old_rhs)
  }

  list(
    Dependencies(
      dependencies = old_deps,
      primary_key = get_prim_key(dependencies)
    ),
    Dependencies(
      dependencies = new_deps,
      primary_key = lhs_dep
    )
  )
}

choose_index <- function(keys, df) {
  # Chooses key from a list of keys. Order of priority:
  # 1) shortest length
  # 2) has "id" in some form in name of an attribute
  # 3) has attribute furthest to the left in table
  #
  # Arguments:
  #     keys (list[set[str]]) : list of keys to choose from
  #     df (pd.DataFrame) : pandas dataframe keys are for
  #
  # Returns:
  #     index (list[str]) : chosen key

  if (length(keys) == 0)
    return(NA_character_)
  sort_key <- keys[order(lengths(keys))]
  m <- length(sort_key[[1]])
  options <- sort_key[lengths(sort_key) == m]
  for (key in options) {
    for (attr in key) {
      if (any(vapply(
        c("_id", " id", "id _", "id "),
        \(s) grepl(s, tolower(attr), fixed = TRUE),
        logical(1)
      )))
        return(key)
    }
  }
  if (isTRUE(is.na(df)))
    return(options[[1]])

  for (col in colnames(df)) {
    includes <- options[vapply(options, \(opt) col %in% opt, logical(1))]
    if (length(includes) == 1)
      return(includes[[1]])
    if (length(includes) > 1)
      options <- includes
  }
  options[[1]]
}

filter <- function(keys, df) {
  # Filters out any keys that contain attributes that are not strings, ints, or
  # categories from a list of relations.
  #
  # Arguments:
  #     keys (list[(list[str], str)]) : relationships to filter out
  #     df (pd.DataFrame) : dataframe attributes in keys are from
  # MY NOTES: original checks class against integer, category/factor, and
  # "object", which seems to be a generic class. That seems silly, so I've
  # added character and logical instead.
  for (ky in keys) {
    key <- ky[[1]]
    rhs <- ky[[2]]
    for (attr in key) {
      if (
        !inherits(df[[attr]], "character") &&
        !inherits(df[[attr]], "integer") &&
        !inherits(df[[attr]], "factor") &&
        !inherits(df[[attr]], "logical")
      ) {
        keys <- setdiff(keys, list(ky))
        break
      }
    }
  }
  keys
}

get_prim_key <- function(dependencies) {
  UseMethod("get_prim_key")
}

#' @export
get_prim_key.Dependencies <- function(dependencies)
  # Gets primary key.
  # Returns:
  #     prim_key (list[str]) : the primary key
  dependencies$primary_key

find_partial_deps <- function(dependencies) {
  UseMethod("find_partial_deps")
}

#' @export
find_partial_deps.Dependencies <- function(dependencies) {
  # Finds all partial dependencies within self.
  # Returns:
  #     partial_deps (list[(list[str], str)]) : partial dependencies
  # Example:
  #     A --> B
  #     C --> D
  #     DF --> E
  #     G --> F
  #     finds:
  #     A --> B
  #     C --> D
  #     G --> F
  partial_deps <- list()
  cand_keys <- get_prim_key(dependencies)
  key_attrs <- cand_keys
  rels <- tuple_relations(dependencies)
  for (lr in rels) {
    lhs <- lr[[1]]
    rhs <- lr[[2]]
    if (!is.element(rhs, key_attrs)) {
      lhs_cands_key_intersection <- intersect(lhs, cand_keys)
      inter_size <- length(lhs_cands_key_intersection)
      if (inter_size > 0 && inter_size < length(cand_keys))
        partial_deps <- c(partial_deps, list(lr))
    }
  }
  partial_deps
}

find_trans_deps <- function(dependencies) {
  UseMethod("find_trans_deps")
}

#' @export
find_trans_deps.Dependencies <- function(dependencies) {
  # Finds all transitive dependencies within self.
  # Returns:
  #     trans_deps (list[(list[str], str)]) : transitive dependencies
  # Example:
  #     A --> B
  #     C --> D
  #     DF --> E
  #     G --> F
  #     finds:
  #     DF --> E
  trans_deps <- list()
  cand_keys <- get_prim_key(dependencies)
  key_attrs <- cand_keys
  all_attrs <- names(dependencies$dependencies)
  rels <- tuple_relations(dependencies)

  for (lr in rels) {
    lhs <- lr[[1]]
    rhs <- lr[[2]]
    if (!is.element(rhs, key_attrs)) {
      if (!identical(find_closure(rels, lhs), all_attrs)) {
        acc <- all(lhs %in% cand_keys)
        if (!acc)
          trans_deps <- c(trans_deps, list(lr))
      }
    }
  }
  trans_deps
}

equiv_attrs <- function(dependencies, one, two) {
  # Returns True if one and two are equivalent attributes, or in another
  # words have equivalent closures.
  # Returns:
  #     is_equiv (bool) : True if equivalent, False otherwise
  tups <- tuple_relations(dependencies)
  identical(find_closure(tups, one), find_closure(tups, two))
}

find_closure <- function(rel, attrs) {
  # Finds the closure of attrs under the relations in rel.
  # Arguments:
  #     rel (list[(list[str], str)]) : relationships to find closure under
  #     attrs (list[str]) : attributes to find the closure of
  # Returns:
  #     closure (set[str]) : attrs' closure, aka the attributes that can be
  #     determined from the attributes in attrs
  if (!is.character(attrs))
    stop(paste("attr is", toString(class(attrs))))
  if (length(rel) == 0)
    return(attrs)
  for (n in seq_along(rel)) {
    r <- rel[[n]]
    dep_attrs <- r[[1]]
    dep <- r[[2]]
    if (all(is.element(dep_attrs, attrs))) {
      if (!is.element(dep, attrs))
        attrs <- c(attrs, dep)
      return(find_closure(rel[-n], attrs))
    }
  }
  attrs
}
