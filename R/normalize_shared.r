find_most_comm <- function(deps, dependencies, df = NA) {
  # Given a list of dependency relations, finds the most common set of
  # LHS attributes. If more than one LHS set occurs the same amount of
  # times, chooses the set with the least number of attributes.
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
    for (key in dependencies$primary_key) {
      if (equiv_attrs(dependencies, max_lhs[i], key))
        max_lhs[i] <- key
    }
  }
  max_lhs
}

split_on_dep <- function(lhs_dep, dependencies) {
  # Given an attribute set, breaks up the given dependencies such that the
  # attribute set is the primary key of the new (child) dependency set. The
  # remaining old (parent) keeps the original primary key.
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
      primary_key = dependencies$primary_key
    ),
    Dependencies(
      dependencies = new_deps,
      primary_key = lhs_dep
    )
  )
}

choose_index <- function(keys, df) {
  # Chooses an index / primary key from a list of keys.
  # Order of priority:
  # 1) shortest length
  # 2) has "id" prefix/suffix in the name of any attribute
  # 3) has the attribute furthest to the left in the table
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

filter <- function(relations, df) {
  # Removes functional dependencies where any determinant attributes do no
  # contain strings, integers, factors, or logicals in the data.frame. The idea
  # is that, for example, we don't expect floats to be part of a key.
  for (rel in relations) {
    lhs <- rel[[1]]
    for (attr in lhs) {
      if (!inherits(
        df[[attr]],
        c("character", "integer", "factor", "logical")
      )) {
        relations <- setdiff(relations, list(rel))
        break
      }
    }
  }
  relations
}

remove_extraneous_attributes <- function(x) {
  rels <- x
  for (lr in x) {
    lhs <- lr[[1]]
    rhs <- lr[[2]]
    y <- lhs
    for (attr in lhs) {
      y_ <- setdiff(y, attr)
      if (rhs %in% find_closure(x, y_))
        y <- setdiff(y, attr)
      rels <- setdiff(rels, list(lr))
      rels <- c(rels, list(list(y, rhs)))
      x <- setdiff(x, list(list(lhs, rhs)))
      x <- c(x, list(list(y, rhs)))
    }
  }
  unique(x)
}

equiv_attrs <- function(dependencies, one, two) {
  # returns TRUE if attribute sets one and two have equivalent closures.
  tups <- tuple_relations(dependencies)
  identical(find_closure(tups, one), find_closure(tups, two))
}

find_closure <- function(rel, attrs) {
  if (!is.character(attrs))
    stop(paste("attr is", toString(class(attrs))))
  if (length(rel) == 0)
    return(attrs)
  for (n in seq_along(rel)) {
    r <- rel[[n]]
    dep_attrs <- r[[1]]
    dep <- r[[2]]
    if (length(dep) != 1)
      stop(paste(toString(dep), length(dep), toString(lengths(dep)), toString(r)))
    if (all(is.element(dep_attrs, attrs))) {
      if (!is.element(dep, attrs))
        attrs <- c(attrs, dep)
      return(find_closure(rel[-n], attrs))
    }
  }
  attrs
}
