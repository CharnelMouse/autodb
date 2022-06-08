normalize <- function(x, ...) {
  UseMethod("normalize")
}

#' @export
normalize.DepDF <- function(x) {
  # Only splits off a descendent as needed, doesn't normalize it too.
  # Additionally, stops after the first split, e.g. splitting for partial
  # dependencies might leave transitive dependencies in the main data.frame.
  # This is a silly function.
  part_deps <- find_filtered_partial_deps(x$deps, x$df)
  if (length(part_deps) > 0) {
    new_depdfs <- split_for(x, part_deps)
    return(new_depdfs)
  }
  trans_deps <- find_filtered_trans_deps(x$deps, x$df)
  if (length(trans_deps) > 0) {
    new_depdfs <- split_for(x, trans_deps)
    return(new_depdfs)
  }
  stats::setNames(list(x), name_dataframe(x))
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
  x <- remove_implied_extroneous(x)
  no_part_deps <- remove_part_deps(x, df)
  no_trans_deps <- list()
  for (grp in no_part_deps)
    no_trans_deps <- c(no_trans_deps, remove_trans_deps(grp, df))
  no_trans_deps
}

#' Normalise a data.frame based on given dependencies
#'
#' @param x a data.frame, containing the data to be normalised.
#' @param dependencies a Dependencies object, giving the dependencies to
#'   determine normalisation with.
#'
#' @return a list of data.frames, containing the normalised data.
#' @export
normalize.data.frame <- function(x, dependencies) {
  # Normalizes a dataframe based on the dependencies given. Keys for the newly
  # created DataFrames can only be columns that are strings, ints, or
  # categories. Keys are chosen according to the priority:
  #   1) shortest lenghts 2) has "id" in some form in the name of an attribute
  # 3) has attribute furthest to left in the table
  #
  # Arguments:
  #   df (pd.DataFrame) : dataframe to split up
  # dependencies (Dependencies) : the dependencies to be normalized
  #
  # Returns:
  #   new_dfs (list[DataFrame]) : list of new dataframes
  depdf <- DepDF(dependencies, x, get_prim_key(dependencies))
  df_list <- normalize.DepDF(depdf)
  df_list
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
normalize.entityset <- function(x, accuracy) {
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

# class DepDF(object):
#   """
#     Represents dataframe and functional dependencies between columns in it.
#     Used in the normalization process.
#
#     Attributes:
#         deps
#         df
#         parent
#         children
#         index
#     """
#
# def __init__(self, deps, df, index, parent=None):
#   """
#         Creates a DepDF.
#
#         Arguments:
#             deps (Dependencies) : dependenies among the df
#             df (pd.DataFrame) : dataframe for the object
#             index (list[str]) : index columns for dataframe
#             parent (DepDF, optional) : parent DepDF object
#         """
# self.deps = deps
# self.df = df
# self.parent = parent
# self.children = []
# self.index = index
#
# def return_dfs(self):
#   """
#         Returns the dataframes stored in self and all its descendents.
#
#         Returns:
#             dfs (list[pd.DataFrame]) : dataframes
#         """
# if self.children == []:
#   return [self.df]
# result = [self.df]
# for child in self.children:
#   result += child.return_dfs()
# return result

DepDF <- function(deps, df, index = deps$primary_key, parent = NA_character_) {
  UseMethod("DepDF")
}

#' @export
DepDF.Dependencies <- function(
  deps,
  df,
  index = deps$primary_key,
  parent = NA_character_
) {
  lst <- list(
    deps = deps,
    df = df,
    index = index,
    children = character(),
    parent = parent
  )
  class(lst) <- c("DepDF", class(lst))
  lst
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

split_up <- function(depdf, split_on) {
  UseMethod("split_up")
}

#' @export
split_up.DepDF <- function(depdf, split_on) {
  # Breaks off a depdf and forms its child. Recursively calls normalize on
  # the original depdf, and its newly formed child.
  #
  # Arguments:
  #     split_on (list[str]) : attributes to split the dataframe on
  #     depdf (DepDF) : the depdf ot split
  pc <- split_on_dep(split_on, depdf$deps)
  parent_deps <- pc[[1]]
  child_deps <- pc[[2]]
  child <- DepDF(
    deps = child_deps,
    df = form_child(depdf$df, child_deps),
    index = split_on,
    parent = name_dataframe(depdf)
  )
  depdf$deps <- parent_deps
  depdf$df <- depdf$df[
    ,
    names(parent_deps$dependencies)
  ]
  depdf$children <- c(depdf$children, name_dataframe(child))
  c(
    normalize(depdf),
    normalize(child)
  )
}

name_dataframe <- function(depdf) {
  paste(depdf$index, collapse = "_")
}

form_child <- function(df, deps) {
  # Returns a new dataframe based off of the dependencies in deps.
  #
  # Arguments:
  #     df (pd.DataFrame) : dataframe to create new dataframe from
  #     deps (Dependencies) : dependencies to base new dataframe off of
  attrs <- names(deps$dependencies)
  drops <- setdiff(colnames(df), attrs)
  new_df <- df[, setdiff(colnames(df), drops)]
  new_df <- drop_primary_dups(new_df, get_prim_key(deps))
  new_df
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

split_for <- function(depdf, unwanted_deps) {
  split_on <- find_most_comm(unwanted_deps, depdf$deps, depdf$df)
  depdfs <- split_up(depdf, split_on)
  nms <- vapply(depdfs, name_dataframe, character(1))
  stopifnot(!anyDuplicated(nms))
  stats::setNames(depdfs, make.unique(nms))
}

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

drop_primary_dups <- function(df, prim_key) {
  # Drops all duplicates based off of the columns in prim_key. If there isn't a
  # unique value for the other columns, for every unique instance of columns in
  # prim_key, keeps the "mode" of the unique instances' occurance.
  #
  # Arguments:
  #     df (pd.DataFrame) : dataframe to drop duplicates of
  #     prim_key (list[str]) : columns that form the primary key of the dataframe
  #
  # Returns:
  #     new_df (pd.DataFrame) : dataframe with duplicates dropped
  df_lst <- list()

  if (nrow(unique(df[, prim_key, drop = FALSE])) == nrow(df))
    return(df)

  groups <- split(
    df,
    as.list(df[, c(prim_key), drop = FALSE]),
    drop = TRUE
  )

  for (group in groups) {
    df_lst <- c(df_lst, list(data.frame(lapply(group, Mode))))
    # new_df = new_df.append(group.mode().iloc[0], ignore_index=TRUE)
  }
  result <- `rownames<-`(
    stats::setNames(Reduce(rbind, df_lst), colnames(df)),
    NULL
  )
  for (i in seq_along(df)) {
    class(result[[i]]) <- class(df[[i]])
  }
  result
}

Mode <- function(x) {
  uniqs <- unique(x)
  tabs <- tabulate(match(x, uniqs))
  uniqs[[which.max(tabs)]]
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

# Relevant methods from classes.py go here

get_prim_key <- function(dependencies) {
  UseMethod("get_prim_key")
}

#' @export
get_prim_key.Dependencies <- function(dependencies)
  # Gets primary key.
  # Returns:
  #     prim_key (list[str]) : the primary key
  dependencies$primary_key

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

remove_implied_extroneous <- function(dependencies) {
  UseMethod("remove_implied_extroneous")
}

#' @export
remove_implied_extroneous.Dependencies <- function(dependencies) {
  # Removes all implied extroneous attributes from relations in self.
  # Example:
  #     A --> B
  #     AB --> C
  #     becomes
  #     A --> B
  #     A --> C
  rels <- tuple_relations(dependencies)
  for (lr in rels) {
    lhs <- lr[[1]]
    rhs <- lr[[2]]
    y <- lhs
    for (attr in lhs) {
      y_ <- setdiff(y, attr)
      if (rhs %in% find_closure(rels, y_))
        y <- setdiff(y, attr)
      rels <- setdiff(rels, list(lr))
      rels <- c(rels, list(list(y, rhs)))
      dependencies$dependencies[[rhs]] <- setdiff(
        dependencies$dependencies[[rhs]],
        list(lhs)
      )
      dependencies$dependencies[[rhs]] <- c(
        dependencies$dependencies[[rhs]],
        list(y)
      )
    }
  }
  dependencies$dependencies <- lapply(dependencies$dependencies, unique)
  dependencies
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
