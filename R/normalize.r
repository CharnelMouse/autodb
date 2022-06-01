normalize <- function(dependencies, df) {
  # Normalizes the dependency relationships in dependencies into new
  # groups by breaking up all partial and transitive dependencies.
  #
  # Arguments:
  #     dependencies (Dependencies) : the dependencies to be split up
  #
  # Returns:
  #     new_groups (list[Dependencies]) : list of new dependencies objects
  #     representing the new groups
  dependencies <- remove_implied_extroneous(dependencies)
  no_part_deps <- remove_part_deps(dependencies, df)
  no_trans_deps <- list()
  for (grp in no_part_deps)
    no_trans_deps <- c(no_trans_deps, list(remove_trans_deps(grp, df)))
  no_trans_deps
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

depDF <- function(deps, df, index, parent = NA) {
  list(
    deps = deps,
    df = df,
    index = index,
    children = list(),
    parent = parent
  )
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

normalize_dataframe <- function(depdf) {
  # Normalizes the dataframe represetned by depdf, created descendents
  # as needed.
  #
  # Arguments:
  #     depdf (DepDF) : depdf to normalize
  part_deps <- find_partial_deps(depdf$deps)
  part_deps <- filter(part_deps, depdf$df)
  if (length(part_deps) > 0) {
    split_on <- find_most_comm(part_deps, depdf$deps, depdf$df)
    depdfs <- split_up(depdf, split_on)
    return(setNames(
      depdfs,
      make.unique(vapply(depdfs, name_dataframe, character(1)))
    ))
  }
  trans_deps <- find_trans_deps(depdf$deps)
  trans_deps <- filter(trans_deps, depdf$df)
  if (length(trans_deps) > 0) {
    split_on <- find_most_comm(trans_deps, depdf$deps, depdf$df)
    depdfs <- split_up(depdf, split_on)
    return(setNames(
      depdfs,
      make.unique(vapply(depdfs, name_dataframe, character(1)))
    ))
  }
  setNames(list(depdf), name_dataframe(depdf))
}

split_up <- function(depdf, split_on) {
  # Breaks off a depdf and forms its child. Recursively calls normalize on
  # the original depdf, and its newly formed child.
  #
  # Arguments:
  #     split_on (list[str]) : attributes to split the dataframe on
  #     depdf (DepDF) : the depdf ot split
  pc <- split_on_dep(split_on, depdf$deps)
  parent_deps <- pc[[1]]
  child_deps <- pc[[2]]
  child <- list(
    deps = child_deps,
    df = form_child(depdf$df, child_deps),
    index = split_on
  )
  depdf$deps <- parent_deps
  depdf$df <- depdf$df[
    ,
    names(parent_deps$dependencies)
  ]
  c(
    normalize_dataframe(depdf),
    normalize_dataframe(child)
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
  # Breaks up the dependency relations in dependencies into new groups of
  # relations so that there are no more partial dependencies.
  #
  # Arguments:
  #     dependencies (Dependncies) : the dependencies to be split up
  #
  # Returns:
  #     new_groups (list[Dependencies]) : list of new dependencies objects
  #     representing the new groups with no partial depenencies
  part_deps <- find_partial_deps(dependencies)
  part_deps <- filter(part_deps, df)
  if (length(part_deps) == 0)
    return(dependencies)
  new_deps <- split_on_dep(find_most_comm(part_deps, dependencies), dependencies)
  c(
    remove_part_deps(new_deps[1], df),
    remove_part_deps(new_deps[2], df)
  )
}

remove_trans_deps <- function(dependencies, df) {
  # Breaks up the dependency relations in dependencies into new groups of
  # relations so that there are no more transitive dependencies.
  #
  # Arguments:
  #     dependencies (Dependencies) : the dependencies to be split up
  #
  # Returns:
  #     new_groups (list[Dependencies]): list of new dependencies objects
  #     representing the new groups with no transitive depenencies
  trans_deps <- find_trans_deps(dependencies)
  trans_deps <- filter(trans_deps, df)
  if (length(trans_deps) == 0)
    return(dependencies)
  new_deps <- split_on_dep(find_most_comm(trans_deps, dependencies), dependencies)
  c(
    remove_trans_deps(new_deps[0], df),
    remove_trans_deps(new_deps[1], df)
  )
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
    list(dependencies = old_deps, primary_key = get_prim_key(dependencies)),
    list(dependencies = new_deps, primary_key = lhs_dep)
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

  groups <- split(df, as.list(df[, c(prim_key), drop = FALSE]), drop = TRUE)

  for (group in groups) {
    df_lst <- c(df_lst, list(lapply(group, Mode)))
    # new_df = new_df.append(group.mode().iloc[0], ignore_index=TRUE)
  }
  result <- `rownames<-`(
    setNames(data.frame(Reduce(rbind, df_lst)), colnames(df)),
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

get_prim_key <- function(dependencies)
  # Gets primary key.
  # Returns:
  #     prim_key (list[str]) : the primary key
  dependencies$primary_key

tuple_relations <- function(dependencies) {
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

find_partial_deps <- function(dependencies) {
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
  identical(find_closure(tups, list(one)), find_closure(tups, list(two)))
}

find_closure <- function(rel, attrs) {
  # Finds the closure of attrs under the relations in rel.
  # Arguments:
  #     rel (list[(list[str], str)]) : relationships to find closure under
  #     attrs (list[str]) : attributes to find the closure of
  # Returns:
  #     closure (set[str]) : attrs' closure, aka the attributes that can be
  #     determined from the attributes in attrs
  helper <- function(rel, set_attr) {
    if (length(rel) == 0)
      return(list(set_attr))
    for (r in rel) {
      dep_attrs <- r[[1]]
      dep <- r[[2]]
      if (is.element(list(dep_attrs), set_attr)) {
        rel_ <- rel
        rel_ <- setdiff(rel, list(list(dep_attrs, dep)))
        return(helper(rel_, c(set_attr, dep)))
      }
    }
    set_attr
  }
  helper(rel, attrs)
}

serialize <- function(dfdd) {
  ser <- dfdd
  for (rhs in names(ser))
    ser[[rhs]] <- lapply(ser[[rhs]], list)
  ser
}
