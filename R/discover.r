#' Dependency discovery with DFD
#'
#' Finds all the minimal functional dependencies represented in a data frame.
#'
#' Column names for \code{\link{df}} must be unique.
#'
#' The algorithm used for finding dependencies is DFD. This searches for
#' determinant sets for each dependent attribute (dependant) by traversing the
#' powerset of the other (non-excluded) attributes, and is equivalent to
#' depth-first.
#'
#' The implementation for DFD differs a little from the algorithm presented in
#' the original paper:
#' \itemize{
#'   \item Some attributes, or attribute types, can be designated, ahead of
#'   time, as not being candidate members for determinant sets. This reduces the
#'   number of candidate determinant sets to be searched, saving time by not
#'   searching for determinant sets that the user would remove later anyway.
#'   \item Attributes that have a single unique value, i.e. are
#'   constant, get attributed a single empty determinant set. In the standard
#'   DFD algorithm, they would be assigned all the other non-excluded attributes
#'   as length-one determinant sets. Assigning them the empty set distinguishes
#'   them as constant, allowing for special treatment at normalisation and later
#'   steps.
#'   \item As was done in the original Python library, there is an extra case in
#'   seed generation for when there are no discovered maximal non-dependencies.
#'   In this case, we take all of the single-attribute nodes, then filter out by
#'   minimal dependencies as usual. This is equivalent to taking the empty set
#'   as the single maximal non-dependency.
#'   \item There are three results when checking whether a candidate node is
#'   minimal/maximal. TRUE indicates the node is minimal/maximal, as usual.
#'   FALSE has been split into FALSE and NA. NA indicates that we can not yet
#'   determine whether the node is minimal/maximal. FALSE indicates that we have
#'   determined that it is not minimal/maximal, and we can set its category as
#'   such. This is done by checking whether any of its adjacent
#'   subsets/supersets are dependencies/non-dependencies, instead of waiting to
#'   exhaust the adjacent subsets/supersets to visit when picking the next node
#'   to visit.
#'   \item We do not yet keep hashmaps to manage subset/superset relationships,
#'   as described in Section 3.5 of the original paper.
#'   \item \code{skip_bijections} allows for additional optimisation for finding
#'   functional dependencies when there are pairwise-equivalent attributes.
#'   \item Missing values (NA) are treated as a normal value, with NA = NA being
#'   true, and x = NA being false for any non-NA value of x.
#' }
#'
#' ## Skipping bijections
#'
#' Skipping bijections allows skipping redundant searches. For example, if the
#' search discovers that \code{A -> B} and \code{B -> A}, then only one of those
#' attributes is considered for the remainder of the search. Since the search
#' time increases exponentially with the number of attributes considered, this
#' can significantly speed up search times. At the moment, this is only be done
#' for bijections between single attributes, such as \code{A <-> B}; if \code{A
#' <-> {B, C}}, nothing is skipped. Whether bijections are skipped doesn't
#' affect which functional dependencies are present in the output, but it might
#' affect their order.
#'
#' Skipping bijections for approximate dependencies, i.e. when `accuracy < 1`,
#' should be avoided: it can result in incorrect output, since an approximate
#' bijection doesn't imply equivalent approximate dependencies.
#'
#' ## Limiting the determinant set size
#'
#' Setting \code{detset_limit} smaller than the largest-possible value has
#' different behaviour for different search algorithms, the result is always
#' that \code{discover(x, 1, detset_limit = n)} is equivalent to doing a full
#' search, \code{fds <- discover(x, 1)}, then
#' filtering by determinant set size post-hoc, \code{fds[lengths(detset(fds)) <=
#' n]}.
#'
#' For DFD, the naive way to implement it is by removing determinant sets larger
#' than the limit from the search tree for possible functional dependencies for
#' each dependant. However, this usually results in the search taking much more
#' time than without a limit.
#'
#' For example, suppose we search for determinant sets for a dependant that has
#' none (the dependant is the only key for \code{df}, for example). Using DFD,
#' we begin with a single attribute, then add other attributes one-by-one, since
#' every set gives a non-dependency. When we reach a maximum-size set, we can
#' mark all subsets as also being non-dependencies.
#'
#' With the default limit, there is only one maximum-size set, containing all of
#' the available attributes. If there are \eqn{n} candidate attributes for
#' determinants, the search finishes after visiting \eqn{n} sets.
#'
#' With a smaller limit \eqn{k}, there are \eqn{\binom{n}{k}} maximum-size sets
#' to explore. Since a DFD search adds or removes one attribute at each step,
#' this means the search must take at least \eqn{k - 2 + 2\binom{n}{k}} steps,
#' which is larger than \eqn{n} for all non-trivial cases \eqn{0 < k \leq n}.
#'
#' We therefore use a different approach, where any determinant sets above the
#' size limit are not allowed to be candidate seeds for new search paths, and
#' any discovered dependencies with a size above the limit are discard at the
#' end of the entire DFD search. This means that nodes for determinant sets
#' above the size limit are only visited in order to determine maximality of
#' non-dependencies within the size limit. It turns out to be rare that this
#' results in a significant speed-up, but it never results in the search having
#' to visit more nodes than it would without a size limit, so the average search
#' time is never made worse.
#' @param df a data.frame, the relation to evaluate.
#' @param accuracy a numeric in (0, 1]: the accuracy threshold required in order
#'   to conclude a dependency.
#' @param full_cache a logical, indicating whether to store information about
#'   how sets of attributes group the relation records (stripped partitions).
#'   Otherwise, only the number of groups is stored. Storing the stripped
#'   partition is expected to let the algorithm run more quickly, but might be
#'   inefficient for small data frames or small amounts of memory.
#' @param store_cache a logical, indicating whether to keep cached information
#'   to use when finding dependencies for other dependants. This allows the
#'   algorithm to run more quickly by not having to re-calculate information,
#'   but takes up more memory.
#' @param skip_bijections a logical, indicating whether to skip some dependency
#'   searches that are made redundant by discovered bijections between
#'   attributes. This can significantly speed up the search if \code{df}
#'   contains equivalent attributes early in column order, but results in
#'   undefined behaviour if \code{accuracy < 1}. See Details for more
#'   information.
#' @param exclude a character vector, containing names of attributes to not
#'   consider as members of determinant sets. If names are given that aren't
#'   present in \code{df}, the user is given a warning.
#' @param exclude_class a character vector, indicating classes of attributes to
#'   not consider as members of determinant_sets. Attributes are excluded if
#'   they inherit from any given class.
#' @param dependants a character vector, containing names of all attributes for
#'   which to find minimal functional dependencies for which they are the
#' dependant. By default, this is all of the attribute names. A smaller set of
#' attribute names reduces the amount of searching required, so can reduce the
#' computation time if only some potential dependencies are of interest.
#' @param detset_limit an integer, indicating the largest determinant set size
#'   that should be searched for. By default, this is large enough to allow all
#'   possible determinant sets. See Details for comments about the effect on the
#'   result, and on the computation time.
#' @inheritParams autodb
#'
#' @return A \code{\link{functional_dependency}} object, containing the discovered
#'   dependencies. The column names of \code{df} are stored in the \code{attrs}
#'   attribute, in order, to serve as a default priority order for the
#'   attributes during normalisation.
#' @encoding UTF-8
#' @references
#' Abedjan Z., Schulze P., Naumann F. (2014) DFD: efficient functional
#' dependency discovery. *Proceedings of the 23rd ACM International Conference
#' on Conference on Information and Knowledge Management (CIKM '14). New York,
#' U.S.A.*, 949--958.
#' @examples
#' # simple example
#' discover(ChickWeight, 1)
#'
#' # example with spurious dependencies
#' discover(CO2, 1)
#' # exclude attributes that can't be determinants.
#' # in this case, the numeric attributes are now
#' # not determined by anything, because of repeat measurements
#' # with no variable to mark them as such.
#' discover(CO2, 1, exclude_class = "numeric")
#' # include only dependencies with dependants of interest.
#' discover(CO2, 1, dependants = c("Treatment", "uptake"))
#' @export
discover <- function(
  df,
  accuracy,
  full_cache = TRUE,
  store_cache = TRUE,
  skip_bijections = FALSE,
  exclude = character(),
  exclude_class = character(),
  dependants = names(df),
  detset_limit = ncol(df) - 1L,
  progress = FALSE,
  progress_file = ""
) {
  use_visited <- TRUE

  if (skip_bijections && accuracy < 1)
    warning("skipping bijections when accuracy < 1 can result in incorrect output")

  report <- reporter(progress, progress_file, new = TRUE)

  n_cols <- ncol(df)
  if (n_cols == 0)
    return(functional_dependency(
      stats::setNames(list(), character()),
      attrs_order = character()
    ))
  column_names <- colnames(df)
  duplicates <- which(duplicated(column_names))
  if (length(duplicates) > 0) {
    dup_names <- unique(column_names[duplicates])
    sorted_dup_names <- dup_names[order(match(dup_names, column_names))]
    stop("duplicate column names: ", toString(sorted_dup_names))
  }
  if (any(!is.element(exclude, column_names)))
    warning("there are attribute names in exclude not present in df")

  valid_determinant_name <- !is.element(column_names, exclude)
  valid_determinant_class <- !vapply(
    df,
    inherits,
    logical(1),
    exclude_class
  )
  valid_determinant <- valid_determinant_name & valid_determinant_class
  valid_determinant_attrs_prefixing <- column_names[valid_determinant]

  # convert all columns to integers, since they're checked for duplicates more
  # quickly when calculating partitions
  df <- report$exp(
    data.frame(lapply(df, \(x) match(x, x))) |>
      stats::setNames(column_names),
    "simplifying data types"
  )
  partitions <- list()
  dependencies <- stats::setNames(rep(list(list()), n_cols), column_names)

  # check for constant-value columns, because if columns are fixed we can
  # ignore them for the rest of the search
  fixed <- character()
  for (i in seq_along(column_names)) {
    attr <- column_names[i]
    if (all(is.na(df[[attr]])) || all(df[[attr]] == df[[attr]][1])) {
      fixed <- report$op(fixed, c, paste(attr, "is fixed"), attr)
      if (attr %in% dependants)
        dependencies[[attr]] <- list(character())
    }
  }
  nonfixed <- setdiff(column_names, fixed)
  valid_dependant_attrs <- intersect(dependants, nonfixed)
  if (
    length(valid_dependant_attrs) == 0 ||
    detset_limit < 1
  ) {
    report$stat("no valid dependants, or detset_limit < 1, skipping search")
    return(flatten(
      filter_nonflat_dependencies(dependencies, detset_limit),
      column_names
    ))
  }

  # For nonfixed attributes, all can be dependants, but
  # might not all be valid determinants.
  # Maximum size of determinant set for a dependant is number
  # of other valid determinants.
  # If there are dependants that aren't valid determinants,
  # this is number of valid determinant attributes. If there
  # aren't, subtract one.
  valid_determinant_attrs <- intersect(
    nonfixed,
    valid_determinant_attrs_prefixing
  )
  if (length(valid_determinant_attrs) < n_cols) {
    report$stat(
      paste(
        "attributes not considered as determinants:",
        toString(setdiff(column_names, valid_determinant_attrs))
      )
    )
  }
  valid_determinant_nonfixed_indices <- match(valid_determinant_attrs, nonfixed)

  n_dependant_only <- length(nonfixed) - length(valid_determinant_attrs)
  max_n_lhs_attrs <- length(valid_determinant_attrs) -
    as.integer(n_dependant_only == 0)
  # using 0 would allow for one more column, but makes indexing a pain
  lhs_attrs_limit <- floor(log(.Machine$integer.max, 2))
  if (max_n_lhs_attrs > lhs_attrs_limit)
    stop(paste(
      "only data.frames with up to",
      lhs_attrs_limit,
      "columns possible in a determinant set currently supported"
    ))
  bijections <- list()

  # main search
  if (max_n_lhs_attrs > 0) {
    powerset <- report$op(
      max_n_lhs_attrs,
      nonempty_powerset,
      "constructing powerset",
      use_visited
    )
    # cache generated powerset and reductions, otherwise we spend a lot
    # of time duplicating reduction work
    all_powersets <- stats::setNames(list(powerset), max_n_lhs_attrs)
    compute_partitions <- partition_computer(
      unname(df[, nonfixed, drop = FALSE]),
      accuracy,
      full_cache
    )
    for (rhs in seq_along(nonfixed)[nonfixed %in% valid_dependant_attrs]) {
      report$stat(paste("dependant", nonfixed[rhs]))
      lhs_nonfixed_indices <- setdiff(valid_determinant_nonfixed_indices, rhs)
      n_lhs_attrs <- length(lhs_nonfixed_indices)
      expected_n_lhs_attrs <- max_n_lhs_attrs -
        (n_dependant_only > 0 && is.element(rhs, valid_determinant_nonfixed_indices))
      stopifnot(n_lhs_attrs == expected_n_lhs_attrs)
      bijection_candidate_nonfixed_indices <- if (skip_bijections)
        match(
          names(dependencies)[
            vapply(
              dependencies,
              \(x) any(vapply(x, identical, logical(1), nonfixed[[rhs]])),
              logical(1)
            )
          ],
          nonfixed
        ) |>
        intersect(lhs_nonfixed_indices)
      else
        integer()
      if (n_lhs_attrs > 0) {
        if (n_lhs_attrs %in% names(all_powersets))
          nodes <- all_powersets[[as.character(n_lhs_attrs)]]
        else{
          nodes <- reduce_powerset(powerset, n_lhs_attrs)
          all_powersets[[as.character(n_lhs_attrs)]] <- nodes
        }
        lhss <- report$op(
          rhs,
          find_LHSs_dfd,
          "determinants available, starting search",
          lhs_nonfixed_indices,
          nodes,
          n_lhs_attrs,
          partitions,
          compute_partitions,
          bijection_candidate_nonfixed_indices,
          detset_limit,
          store_cache
        )
        if (lhss[[2 + store_cache]]) {
          stopifnot(
            is.element(lhss[[1]], bijection_candidate_nonfixed_indices),
            lhss[[1]] < rhs
          )
          bij_ind <- match(lhss[[1]], names(bijections))
          if (is.na(bij_ind))
            bijections <- c(
              bijections,
              stats::setNames(list(c(lhss[[1]], rhs)), lhss[[1]])
            )
          else{
            bijections[[bij_ind]] <- c(
              bijections[[bij_ind]],
              rhs
            )
          }
          valid_determinant_nonfixed_indices <- setdiff(
            valid_determinant_nonfixed_indices,
            rhs
          )
          max_n_lhs_attrs <- max_n_lhs_attrs - 1L
          if (max_n_lhs_attrs %in% names(all_powersets))
            powerset <- all_powersets[[as.character(max_n_lhs_attrs)]]
          else{
            powerset <- reduce_powerset(powerset, max_n_lhs_attrs)
            all_powersets[[as.character(max_n_lhs_attrs)]] <- powerset
          }
        }else
          dependencies[[nonfixed[rhs]]] <- c(
            dependencies[[nonfixed[rhs]]],
            lapply(lhss[[1]], \(x) nonfixed[x])
          )
        if (store_cache)
          partitions <- lhss[[2]]
      }
    }
  }

  report$stat("DFD complete")
  if (skip_bijections)
    dependencies <- add_deps_implied_by_bijections(
      dependencies,
      bijections,
      nonfixed,
      column_names
    )
  flatten(
    filter_nonflat_dependencies(dependencies, detset_limit),
    column_names
  )
}

find_LHSs_dfd <- function(
  rhs,
  lhs_nonfixed_indices,
  nodes,
  n_lhs_attrs,
  partitions,
  compute_partitions,
  bijection_candidate_nonfixed_indices,
  detset_limit,
  store_cache = FALSE
) {
  # The original library "names" nodes with their attribute set,
  # so finding a node involves matching a character vector against
  # a list of character vectors. This is slow.
  # Instead, we assign each possible attribute set an integer,
  # by taking the integer that matches the bitset. This limits
  # us to floor(.Machine$integer.max) attributes as potential determinants.
  # We could use numerics to get to .Machine$double.digits determinant
  # attributes, but these would be arbitrary floats rather than integers,
  # so more work would be required to match indices etc.
  # Assigning the integers per find_LHSs call allows one more column,
  # since we don't need to include the dependant.
  # Node categories:
  # -3 = candidate maximal non-dependency
  # -2 = maximal non-dependency
  # -1 = non-maximal non-dependency
  #  0 = unresolved
  #  1 = non-minimal dependency
  #  2 = minimal dependency
  #  3 = candidate minimal dependency

  # initial seeds are the single-attribute nodes, possibly pruned by detset
  # constraints
  lhs_attr_nodes <- to_nodes(seq_len(n_lhs_attrs), nodes)
  initial_seeds <- lhs_attr_nodes
  seeds <- generate_next_seeds(
    max_non_deps = integer(),
    min_deps = integer(),
    initial_seeds = lhs_attr_nodes,
    nodes = nodes,
    detset_limit = detset_limit
  )
  min_deps <- integer()
  max_non_deps <- integer()
  trace <- integer()
  bijection_nodes <- to_nodes(
    match(
      bijection_candidate_nonfixed_indices,
      lhs_nonfixed_indices
    ),
    nodes
  )

  while (length(seeds) != 0) {
    node <- seeds[sample.int(length(seeds), 1)]
    while (!is.na(node)) {
      if (nodes$visited[node]) {
        if (nodes$category[node] == 3) { # dependency
          min_infer <- is_minimal(node, nodes)
          if (isTRUE(min_infer)) {
            nodes$category[node] <- 2L
            min_deps <- c(min_deps, node)
            if (is.element(node, bijection_nodes)) {
              lhs_index <- lhs_nonfixed_indices[nodes$bits[[node]]]
              stopifnot(is.element(
                lhs_index,
                bijection_candidate_nonfixed_indices
              ))
              if (store_cache)
                return(list(lhs_index, partitions, TRUE))
              else
                return(list(lhs_index, TRUE))
            }
          }
          if (isFALSE(min_infer))
            nodes$category[node] <- 1L
        }
        if (nodes$category[node] == -3) { # non-dependency
          max_infer <- is_maximal(node, nodes)
          if (isTRUE(max_infer)) {
            nodes$category[node] <- -2L
            max_non_deps <- c(max_non_deps, node)
          }
          if (isFALSE(max_infer)) {
            nodes$category[node] <- -1L
          }
        }
        nodes$category[node] <- update_dependency_type(
          node,
          nodes,
          min_deps,
          max_non_deps
        )
      }else{
        inferred_type <- infer_type(node, nodes)
        if (!is.na(inferred_type)) {
          nodes$category[node] <- inferred_type
        }
        if (nodes$category[node] == 0L) {
          lhs_set <- lhs_nonfixed_indices[nodes$bits[[node]]]
          cp <- compute_partitions(
            rhs,
            lhs_set,
            partitions
          )
          result <- cp[[1]]
          partitions <- cp[[2]]
          if (result) {
            min_infer <- is_minimal(node, nodes)
            if (isFALSE(min_infer))
              nodes$category[node] <- 1L
            if (isTRUE(min_infer)) {
              min_deps <- c(min_deps, node)
              nodes$category[node] <- 2L
              if (is.element(node, bijection_nodes)) {
                lhs_index <- lhs_nonfixed_indices[nodes$bits[[node]]]
                stopifnot(is.element(
                  lhs_index,
                  bijection_candidate_nonfixed_indices
                ))
                if (store_cache)
                  return(list(lhs_index, partitions, TRUE))
                else
                  return(list(lhs_index, TRUE))
              }
            }
            if (is.na(min_infer))
              nodes$category[node] <- 3L
          }else{
            max_infer <- is_maximal(node, nodes)
            if (isFALSE(max_infer)) {
              nodes$category[node] <- -1L
            }
            if (isTRUE(max_infer)) {
              max_non_deps <- c(max_non_deps, node)
              nodes$category[node] <- -2L
            }
            if (is.na(max_infer))
              nodes$category[node] <- -3L
          }
        }
        nodes$visited[node] <- TRUE
      }
      res <- pick_next_node(
        node,
        nodes,
        trace,
        min_deps,
        max_non_deps
      )
      trace <- res[[2]]
      nodes <- res[[3]]
      min_deps <- res[[4]]
      max_non_deps <- res[[5]]
      node <- res[[1]]
    }
    seeds <- generate_next_seeds(
      max_non_deps,
      min_deps,
      initial_seeds,
      nodes,
      detset_limit
    )
  }
  if (store_cache)
    list(
      lapply(min_deps, \(md) lhs_nonfixed_indices[nodes$bits[[md]]]),
      partitions,
      FALSE
    )
  else
    list(
      lapply(min_deps, \(md) lhs_nonfixed_indices[nodes$bits[[md]]]),
      FALSE
    )
}

pick_next_node <- function(node, nodes, trace, min_deps, max_non_deps) {
  if (nodes$category[node] == 3) { # candidate dependency
    s <- nonvisited_children(node, nodes)
    s <- remove_pruned_subsets(s, min_deps, nodes$bits)
    if (length(s) == 0) {
      min_deps <- c(min_deps, node)
      nodes$category[node] <- 2L
    }else{
      trace <- c(node, trace)
      return(list(min(s), trace, nodes, min_deps, max_non_deps))
    }
  }else if (nodes$category[node] == -3) { # candidate non-dependency
    s <- nonvisited_parents(node, nodes)
    s <- remove_pruned_supersets(s, max_non_deps, nodes$bits)
    if (length(s) == 0) {
      max_non_deps <- c(max_non_deps, node)
      nodes$category[node] <- -2L
    }else{
      trace <- c(node, trace)
      return(list(min(s), trace, nodes, min_deps, max_non_deps))
    }
  }
  if (length(trace) == 0)
    return(list(NA, trace, nodes, min_deps, max_non_deps))
  list(trace[1], trace[-1], nodes, min_deps, max_non_deps)
}

remove_pruned_subsets <- function(subsets, supersets, bitsets) {
  if (length(subsets) == 0 || length(supersets) == 0)
    return(subsets)
  check_pairs <- outer(
    bitsets[subsets],
    bitsets[supersets],
    Vectorize(is_subset)
  )
  prune <- apply(check_pairs, 1, any)
  subsets[!prune]
}

remove_pruned_supersets <- function(supersets, subsets, bitsets) {
  if (length(subsets) == 0 || length(supersets) == 0)
    return(supersets)
  check_pairs <- outer(
    bitsets[supersets],
    bitsets[subsets],
    Vectorize(is_superset)
  )
  prune <- rowSums(check_pairs) > 0
  supersets[!prune]
}

generate_next_seeds <- function(
  max_non_deps,
  min_deps,
  initial_seeds,
  nodes,
  detset_limit
) {
  # Seed generation assumes that the empty set is known to be a non-determinant.
  # The below is equivalent to beginning with a single empty seed, and having
  # the empty set as an additional non-determinant. Being able to refer to the
  # empty set directly would remove the special cases we have below for
  # max_non_deps being empty, and for applying the first one.
  # Nodes are completely unknown iff they're non-visited
  stopifnot(
    all(nodes$visited == (nodes$category != 0)),
    !any(abs(nodes$category) == 3)
  )
  if (length(max_non_deps) == 0) {
    # original DFD paper doesn't mention case where no maximal non-dependencies
    # found yet, so this approach could be inefficient
    seeds <- remove_pruned_subsets(
      initial_seeds,
      min_deps,
      nodes$bits
    )
    # At generation time, all seed nodes are non-categorised, and nodes in
    # general are non-categorised or minimal dependencies, because to be in this
    # case the only explored nodes must be single-attribute nodes found to be
    # minimal. i.e.:
    stopifnot(
      all(nodes$category[seeds] == 0),
      all(nodes$category %in% c(0L, 2L))
    )
  }else{
    seeds <- integer()
    for (n in seq_along(max_non_deps)) {
      nfd <- max_non_deps[[n]]
      max_non_dep_c <- remove_pruned_subsets(initial_seeds, nfd, nodes$bits)
      # paper condition is "seeds is empty", trimming cross-intersections
      # by detset_limit means seeds can be empty before the end,
      # which would cause the remaining nfds to start from scratch.
      # we therefore just initiate the seeds on the first nfd, as
      # intended anyway.
      if (n == 1) {
        seeds <- max_non_dep_c
      }else{
        seeds <- cross_intersection(seeds, max_non_dep_c, nodes, detset_limit)
      }
    }
  }
  remove_pruned_supersets(seeds, min_deps, nodes$bits)
}

cross_intersection <- function(seeds, max_non_dep, powerset, detset_limit) {
  new_seed_full_indices <- integer()
  for (dep in seeds) {
    seed_bitset <- powerset$bits[[dep]]
    for (set in max_non_dep) {
      set_bitset <- powerset$bits[[set]]
      new_seed_bitset <- seed_bitset | set_bitset
      if (sum(new_seed_bitset) > detset_limit)
        next
      new_seed_bitset_index <- to_bitset_index(which(new_seed_bitset))
      new_seed_full_indices <- c(new_seed_full_indices, new_seed_bitset_index)
    }
  }
  seeds <- powerset$bitset_index[new_seed_full_indices]
  seeds <- seeds[!is.na(seeds)]
  minimise_seeds(seeds, powerset$bits)
}

to_bitset_index <- function(bits) {
  sum(2^(bits - 1))
}

minimise_seeds <- function(seeds, bitsets) {
  if (length(seeds) == 0)
    return(seeds)
  # remove duplicates first instead of comparing identical bitsets
  unique_seeds <- unique(seeds)
  n_seeds <- length(unique_seeds)
  include <- rep(TRUE, length(unique_seeds))
  for (n in seq_len(n_seeds - 1)) {
    if (include[n]) {
      for (m in seq.int(n + 1L, n_seeds)) {
        if (include[[m]]) {
          if (is_subset(bitsets[[unique_seeds[n]]], bitsets[[unique_seeds[m]]]))
            include[m] <- FALSE
          else{
            if (is_superset(bitsets[[unique_seeds[n]]], bitsets[[unique_seeds[m]]])) {
              include[n] <- FALSE
              break
            }
          }
        }
      }
    }
  }
  unique_seeds[include]
}

partition_computer <- function(df, accuracy, cache) {
  threshold <- ceiling(nrow(df)*accuracy)

  partitions_ui <- list(
    # we could use the partkey directly as an index into a list of
    # pre-allocated length, but this often requires a very large list that is
    # slow to assign elements in, so we stick to matching on a growing list
    # here.
    # It would also require the partkey to be representable as an integer,
    # rather than a double, which introduces a tighter constraint on the maximum
    # number of columns df can have (nonfixed attrs instead of just LHS attrs).
    # "Partition nodes" in this UI refer to IDs within the partition: these are
    # different to those used in find_LHSs and powersets.
    add_partition = function(partition_node, val, partitions) {
      partitions$key <- c(partitions$key, partition_node)
      partitions$value <- c(partitions$value, list(val))
      partitions
    },
    get_with_index = function(index, partitions) {
      partitions$value[[index]]
    },
    lookup_node = function(partition_node, partitions) {
      match(partition_node, partitions$key)
    }
  )

  check_FD_partition <- if (cache)
    function(attr_indices, df, partitions)
      check_FD_partition_stripped(attr_indices, df, partitions, partitions_ui)
  else
    function(attr_indices, df, partitions)
      check_FD_partition_nclass(attr_indices, df, partitions, partitions_ui)

  if (threshold < nrow(df)) {
    check_AD <- if (cache)
      function(df, rhs, lhs_set, partitions, threshold, limit)
        check_AD_cache(df, rhs, lhs_set, partitions, threshold, limit, partitions_ui)
    else
      function(df, rhs, lhs_set, partitions, threshold, limit)
        check_AD_nocache(df, rhs, lhs_set, partitions, threshold, limit, partitions_ui)
    function(rhs, lhs_set, partitions) {
      approximate_dependencies(
        df,
        rhs,
        lhs_set,
        partitions,
        threshold,
        check_FD_partition,
        check_AD
      )
    }
  }else
    function(rhs, lhs_set, partitions) {
      exact_dependencies(
        df,
        rhs,
        lhs_set,
        partitions,
        check_FD_partition
      )
    }
}

exact_dependencies <- function(
  df,
  rhs,
  lhs_set,
  partitions,
  check_FD_partition
) {
  res1 <- check_FD_partition(lhs_set, df, partitions)
  part_lhs <- res1[[1]]
  partitions <- res1[[2]]
  if (part_lhs == 0)
    return(list(TRUE, partitions))
  res2 <- check_FD_partition(union(lhs_set, rhs), df, partitions)
  part_union <- res2[[1]]
  partitions <- res2[[2]]
  list(part_union == part_lhs, partitions)
}

approximate_dependencies <- function(
  df,
  rhs,
  lhs_set,
  partitions,
  threshold,
  check_FD_partition,
  check_AD
) {
  limit <- nrow(df) - threshold
  # cheaper bounds checks:
  # nrow(df) - (part_lhs - part_union) <= majorities_total <= nrow(df) - part_lhs
  res1 <- check_FD_partition(lhs_set, df, partitions)
  part_lhs <- res1[[1]]
  partitions <- res1[[2]]
  if (part_lhs <= limit)
    return(list(TRUE, partitions))
  res2 <- check_FD_partition(union(lhs_set, rhs), df, partitions)
  part_union <- res2[[1]]
  partitions <- res2[[2]]
  if (part_lhs - part_union > limit)
    return(list(FALSE, partitions))

  check_AD(df, rhs, lhs_set, partitions, threshold, limit)
}

check_FD_partition_nclass <- function(
  attr_indices,
  df,
  partitions,
  partitions_ui
) {
  # This only returns the number |p| of equivalence classes in the partition p,
  # not its contents. This is less demanding on memory than storing stripped
  # partitions, but we cannot efficiently calculate the partition for supersets.
  partition_node <- to_partition_node(attr_indices)
  partkey <- partitions_ui$lookup_node(partition_node, partitions)
  if (!is.na(partkey)) {
    return(list(partitions_ui$get_with_index(partkey, partitions), partitions))
  }
  df_attrs_only <- df[, attr_indices, drop = FALSE]
  n_remove <- sum(duplicated(df_attrs_only))
  partitions <- partitions_ui$add_partition(partition_node, n_remove, partitions)
  list(n_remove, partitions)
}

check_FD_partition_stripped <- function(
  attr_indices,
  df,
  partitions,
  partitions_ui
) {
  attr_nodes <- to_partition_node(attr_indices)
  partition_node <- sum(attr_nodes)
  partkey <- partitions_ui$lookup_node(partition_node, partitions)
  if (!is.na(partkey)) {
    sp <- partitions_ui$get_with_index(partkey, partitions)
    return(list(sum(lengths(sp)) - length(sp), partitions))
  }
  subset_nodes <- partition_node - attr_nodes
  subsets_match <- vapply(
    subset_nodes,
    partitions_ui$lookup_node,
    integer(1L),
    partitions
  )
  if (sum(!is.na(subsets_match)) >= 2) {
    indices <- which(!is.na(subsets_match))[1:2]
    sp <- stripped_partition_product(
      partitions_ui$get_with_index(subsets_match[indices[[1]]], partitions),
      partitions_ui$get_with_index(subsets_match[indices[[2]]], partitions),
      nrow(df)
    )
  }else{
    if (sum(!is.na(subsets_match)) == 1) {
      index <- which(!is.na(subsets_match))
      small_subset <- attr_indices[[index]]
      small_subset_node <- attr_nodes[[index]]
      main_partition <- partitions_ui$get_with_index(
        subsets_match[index],
        partitions
      )
      subres <- check_FD_partition_stripped(
        small_subset,
        df,
        partitions,
        partitions_ui
      )
      partitions <- subres[[2]]
      small_partition <- partitions_ui$get_with_index(
        partitions_ui$lookup_node(small_subset_node, partitions),
        partitions
      )
      sp <- stripped_partition_product(
        main_partition,
        small_partition,
        nrow(df)
      )
    }else{
      sp <- fsplit_rows(df, attr_indices)
      sp <- unname(sp[lengths(sp) > 1])
    }
  }
  partitions <- partitions_ui$add_partition(partition_node, sp, partitions)
  list(sum(lengths(sp)) - length(sp), partitions)
}

check_AD_cache <- function(
  df,
  rhs,
  lhs_set,
  partitions,
  threshold,
  limit,
  partitions_ui
) {
  # e(lhs_set -> rhs)
  lhs_set_partition_node <- to_partition_node(lhs_set)
  rhs_partition_node <- to_partition_nodes(rhs)
  ind_lhs <- partitions_ui$lookup_node(lhs_set_partition_node, partitions)
  stopifnot(!is.na(ind_lhs))
  classes_lhs <- partitions_ui$get_with_index(ind_lhs, partitions)
  classes_lhs_node <- lhs_set_partition_node + rhs_partition_node
  ind_union <- partitions_ui$lookup_node(classes_lhs_node, partitions)
  stopifnot(!is.na(ind_union))
  classes_union <- partitions_ui$get_with_index(ind_union, partitions)
  e <- 0L
  Ts <- integer()
  for (c in classes_union) {
    Ts[c[1]] <- length(c)
  }
  for (c in classes_lhs) {
    m <- 1L
    for (ts in c) {
      m <- max(m, Ts[ts], na.rm = TRUE)
    }
    e <- e + length(c) - m
  }
  list(e <= limit, partitions)
}

check_AD_nocache <- function(
  df,
  rhs,
  lhs_set,
  partitions,
  threshold,
  limit,
  partitions_ui
) {
  # This is a quick working version I put together to replace the non-working
  # original. The quicker version from Tane requires cache = TRUE for stripped
  # partition information.
  majority_size <- function(x) {
    max(tabulate(x))
  }
  splitted <- df[[rhs]]
  splitter <- df[, lhs_set, drop = FALSE]
  rhs_split <- fsplit(splitted, splitter)
  majorities_total <- sum(vapply(
    rhs_split,
    majority_size,
    integer(1)
  ))
  list(majorities_total >= threshold, partitions)
}

to_partition_node <- function(element_indices) {
  as.integer(sum(2^(element_indices - 1L)))
}

to_partition_nodes <- function(element_indices) {
  as.integer(2^(element_indices - 1L))
}

stripped_partition_product <- function(sp1, sp2, n_rows) {
  # vectorised union of stripped partitions to replace algorithm from Tane
  if (length(sp1) == 0L || length(sp2) == 0L)
    return(list())
  tab <- rep(NA_integer_, n_rows)
  tab[unlist(sp1)] <- rep(seq_along(sp1), lengths(sp1))
  tab2 <- rep(NA_integer_, n_rows)
  tab2[unlist(sp2)] <- rep(seq_along(sp2), lengths(sp2))
  in_both <- which(!is.na(tab) & !is.na(tab2))
  tab_both <- paste(tab[in_both], tab2[in_both])
  # not pre-factoring with specified levels results in split calling
  # order(tab_both), which can be very slow for long tab_both
  tab_both <- factor(tab_both, levels = unique(tab_both), ordered = FALSE)
  sp <- split(in_both, tab_both, drop = TRUE)
  unname(sp[lengths(sp) >= 2])
}

fsplit <- function(splitted, splitter) {
  # Column contents are known to be integer, so we paste them together before
  # calling split. This is much faster than the iterated pasting of multiple f
  # elements done by interaction().
  # splitter is unnamed in case any attributes have names like "sep"
  # that would be used as arguments for split
  single_splitter <- do.call(paste, unname(splitter))
  split(splitted, single_splitter, drop = TRUE)
}

fsplit_rows <- function(df, attr_indices) {
  fsplit(seq_len(nrow(df)), df[, attr_indices, drop = FALSE])
}

add_deps_implied_by_bijections <- function(
  dependencies,
  bijections,
  nonfixed,
  column_names
) {
  for (b in bijections) {
    first_index <- nonfixed[[b[[1]]]]
    # add the bijection
    for (nonfixed_index in b[-1]) {
      replacement <- nonfixed[[nonfixed_index]]
      dependencies[[replacement]] <- c(
        first_index,
        setdiff(dependencies[[first_index]], replacement)
      )
      stopifnot(!anyDuplicated(dependencies[[nonfixed_index]]))
    }
    # add dependencies implied by the bijection
    # only needed when bijection attribute is earlier than dependant, since
    # later ones were added before the bijection was known
    for (rhs in setdiff(seq_along(dependencies), match(nonfixed[b], column_names))) {
      for (nonfixed_index in b[-1]) {
        replacement <- nonfixed[[nonfixed_index]]
        if (match(replacement, column_names) < rhs) {
          dependencies[[rhs]] <- c(
            dependencies[[rhs]],
            lapply(
              Filter(\(d) is.element(first_index, d), dependencies[[rhs]]),
              \(d) c(setdiff(d, first_index), replacement)
            )
          )
        }
        stopifnot(!anyDuplicated(dependencies[[rhs]]))
      }
    }
  }
  dependencies
}

filter_nonflat_dependencies <- function(
  dependencies,
  detset_limit
) {
  lapply(
    dependencies,
    \(x) {
      if (length(x) == 0)
        return(x[FALSE])
      wanted <- lengths(x) <= detset_limit
      x[wanted]
    }
  )
}

flatten <- function(dependencies, attributes) {
  result <- list()
  for (i in seq_along(dependencies)) {
    rhs <- names(dependencies)[i]
    result <- c(
      result,
      lapply(dependencies[[i]], \(lhs) list(lhs, rhs))
    )
  }
  functional_dependency(result, attributes)
}
