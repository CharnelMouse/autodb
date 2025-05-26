#' Dependency discovery with DFD
#'
#' Finds all the minimal functional dependencies represented in a data frame.
#'
#' Column names for \code{\link{df}} must be unique.
#'
#' There are two search algorithms available for finding dependencies: DFD, and FDHits. These are described below.
#'
#' ## DFD
#'
#' The DFD algorithm searches for determinant sets for each dependant attribute
#' separately, by traversing the powerset of the other (non-excluded)
#' attributes. It can roughly be considered as a depth-first search over
#' candidate determinant sets.
#'
#' The implementation for DFD differs a little from the algorithm presented in
#' the original paper:
#' \itemize{
#'   \item Some attributes, or attribute types, can be designated, ahead of
#'   time, as not being candidate members for determinant sets. This reduces the
#'   number of candidate determinant sets to be searched, saving time by not
#'   searching for determinant sets that the user would remove later anyway. The
#'   dependants can also be trimmed in similar fashion, although this reduces
#'   the search space linearly rather than exponentially.
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
#' ## FDHits
#'
#' FDHits begins by sampling pairs of records for attributes in which their
#' values differ (difference sets). These difference sets render some
#' determinant-dependant pairings invalid, pruning the search space. If a
#' candidate pairing is not rendered invalid by the known difference sets, then
#' it's validated, resulting in either confirmation as a minimal functional
#' dependency, or discovery of new difference sets that make it invalid.
#'
#' There are two variants. FDHitsSep performs a search for each dependant attribute
#' separately, in the same way as DFD. FDHitsJoint handles all dependant
#' attributes at the same time.
#'
#' The implementation for FDHits differs a little from the algorithm presented
#' in the original paper:
#' \itemize{
#'   \item Some attributes, or attribute types, can be designated, ahead of
#'   time, as not being candidate members for determinant sets. This reduces the
#'   number of candidate determinant sets to be searched, saving time by not
#'   searching for determinant sets that the user would remove later anyway. The
#'   dependants can also be trimmed in similar fashion.
#'   \item The search can be limited to determinant sets up to a given size.
#'   This is also an option for DFD, but it's more useful for FDHits, due to the
#'   search order.
#'   \item As described in the paper, FDHitsSep and FDHitsJoint branch a search
#'   into several child nodes: \eqn{\mu_0} for FDHitsJoint, and \eqn{\mu_1},
#'   \eqn{\mu_2} etc. for both. The paper implies that the latter nodes are
#'   visited in order. However, this causes the algorithm to not always work
#'   correctly: for guaranteed correctness, they must be visited in reverse
#'   order, with \eqn{\mu_0} able to be visited at any point. This correction is
#'   expected to appear in a future paper.
#'   \item The final algorithm in the paper automatically chooses between
#'   FDHitsSep and FDHits Joint, depending on the number of initially-sampled
#'   difference sets. This is not yet implemented.
#' }
#'
#' ## Floating-point variables
#'
#' Numerical/complex values, i.e. floating-point values, represent difficulties
#' for stating functional dependencies. A fundamental condition for stating
#' functional dependencies is that we can compare two values for the same
#' variable, and they are equivalent or not equivalent.
#'
#' Usually, this is done by checking they're equal -- this is the approach used
#' in \code{discover} -- but we can use any comparison that is an equivalence
#' relation.
#'
#' However, checking floating-point values for equality is not simple. \code{==}
#' is not appropriate, even when comparing non-calculated values we've read from
#' a file, because how a given number is converted into a float can vary by
#' computer architecture, meaning that two values can be considered equal on one
#' computer, and not equal on another. This can happen even if they're both
#' using 64-bit R, and even though all R platforms work with values conforming
#' to the same standard (see \code{\link{double}}). For example,
#' \eqn{8.54917750000000076227} and \eqn{8.54917749999999898591} are converted into
#' different floating-point representations on x86, but the same representation
#' on ARM, resulting in inequality and equality respectively.
#'
#' For this and other reasons, checking numerical/complex values for
#' (near-)equality in R is usually done with \code{\link{all.equal}}. This
#' determines values \eqn{x} and \eqn{y} to be equal if their absolute/relative
#' absolute difference is within some tolerance value. However, we can not use
#' this. Equivalence relations must be transitive: if we have values \eqn{x},
#' \eqn{y}, and \eqn{z}, and \eqn{x} is equivalent to both \eqn{y} and \eqn{z},
#' then \eqn{y} and \eqn{z} must also be equivalent. This tolerance-based
#' equivalence is not transitive: it is reasonably straightforward to set up
#' three values so that the outer values are far enough apart to be considered
#' non-equivalent, but the middle value is close enough to be considered
#' equivalent to both of them. Using this to determine functional dependencies,
#' therefore, could easily result in a large number of inconsistencies.
#'
#' This means we have no good option for comparing numerical/complex values
#' as-is for equivalence, with consistent results across different machines, so
#' we must treat them differently. We have three options:
#'
#' - Round/truncate the values, before comparison, to some low degree of precision;
#' - Coerce the values to another class before passing them into \code{discover};
#' - Read values as characters if reading data from a file.
#'
#' \code{discover} takes the first option, with a default number of significant
#' digits low enough to ensure consistency across different machines. However,
#' the user can also use any of these options when processing the data before
#' passing it to \code{discover}. The third option, in particular, is
#' recommended if reading data from a file.
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
#' different behaviour for different search algorithms, but the result is always
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
#'
#' FDHits implements `detset_limit` more naturally, since it explores
#' determinant sets in increasing set size. Limiting the size is simply a matter
#' of only visiting new nodes if their determinant set is within the given size
#' limit.
#' @param df a data.frame, the relation to evaluate.
#' @param accuracy a numeric in (0, 1]: the accuracy threshold required in order
#'   to conclude a dependency. Accuracy thresholds less than one are only
#'   supported in DFD.
#' @param method a string, indicating which search algorithm to use. Currently,
#'   this defaults to DFD. Alternative options are FDHitsSep and FDHitsJoint.
#' @param digits a positive integer, indicating how many significant digits are
#'   to be used for numeric and complex variables. A value of \code{NA} results
#'   in no rounding. By default, this uses \code{getOption("digits")}, similarly
#'   to \code{\link{format}}. See the "Floating-point variables" section below
#'   for why this rounding is necessary for consistent results across different
#'   machines. See the note in \code{\link{print.default}} about \code{digits >=
#'   16}.
#' @param full_cache a logical, indicating whether to store information about
#'   how sets of attributes group the relation records (stripped partitions).
#'   Otherwise, only the number of groups is stored. Storing the stripped
#'   partition is expected to let the algorithm run more quickly, but might be
#'   inefficient for small data frames or small amounts of memory. Only relevant
#'   for DFD.
#' @param store_cache a logical, indicating whether to keep cached information
#'   to use when finding dependencies for other dependants. This allows the
#'   algorithm to run more quickly by not having to re-calculate information,
#'   but takes up more memory. Only relevant for DFD.
#' @param skip_bijections a logical, indicating whether to skip some dependency
#'   searches that are made redundant by discovered bijections between
#'   attributes. This can significantly speed up the search if \code{df}
#'   contains equivalent attributes early in column order, but results in
#'   undefined behaviour if \code{accuracy < 1}. See Details for more
#'   information. Currently only implemented for DFD.
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
#' DFD: Abedjan Z., Schulze P., Naumann F. (2014) DFD: efficient functional
#' dependency discovery. *Proceedings of the 23rd ACM International Conference
#' on Conference on Information and Knowledge Management (CIKM '14). New York,
#' U.S.A.*, 949--958.
#'
#' FDHits: Bleifuss T., Papenbrock T., Bläsius T., Schirneck M, Naumann F.
#' (2024) Discovering Functional Dependencies through Hitting Set Enumeration.
#' *Proc. ACM Manag. Data*, **2, 1**, 43:1--24.
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
  method = c("DFD", "FDHitsSep", "FDHitsJoint"),
  digits = getOption("digits"),
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
  method <- match.arg(method)
  if (method == "FDHitsSep" && accuracy < 1)
    stop("FDHitsSep can not take accuracy < 1")
  if (method == "FDHitsJoint" && accuracy < 1)
    stop("FDHitsJoint can not take accuracy < 1")

  if (skip_bijections && accuracy < 1)
    warning("skipping bijections when accuracy < 1 can result in incorrect output")

  report <- reporter(progress, progress_file, new = TRUE)

  n_cols <- ncol(df)
  if (n_cols == 0)
    return(functional_dependency(
      stats::setNames(list(), character()),
      attrs_order = character()
    ))
  attr_names <- colnames(df)
  duplicates <- which(duplicated(attr_names))
  if (length(duplicates) > 0) {
    dup_names <- unique(attr_names[duplicates])
    sorted_dup_names <- dup_names[order(match(dup_names, attr_names))]
    stop("duplicate column names: ", toString(sorted_dup_names))
  }
  if (any(!is.element(exclude, attr_names)))
    warning("there are attribute names in exclude not present in df")
  if (any(!is.element(dependants, attr_names)))
    warning("there are attribute names in dependants not present in df")
  dependants <- intersect(attr_names, dependants)
  dependants <- match(dependants, attr_names)

  valid_determinant_name <- !is.element(attr_names, exclude)
  valid_determinant_class <- !vapply(
    df,
    inherits,
    logical(1),
    exclude_class
  )
  valid_determinant_attrs_prefixing <- which(
    valid_determinant_name & valid_determinant_class
  )

  # convert all columns to integers, since they're checked for duplicates more
  # quickly when calculating partitions
  # we must round floating-point/complex columns, since they're otherwise
  # infeasible:
  # - all.equal, i.e. equality by tolerance, isn't transient, so isn't an
  #   equivalence relation; we need such a relation for consistent partitioning
  # - ==, identical, etc., i.e. equality by bit comparison, results in different
  #   results on different machines, e.g. x86 and ARM(?) 64-bit both represent
  #   floats in 64-bit, but x86 represents in 80 bits first and then rounds,
  #   so non-representable numbers get approximated differently, resulting in
  #   different partition results
  if (!is.na(digits))
    report$exp(
      df[] <- lapply(df, format_if_float, digits = digits),
      paste("formatting numerical/complex variables with", digits, "significant digits")
    )
  df <- report$exp(
    lookup_table(df),
    "simplifying data types"
  )

  switch(
    method,
    DFD = DFD(
      df,
      accuracy = accuracy,
      full_cache  = full_cache,
      store_cache = store_cache,
      skip_bijections = skip_bijections,
      determinants = valid_determinant_attrs_prefixing,
      dependants = dependants,
      detset_limit = detset_limit,
      report = report
    ),
    FDHitsSep = FDHits(
      df,
      method = "Sep",
      determinants = valid_determinant_attrs_prefixing,
      dependants = dependants,
      detset_limit = detset_limit,
      report = report
    ),
    FDHitsJoint = FDHits(
      df,
      method = "Joint",
      determinants = valid_determinant_attrs_prefixing,
      dependants = dependants,
      detset_limit = detset_limit,
      report = report
    )
  )
}

format_if_float <- function(x, digits) {
  if ((inherits(x, "numeric") || inherits(x, "complex")))
    format(x, digits = digits, scientific = TRUE)
  else
    x
}

lookup_table <- function(df) {
  lapply(df, lookup_indices) |>
    data.frame(check.names = FALSE)
}

lookup_indices <- function(x) match(x, x)
