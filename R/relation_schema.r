#' Relation schema vectors
#'
#' Creates a set of relation schemas, including the relation's attributes and
#' candidate keys.
#'
#' When several sets of relation schemas are concatenated, their
#' \code{all_attrs} attributes are merged, so as to preserve all of the original
#' attribute orders, if possible. If this is not possible, because the orderings
#' disagree, then the returned value of the \code{all_attrs} attribute is their
#' union instead.
#'
#' @param schemas a named list of schemas, in the form of two-element lists: the
#'   first element contains a character vector of all attributes in the relation
#'   schema, and the second element contains a list of character vectors,
#'   each representing a candidate key.
#' @param all_attrs a character vector, giving the names of all attributes.
#'   These need not be present in \code{schemas}, but all attributes in
#'   \code{schemas} must be present in \code{all_attrs}.
#' @param unique a logical, TRUE by default, for whether to remove duplicate
#'   schemas.
#'
#' @return A \code{relation_schema} object, containing the list given in
#'   \code{schemas}, with \code{all_attrs} stored in an attribute of the same
#'   name. Relation schemas are returned with their keys' attributes sorted
#'   according to the attribute order in \code{all_attrs}, and the keys then
#'   sorted by priority order. Attributes in the schema are also sorted, first
#'   by order of appearance in the sorted keys, then by order in
#'   \code{all_attrs} for non-prime attributes.
#' @seealso \code{\link{attrs}}, \code{\link{keys}}, and \code{\link{all_attrs}}
#'   for extracting parts of the information in a \code{relation_schema}.
#' @export
#' @examples
#' schemas <- relation_schema(
#'   list(a = list(c("a", "b"), list("a")), b = list(c("b", "c"), list("b", "c"))),
#'   all_attrs = c("a", "b", "c", "d")
#' )
#' print(schemas)
#' attrs(schemas)
#' keys(schemas)
#' all_attrs(schemas)
#' names(schemas)
relation_schema <- function(schemas, all_attrs, unique = TRUE) {
  if (!all(lengths(schemas) == 2L))
    stop("schema elements must have length two")
  if (!all(vapply(schemas, \(s) is.character(s[[1]]), logical(1))))
    stop("schema attribute sets must be characters")
  if (!all(vapply(schemas, \(s) is.list(s[[2]]), logical(1))))
    stop("schema key sets must be lists")
  if (!all(vapply(schemas, \(s) length(s[[2]]) > 0L, logical(1))))
    stop("schema key sets must have at least one element")
  if (!all(vapply(schemas, \(s) all(vapply(s[[2]], is.character, logical(1))), logical(1))))
    stop("schema key sets must have character elements")
  if (!is.character(all_attrs))
    stop("expected character all_attrs")
  if (!is.character(names(schemas)))
    stop("relations must be named")
  if (anyDuplicated(names(schemas)))
    stop("relation names must be unique")
  if (!all(vapply(schemas, \(s) !anyDuplicated(s[[1]]), logical(1))))
    stop("relation attributes must be unique")
  if (!all(vapply(
    schemas,
    \(s) all(vapply(s[[2]], Negate(anyDuplicated), logical(1))),
    logical(1)
  )))
    stop("relation key attributes must be unique")
  if (anyDuplicated(all_attrs))
    stop("all_attrs must be unique")
  if (!all(is.element(unlist(schemas, recursive = TRUE), all_attrs)))
    stop("attributes in schema must be present in all_attrs")
  for (s in schemas)
    if (!all(is.element(unlist(s[[2]]), s[[1]])))
      stop("attributes in keys must be present in relation")
  schemas <- lapply(
    schemas,
    \(s) {
      within_sorted_keys <- unique(lapply(s[[2]], \(as) as[order(match(as, all_attrs))]))
      sorted_keys <- within_sorted_keys[keys_order(lapply(
        within_sorted_keys,
        match,
        all_attrs
      ))]
      sorted_attrs <- s[[1]][order(match(s[[1]], all_attrs))]
      sorted_attrs <- c(
        unique(unlist(sorted_keys)),
        setdiff(sorted_attrs, unlist(sorted_keys))
      )
      list(
        sorted_attrs,
        sorted_keys
      )
    }
  )
  structure(
    schemas[if (unique) !duplicated(schemas) else rep(TRUE, length(schemas))],
    all_attrs = all_attrs,
    class = "relation_schema"
  )
}

#' @exportS3Method
print.relation_schema <- function(x, max = 10, ...) {
  n_relations <- length(x)
  cat(paste0(
    with_number(n_relations, "relation schema", "", "s"),
    "\n"
  ))

  cat(with_number(length(all_attrs(x)), "attribute", "", "s"))
  if (length(all_attrs(x)) > 0L)
    cat(":", toString(all_attrs(x)))
  cat("\n")

  for (n in seq_len(min(n_relations, max))) {
    cat(paste0("schema ", names(x)[[n]], ": ", toString(attrs(x)[[n]]), "\n"))
    keyset <- keys(x)[[n]]
    n_keys <- length(keyset)
    for (k in seq_len(min(n_keys, max))) {
      cat(paste0("  key ", k, ": ", toString(keyset[[k]]), "\n"))
    }
    if (max < n_keys)
      cat("  ... and", with_number(n_keys - max, "other key", "", "s"), "\n")
  }
  if (max < n_relations) {
    cat("... and", with_number(n_relations - max, "other schema", "", "s"), "\n")
  }
}

#' @exportS3Method
attrs.relation_schema <- function(x, ...) {
  lapply(unclass(x), `[[`, 1L)
}

#' @exportS3Method
keys.relation_schema <- function(x, ...) {
  lapply(unclass(x), `[[`, 2L)
}

#' @exportS3Method
all_attrs.relation_schema <- function(x, ...) {
  attr(x, "all_attrs")
}

#' @export
`[.relation_schema` <- function(x, i) {
  attrs <- attributes(x)
  res <- unclass(x)[i]
  attrs$names <- attrs$names[i]
  attributes(res) <- attrs
  res
}

#' @export
`[[.relation_schema` <- function(x, i) {
  if (length(i) == 0L)
    stop("attempt to select less than one element")
  if (length(i) > 1L)
    stop("attempt to select more than one element")
  x[i]
}

#' @exportS3Method
unique.relation_schema <- function(x, ...) {
  relation_schema(unclass(x), all_attrs(x), unique = TRUE)
}

#' @exportS3Method
c.relation_schema <- function(..., unique = TRUE) {
  lst <- list(...)
  joined_schemas <- Reduce(c, lapply(lst, unclass))
  names(joined_schemas) <- if (is.null(names(joined_schemas)))
    character(length(joined_schemas))
  else
    make.unique(names(joined_schemas))

  all_attrs_list <- lapply(lst, all_attrs)
  joined_all_attrs <- do.call(merge_attribute_orderings, all_attrs_list)

  relation_schema(joined_schemas, joined_all_attrs, unique = unique)
}
