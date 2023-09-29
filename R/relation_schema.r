#' Relation schema vectors
#'
#' Creates a set of relation schemas, including the relation's attributes and
#' candidate keys.
#'
#' Duplicate schemas, after ordering by attribute, are allowed, and can be
#' removed with `\code{\link{unique}}`.
#'
#' When several sets of relation schemas are concatenated, their
#' \code{attrs_order} attributes are merged, so as to preserve all of the original
#' attribute orders, if possible. If this is not possible, because the orderings
#' disagree, then the returned value of the \code{attrs_order} attribute is their
#' union instead.
#'
#' @param schemas a named list of schemas, in the form of two-element lists: the
#'   first element contains a character vector of all attributes in the relation
#'   schema, and the second element contains a list of character vectors,
#'   each representing a candidate key.
#' @param attrs_order a character vector, giving the names of all attributes.
#'   These need not be present in \code{schemas}, but all attributes in
#'   \code{schemas} must be present in \code{attrs_order}.
#' @param attrs_class a named list of character vectors, giving the default class
#'   vectors for all attributes. By default, every attribute is assumed to be
#'   logical. These default classes are applied on use of \code{\link{create}},
#'   but get overridden by inserted data under usual R type coercion rules.
#'
#' @return A \code{relation_schema} object, containing the list given in
#'   \code{schemas}, with \code{attrs_order} and \code{attrs_class} stored in an
#'   \code{attrs_order} attribute. Relation schemas are returned with their
#'   keys' attributes sorted according to the attribute order in
#'   \code{attrs_order}, and the keys then sorted by priority order. Attributes
#'   in the schema are also sorted, first by order of appearance in the sorted
#'   keys, then by order in \code{attrs_order} for non-prime attributes.
#'   Elements of \code{attrs_class} are sorted to be in the same order as in
#'   \code{attrs_order}.
#' @seealso \code{\link{attrs}}, \code{\link{keys}}, and \code{\link{attrs_order}}
#'   for extracting parts of the information in a \code{relation_schema}.
#' @export
#' @examples
#' schemas <- relation_schema(
#'   list(a = list(c("a", "b"), list("a")), b = list(c("b", "c"), list("b", "c"))),
#'   attrs_order = c("a", "b", "c", "d")
#' )
#' print(schemas)
#' attrs(schemas)
#' keys(schemas)
#' attrs_order(schemas)
#' names(schemas)
#' attrs_order(schemas) <- c("d", "c", "b", "a")
#' print(schemas)
relation_schema <- function(
  schemas,
  attrs_order,
  attrs_class = stats::setNames(
    rep_len(list("logical"), length(attrs_order)),
    attrs_order
  )
) {
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
  if (!is.character(attrs_order))
    stop("expected character attrs_order")
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
  if (anyDuplicated(attrs_order))
    stop("attrs_order must be unique")
  if (!all(is.element(unlist(schemas, recursive = TRUE), attrs_order)))
    stop("attributes in schema must be present in attrs_order")
  for (s in schemas)
    if (!all(is.element(unlist(s[[2]]), s[[1]])))
      stop("attributes in keys must be present in relation")
  if (length(attrs_class) != length(attrs_order))
    stop("attrs_class must have same length as attrs_order")
  if (!setequal(names(attrs_class), attrs_order))
    stop("attrs_class must have attrs_order as (unordered) names")
  schemas <- lapply(
    schemas,
    \(s) {
      within_sorted_keys <- unique(lapply(s[[2]], \(as) as[order(match(as, attrs_order))]))
      sorted_keys <- within_sorted_keys[keys_order(lapply(
        within_sorted_keys,
        match,
        attrs_order
      ))]
      sorted_attrs <- s[[1]][order(match(s[[1]], attrs_order))]
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
    schemas,
    attrs_class = attrs_class[attrs_order],
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

  cat(with_number(length(attrs_order(x)), "attribute", "", "s"))
  if (length(attrs_order(x)) > 0L)
    cat(":", toString(attrs_order(x)))
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

#' @export
`attrs<-.relation_schema` <- function(x, ..., value) {
  relation_schema(
    stats::setNames(Map(list, value, keys(x)), names(x)),
    attrs_order(x)
  )
}

#' @exportS3Method
keys.relation_schema <- function(x, ...) {
  lapply(unclass(x), `[[`, 2L)
}

#' @exportS3Method
attrs_order.relation_schema <- function(x, ...) {
  names(attrs_class(x))
}

#' @export
`attrs_order<-.relation_schema` <- function(x, ..., value) {
  rels <- unclass(x)
  attributes(rels)[c("attrs_order", "attrs_class")] <- NULL
  relation_schema(
    rels,
    attrs_order = value,
    attrs_class = attrs_class(x)[value]
  )
}

#' @exportS3Method
attrs_class.relation_schema <- function(x, ...) {
  attr(x, "attrs_class")
}

#' @export
`attrs_class<-.relation_schema` <- function(x, ..., value) {
  rels <- unclass(x)
  attributes(rels)[c("attrs_order", "attrs_class")] <- NULL
  relation_schema(
    rels,
    attrs_order = attrs_order(x),
    attrs_class = value
  )
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
  x[!duplicated(x)]
}

#' @exportS3Method
c.relation_schema <- function(...) {
  lst <- list(...)
  joined_schemas <- Reduce(c, lapply(lst, unclass))
  names(joined_schemas) <- if (is.null(names(joined_schemas))) {
    character(length(joined_schemas))
  }else{
    make.unique(names(joined_schemas))
  }

  attrs_order_list <- lapply(lst, attrs_order)
  joined_attrs_order <- do.call(merge_attribute_orderings, attrs_order_list)

  all_classes <- Reduce(c, lapply(lst, attrs_class))
  if (is.null(names(all_classes)))
    names(all_classes) <- character()
  all_classes <- all_classes[!duplicated(Map(
    list,
    unname(all_classes),
    names(all_classes)
  ))]
  if (length(all_classes) != length(joined_attrs_order))
    stop("attrs_class values are not consistent")
  joined_attrs_class <- all_classes[joined_attrs_order]

  relation_schema(joined_schemas, joined_attrs_order, joined_attrs_class)
}

#' @exportS3Method
create.relation_schema <- function(x, ...) {
  relation(
    Map(
      \(df, ks) list(df = df, keys = ks),
      lapply(
        attrs(x),
        \(as) {
          df <- data.frame(stats::setNames(lapply(as, \(x) logical()), as))
          df[] <- Map(
            \(column, classes) {
              # apply classes in reverse order, to allow coercion
              for (cl in rev(seq_along(classes))) {
                if (classes[[cl]] == "factor")
                  column <- factor(column)
                class(column) <- classes[cl:length(classes)]
              }
              column
            },
            df,
            attrs_class(x)[names(df)]
          )
          df
        }
      ),
      keys(x)
    ),
    attrs_order(x)
  )
}
