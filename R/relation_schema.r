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
#'
#' @return A \code{relation_schema} object, containing the list given in
#'   \code{schemas}, with \code{attrs_order} stored in an attribute of the same
#'   name. Relation schemas are returned with their keys' attributes sorted
#'   according to the attribute order in \code{attrs_order}, and the keys then
#'   sorted by priority order. Attributes in the schema are also sorted, first
#'   by order of appearance in the sorted keys, then by order in
#'   \code{attrs_order} for non-prime attributes.
#' @seealso \code{\link{attrs}}, \code{\link{keys}}, and
#'   \code{\link{attrs_order}} for extracting parts of the information in a
#'   \code{relation_schema}; \code{\link{create}} for creating a
#'   \code{\link{relation}} object that uses the given schema; \code{\link{gv}}
#'   for converting the schema into Graphviz code; \code{\link{rename_attrs}}
#'   for renaming the attributes in \code{attrs_order};
#'   \code{\link{merge_empty_keys}} for combining relations with an empty key;
#'   \code{\link{merge_schemas}} for combining relations with matching sets of
#'   keys.
#' @export
#' @examples
#' schemas <- relation_schema(
#'   list(
#'     a = list(c("a", "b"), list("a")),
#'     b = list(c("b", "c"), list("b", "c"))
#'   ),
#'   attrs_order = c("a", "b", "c", "d")
#' )
#' print(schemas)
#' attrs(schemas)
#' keys(schemas)
#' attrs_order(schemas)
#' names(schemas)
#'
#' # vector operations
#' schemas2 <- relation_schema(
#'   list(
#'     e = list(c("a", "e"), list("e"))
#'   ),
#'   attrs_order = c("a", "e")
#' )
#' c(schemas, schemas2) # attrs_order attributes are merged
#' unique(c(schemas, schemas))
#'
#' # subsetting
#' schemas[1]
#' schemas[c(1, 2, 1)]
#' stopifnot(identical(schemas[[1]], schemas[1]))
#'
#' # reassignment
#' schemas3 <- schemas
#' schemas3[2] <- relation_schema(
#'   list(d = list(c("d", "c"), list("d"))),
#'   attrs_order(schemas3)
#' )
#' print(schemas3) # note the schema's name doesn't change
#' # names(schemas3)[2] <- "d" # this would change the name
#' keys(schemas3)[[2]] <- list(character()) # removing keys first...
#' attrs(schemas3)[[2]] <- c("b", "c") # so we can change the attrs legally
#' keys(schemas3)[[2]] <- list("b", "c") # add the new keys
#' stopifnot(identical(schemas3, schemas))
#'
#' # changing appearance priority for attributes
#' attrs_order(schemas3) <- c("d", "c", "b", "a")
#' print(schemas3)
#'
#' # reconstructing from components
#' schemas_recon <- relation_schema(
#'   Map(list, attrs(schemas), keys(schemas)),
#'   attrs_order(schemas)
#' )
#' stopifnot(identical(schemas_recon, schemas))
#'
#' # can be a data frame column
#' data.frame(id = 1:2, schema = schemas)
relation_schema <- function(
  schemas,
  attrs_order
) {
  stop_with_elements_if(
    lengths(schemas) != 2L,
    "schema elements must have length two"
  )
  stop_with_elements_if(
    !vapply(schemas, \(s) is.character(s[[1]]), logical(1)),
    "schema attribute sets must be characters"
  )
  stop_with_elements_if(
    !vapply(schemas, \(s) is.list(s[[2]]), logical(1)),
    "schema key sets must be lists"
  )
  stop_with_elements_if(
    !vapply(schemas, \(s) length(s[[2]]) > 0L, logical(1)),
    "schema key sets must have at least one element"
  )
  stop_with_values_if(
    unlist(lapply(
      seq_along(schemas),
      \(n) paste(n, seq_along(schemas[[n]][[2]]), sep = ".")
    )),
    unlist(lapply(
      schemas,
      \(s) vapply(s[[2]], Negate(is.character), logical(1))
    )),
    "schema key sets must have character elements"
  )
  if (!is.character(attrs_order))
    stop("expected character attrs_order")
  check_schema_names(names(schemas))
  stop_with_elements_if(
    !vapply(schemas, \(s) !anyDuplicated(s[[1]]), logical(1)),
    "relation attributes must be unique"
  )
  stop_with_values_if(
    unlist(lapply(
      seq_along(schemas),
      \(n) lapply(
        seq_along(schemas[[n]][[2]]),
        \(m) paste0(
          n,
          ".",
          m,
          ".{",
          toString(unique(
            schemas[[n]][[2]][[m]][duplicated(schemas[[n]][[2]][[m]])]
          )),
          "}"
        )
      )
    )),
    unlist(lapply(
      seq_along(schemas),
      \(n) vapply(
        seq_along(schemas[[n]][[2]]),
        \(m) as.logical(anyDuplicated(schemas[[n]][[2]][[m]])),
        logical(1)
      )
    )),
    "relation key attributes must be unique"
  )
  stop_with_values_if(
    attrs_order,
    duplicated(attrs_order),
    "attrs_order must be unique",
    "duplicated",
    suffix_else = "",
    unique = TRUE
  )
  not_ordered <- setdiff(unlist(lapply(schemas, `[[`, 1)), attrs_order)
  stop_with_values_if(
    not_ordered,
    rep(TRUE, length(not_ordered)),
    "attributes in schema must be present in attrs_order",
    "absent",
    suffix_else = "",
    unique = TRUE
  )
  keys_minus_attrs <- as.character(unlist(lapply(
    seq_along(schemas),
    \(n) {
      paste0(
        n,
        ".{",
        toString(setdiff(unlist(schemas[[n]][[2]]), schemas[[n]][[1]])),
        "}"
      )
    }
  )))
  stop_with_values_if(
    keys_minus_attrs,
    grepl("\\{.+\\}", keys_minus_attrs),
    "attributes in keys must be present in relation"
  )
  relation_schema_nocheck(schemas, attrs_order)
}

relation_schema_nocheck <- function(schemas, attrs_order) {
  schemas <- lapply(
    schemas,
    \(s) {
      within_sorted_keys <- unique(lapply(
        s[[2]],
        \(as) as[order(match(as, attrs_order))]
      ))
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
    attrs_order = attrs_order,
    class = "relation_schema"
  )
}

#' @exportS3Method
attrs.relation_schema <- function(x, ...) {
  lapply(unclass(x), `[[`, 1L)
}

#' @export
`attrs<-.relation_schema` <- function(x, ..., value) {
  if (any(mapply(\(ks, val) any(!is.element(unlist(ks), val)), keys(x), value)))
    stop("attrs reassignments must keep attributes used in keys")
  relation_schema(
    stats::setNames(Map(list, value, keys(x)), names(x)),
    attrs_order(x)
  )
}

#' @exportS3Method
rename_attrs.relation_schema <- function(x, names, ...) {
  old <- attrs_order(x)
  new_attrs <- lapply(attrs(x), \(as) names[match(as, old)])
  new_keys <- lapply(keys(x), lapply, \(as) names[match(as, old)])
  relation_schema(
    stats::setNames(Map(list, new_attrs, new_keys), names(x)),
    names
  )
}

#' @exportS3Method
keys.relation_schema <- function(x, ...) {
  lapply(unclass(x), `[[`, 2L)
}

#' @export
`keys<-.relation_schema` <- function(x, ..., value) {
  relation_schema(
    stats::setNames(Map(list, attrs(x), value), names(x)),
    attrs_order(x)
  )
}

#' @exportS3Method
attrs_order.relation_schema <- function(x, ...) {
  attr(x, "attrs_order")
}

#' @export
`attrs_order<-.relation_schema` <- function(x, ..., value) {
  relation_schema(
    unclass(x),
    attrs_order = value
  )
}

#' @export
`names<-.relation_schema` <- function(x, value) {
  check_schema_names(value)
  attr(x, "names") <- value
  x
}

check_schema_names <- function(nms) {
  if (!is.character(nms))
    stop("relation schemas must be named")
  stop_with_values_if(
    nms,
    duplicated(nms),
    "relation schema names must be unique",
    "duplicated",
    suffix_else = ""
  )
  stop_with_elements_if(
    nms == "",
    "relation schema names must be non-empty"
  )
}

#' @exportS3Method
create.relation_schema <- function(x, ...) {
  relation_nocheck(
    Map(
      \(df, ks) list(df = df, keys = ks),
      lapply(
        attrs(x),
        \(as) data.frame(
          stats::setNames(lapply(as, \(x) logical()), as),
          check.names = FALSE
        )
      ),
      keys(x)
    ),
    attrs_order(x)
  )
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

  relation_schema(joined_schemas, joined_attrs_order)
}

#' @exportS3Method
merge_schemas.relation_schema <- function(x, to_remove, merge_into, ...) {
  stopifnot(length(to_remove) == length(merge_into))

  for (n in seq_along(to_remove)[to_remove != merge_into]) {
    stopifnot(identical(
      unname(keys(x[[to_remove[[n]]]])),
      unname(keys(x[[merge_into[[n]]]]))
    ))
    attrs(x)[[merge_into[[n]]]] <- unique(unlist(attrs(x[c(
      merge_into[[n]],
      to_remove[[n]]
    )])))
  }

  x[-to_remove]
}

#' @export
`[.relation_schema` <- function(x, i) {
  attrs <- attributes(x)
  indices <- stats::setNames(seq_along(x), names(x))
  taken <- indices[i]
  if (anyNA(taken))
    stop("subset names that don't exist: ", toString(i[is.na(taken)]))
  res <- unclass(x)[taken]
  attrs$names <- make.unique(unname(stats::setNames(nm = attrs$names)[taken]))
  attributes(res) <- attrs
  res
}

#' @export
`[<-.relation_schema` <- function(x, i, value) {
  check_reassignment_same_class(value, x)
  as <- attrs(x)
  ks <- keys(x)
  as[i] <- attrs(value)
  ks[i] <- keys(value)
  ao <- merge_attribute_orderings(attrs_order(x), attrs_order(value))
  relation_schema(Map(list, as, ks), ao)
}

#' @export
`[[.relation_schema` <- function(x, i) {
  indices <- stats::setNames(seq_along(x), names(x))
  taken <- try(indices[[i]], silent = TRUE)
  if (class(taken)[[1]] == "try-error")
    stop(attr(taken, "condition")$message)
  x[taken]
}

#' @export
`[[<-.relation_schema` <- function(x, i, value) {
  indices <- stats::setNames(seq_along(x), names(x))
  taken <- try(indices[[i]], silent = TRUE)
  if (class(taken)[[1]] == "try-error")
    stop(attr(taken, "condition")$message)
  x[taken] <- value
  x
}

#' @export
`$.relation_schema` <- function(x, name) {
  x[[name]]
}

#' @export
`$<-.relation_schema` <- function(x, name, value) {
  check_reassignment_same_class(value, x)
  pos <- match(name, names(x))
  if (is.na(pos))
    c(x, stats::setNames(value, name))
  else
    append(x[-pos], stats::setNames(value, name), pos)
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
format.relation_schema <- function(x, ...) {
  paste("schema", names(x))
}

#' @exportS3Method
as.data.frame.relation_schema <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  ...,
  nm = deparse1(substitute(x))[[1L]]
) {
  res <- data.frame(a = seq_along(x))[, FALSE, drop = FALSE]
  res$x <- x
  names(res) <- NULL
  if (!optional)
    names(res) <- nm
  res
}
