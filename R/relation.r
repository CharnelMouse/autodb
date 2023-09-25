#' Relation vectors
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
#' @inheritParams relation_schema
#' @param relations a named list of relations, in the form of two-element lists:
#'   the first element contains a data frame, where the column names are the
#'   attributes in the associated schema, and the second element contains a list
#'   of character vectors, each representing a candidate key.
#'
#' @return A \code{relation} object, containing the list given in
#'   \code{relations}, with \code{attrs_order} stored in an attribute of the
#'   same name. Relation schemas are returned with their keys' attributes sorted
#'   according to the attribute order in \code{attrs_order}, and the keys then
#'   sorted by priority order. Attributes in the data frame are also sorted,
#'   first by order of appearance in the sorted keys, then by order in
#'   \code{attrs_order} for non-prime attributes.
#' @export
relation <- function(relations, attrs_order) {
  stopifnot(is.list(relations))
  stopifnot(is.character(attrs_order))

  stopifnot(all(lengths(relations) == 2L))
  stopifnot(all(vapply(
    relations,
    \(rel) setequal(names(rel), c("df", "keys")),
    logical(1L)
  )))

  col_indices <- lapply(
    relations,
    \(rel) match(names(rel$df), union(unlist(rel$keys), attrs_order))
  )
  stopifnot(!anyNA(unlist(col_indices)))
  stopifnot(all(!vapply(
    col_indices,
    is.unsorted,
    logical(1)
  )))

  stopifnot(all(vapply(
    relations,
    \(rel) {
      all(vapply(
        rel$keys,
        \(key) {
          all(key %in% attrs_order) &&
          all(key %in% names(rel$df)) &&
            !anyDuplicated(rel$df[, key, drop = FALSE])
        },
        logical(1)
      ))
    },
    logical(1)
  )))

  structure(
    relations,
    attrs_order = attrs_order,
    class = "relation"
  )
}

#' @exportS3Method
attrs.relation <- function(x, ...) {
  lapply(x, \(rel) names(rel$df))
}

#' @exportS3Method
attrs_order.relation <- function(x, ...) {
  attr(x, "attrs_order")
}

#' @exportS3Method
keys.relation <- function(x, ...) {
  lapply(x, \(rel) rel$keys)
}

#' @exportS3Method
print.relation <- function(x, max = 10, ...) {
  n_relations <- length(x)
  cat(paste0(
    with_number(n_relations, "relation", "", "s"),
    "\n"
  ))

  cat(with_number(length(attrs_order(x)), "attribute", "", "s"))
  if (length(attrs_order(x)) > 0L)
    cat(":", toString(attrs_order(x)))
  cat("\n")

  for (n in seq_len(min(n_relations, max))) {
    cat(paste0(
      "relation ",
      names(x)[[n]],
      ": ",
      toString(attrs(x)[[n]]),
      "; ",
      with_number(nrow(x[[n]]$df), "record", "", "s"),
      "\n"
    ))
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
insert.relation <- function(x, vals, ...) {
  new_relations <- lapply(
    x,
    \(rel) {
      rel$df <- if (ncol(rel$df) == 0L)
        vals[
          seq_len(nrow(rel$df) + (nrow(vals) >= 1L)),
          names(rel$df),
          drop = FALSE
        ]
      else
        rbind(
          rel$df,
          unique(vals[, names(rel$df), drop = FALSE])
        )
      rel
    }
  )
  keydups <- lapply(
    new_relations,
    \(rel) {
      vapply(
        rel$keys,
        \(key) {
          if (length(key) == 0L) {
            if (nrow(rel$df) == 0L)
              logical()
            else
              c(FALSE, rep(TRUE, nrow(rel$df) - 1L))
          }else{
            duplicated(rel$df[, key, drop = FALSE])
          }
        },
        logical(nrow(rel$df))
      )
    }
  )
  if (any(unlist(keydups))) {
    bad_relation_names <- names(new_relations)[vapply(keydups, any, logical(1))]
    stop(
      "insertion violates key constraints in ",
      with_number(
        length(bad_relation_names),
        "relation",
        "",
        "s"
      ),
      ": ",
      toString(bad_relation_names)
    )
  }
  x[] <- new_relations
  x
}
