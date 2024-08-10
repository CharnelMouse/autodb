#' Relation vectors
#'
#' Creates a set of relation schemas, including the relation's attributes and
#' candidate keys.
#'
#' Relation vectors are unlikely to be needed by the user directly, since they
#' are essentially \code{\link{database}} objects that can't have foreign key
#' references. They are mostly used to mirror the use of the vector-like
#' \code{\link{relation_schema}} class for the \code{\link{database_schema}}
#' class to be a wrapper around. This makes creating a \code{\link{database}}
#' from a \code{\link{relation_schema}} a two-step process, where the two steps
#' can be done in either order: creation with \code{\link{create}} and
#' \code{\link{insert}}, and adding references with
#' \code{\link{database_schema}} or \code{\link{database}}.
#'
#' Duplicate schemas, after ordering by attribute, are allowed, and can be
#' removed with \code{\link{unique}}.
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

  if (anyDuplicated(attrs_order))
    stop("attrs_order must be unique")
  check_relation_names(names(relations))
  if (!all(vapply(relations, \(r) !anyDuplicated(names(r$df)), logical(1))))
    stop("relation attributes must be unique")
  if (!all(vapply(
    relations,
    \(r) all(vapply(r$keys, Negate(anyDuplicated), logical(1))),
    logical(1)
  )))
    stop("relation key attributes must be unique")
  if (!all(lengths(relations) == 2L))
    stop("relation elements must have length two")
  correct_element_names <- vapply(
    relations,
    \(rel) setequal(names(rel), c("df", "keys")),
    logical(1L)
  )
  if (!all(correct_element_names))
    stop(paste(
      "relations must contain 'df' and 'keys' elements:",
      toString(names(relations)[!correct_element_names])
    ))
  if (!all(vapply(relations, \(rel) is.data.frame(rel$df), logical(1)))) {
    stop("relation 'df' elements must be data frames")
  }
  if (!all(vapply(relations, \(rel) inherits(rel$keys, "list"), logical(1)))) {
    stop("relation 'keys' elements must be lists")
  }

  # sort key contents and keys
  relations[] <- lapply(
    relations,
    \(rel) {
      sorted_keys <- unique(lapply(rel$keys, \(k) k[order(match(k, attrs_order))]))
      key_indices <- lapply(sorted_keys, match, attrs_order)
      ord <- keys_order(key_indices)
      rel$keys <- sorted_keys[ord]
      rel
    }
  )

  # sort attributes
  col_order_indices <- lapply(
    relations,
    \(rel) match(names(rel$df), attrs_order)
  )
  if (anyNA(unlist(col_order_indices)))
    stop("relation attributes not in attrs_order")
  col_indices <- lapply(
    relations,
    \(rel) match(names(rel$df), union(unlist(rel$keys), attrs_order))
  )
  relations[] <- Map(
    \(rel, indices) {
      rel$df <- rel$df[, order(indices), drop = FALSE]
      rel
    },
    relations,
    col_indices
  )

  if (!all(vapply(
    relations,
    \(rel) {
      all(vapply(
        rel$keys,
        \(key) all(key %in% names(rel$df)),
        logical(1)
      ))
    },
    logical(1)
  )))
    stop("relation keys must be within relation attributes")
  if (!all(vapply(
    relations,
    \(rel) {
      all(vapply(
        rel$keys,
        \(key) {
          if (length(key) == 0)
            nrow(rel$df) <= 1
          else
            !anyDuplicated(rel$df[, key, drop = FALSE])
        },
        logical(1)
      ))
    },
    logical(1)
  )))
    stop("relations must satisfy their keys")

  relation_nocheck(relations, attrs_order)
}

relation_nocheck <- function(relations, attrs_order) {
  structure(
    relations,
    attrs_order = attrs_order,
    class = "relation"
  )
}

#' @exportS3Method
attrs.relation <- function(x, ...) {
  lapply(records(x), names)
}

#' @export
`attrs<-.relation` <- function(x, ..., value) {
  if (any(mapply(\(as, val) any(!is.element(val, as)), attrs(x), value)))
    stop("attrs reassignments for relational data objects can not add attributes")
  records(x) <- Map(\(df, as) df[, as, drop = FALSE], records(x), value)
  x
}

#' @exportS3Method
attrs_order.relation <- function(x, ...) {
  attr(x, "attrs_order")
}

#' @export
`attrs_order<-.relation` <- function(x, ..., value) {
  relation(unclass(x), value)
}

#' @exportS3Method
rename_attrs.relation <- function(x, names, ...) {
  old <- attrs_order(x)
  new_records <- lapply(
    records(x),
    \(df) stats::setNames(df, names[match(names(df), old)])
  )
  new_keys <- lapply(keys(x), lapply, \(as) names[match(as, old)])
  relation(
    stats::setNames(
      Map(\(recs, ks) list(df = recs, keys = ks), new_records, new_keys),
      names(x)
    ),
    names
  )
}

#' @export
`names<-.relation` <- function(x, value) {
  check_relation_names(value)
  attr(x, "names") <- value
  x
}

check_relation_names <- function(nms) {
  if (!is.character(nms))
    stop("relations must be named")
  if (anyDuplicated(nms))
    stop("relation names must be unique")
  if (any(nms == ""))
    stop("relation names must be non-empty")
}

#' @exportS3Method
keys.relation <- function(x, ...) {
  lapply(unclass(x), \(rel) rel$keys)
}

#' @export
`keys<-.relation` <- function(x, ..., value) {
  relation(
    stats::setNames(
      Map(\(df, ks) list(df = df, keys = ks), records(x), value),
      names(x)
    ),
    attrs_order(x)
  )
}

#' @exportS3Method
records.relation <- function(x, ...) {
  lapply(unclass(x), \(rel) rel$df)
}

#' @export
`records<-.relation` <- function(x, ..., value) {
  value_names <- Map(
    \(df, ks) {
      nms <- names(df)
      nms[order(match(nms, c(unlist(ks), attrs_order(x))))]
    },
    value,
    keys(x)
  )
  keys_kept <- mapply(
    \(nms, ks) {
      all(is.element(unlist(ks), nms))
    },
    value_names,
    keys(x)
  )
  if (!all(keys_kept))
    stop("record reassignments must keep key attributes")
  no_additions <- mapply(
    \(nms, as, ks) {
      all(is.element(setdiff(nms, unlist(ks)), as))
    },
    value_names,
    attrs(x),
    keys(x)
  )
  if (!all(no_additions))
    stop("record reassignments can not add attributes")
  new <- Map(
    \(recs, nms, ks) list(df = recs[, nms, drop = FALSE], keys = ks),
    value,
    value_names,
    keys(x)
  )
  attributes(new) <- attributes(x)
  new
}

#' @exportS3Method
unique.relation <- function(x, ...) {
  x[!duplicated(x)]
}

#' @exportS3Method
duplicated.relation <- function(x, incomparables = FALSE, ...) {
  y <- sort_records(x)
  duplicated(unclass(y))
}

sort_records <- function(relations) {
  res <- relations
  records(res) <- Map(
    \(df, ks) {
      if (nrow(df) <= 1)
        df
      else # 2+ rows => non-empty keys
        `rownames<-`(
          df[
            do.call(order, df[, ks[[1]], drop = FALSE]),
            ,
            drop = FALSE
          ],
          NULL
        )
    },
    records(res),
    keys(res)
  )
  res
}

#' @exportS3Method
c.relation <- function(...) {
  lst <- list(...)
  joined_rels <- Reduce(c, lapply(lst, unclass))
  names(joined_rels) <- if (is.null(names(joined_rels))) {
    character(length(joined_rels))
  }else{
    make.unique(names(joined_rels))
  }

  attrs_order_list <- lapply(lst, attrs_order)
  joined_attrs_order <- do.call(merge_attribute_orderings, attrs_order_list)

  relation(joined_rels, joined_attrs_order)
}

#' @exportS3Method
insert.relation <- function(x, vals, relations = names(x), ...) {
  if (any(!is.element(relations, names(x))))
    stop("given relations must exist")
  if (anyDuplicated(relations))
    stop("given relations must be unique")
  if (length(relations) == 0)
    return(x)
  extra <- setdiff(names(vals), attrs_order(x))
  if (length(extra) > 0L)
    stop(paste(
      "inserted attributes aren't included in target:",
      toString(extra)
    ))
  new_records <- records(x)
  new_records[relations] <- lapply(
    new_records[relations],
    \(df) {
      if (!all(is.element(names(df), names(vals))))
        return(df)
      df <- if (nrow(df) == 0L) {
        if (ncol(df) == 0L)
          vals[seq_len(nrow(vals) > 0L), character(), drop = FALSE]
        else
          unique(vals[, names(df), drop = FALSE])
      }else{
        if (ncol(df) == 0L)
          vals[
            seq_len((nrow(df) + nrow(vals)) >= 1L),
            names(df),
            drop = FALSE
          ]
        else
          unique(rbind(
            df,
            vals[, names(df), drop = FALSE]
          ))
      }
      df
    }
  )
  keydups <- mapply(
    \(df, ks) {
      any(vapply(
        ks,
        \(key) {
          if (length(key) == 0L) {
            if (nrow(df) == 0L)
              logical()
            else
              c(FALSE, rep(TRUE, nrow(df) - 1L))
          }else{
            duplicated(df[, key, drop = FALSE])
          }
        },
        logical(nrow(df))
      ))
    },
    new_records[relations],
    keys(x)[relations]
  )
  if (any(keydups)) {
    bad_relation_names <- names(which(keydups))
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
  records(x) <- new_records
  x
}

#' @export
`[.relation` <- function(x, i) {
  attrs <- attributes(x)
  indices <- stats::setNames(seq_along(x), names(x))
  taken <- indices[i]
  res <- unclass(x)[taken]
  attrs$names <- make.unique(unname(stats::setNames(nm = attrs$names)[taken]))
  attributes(res) <- attrs
  res

}

#' @export
`[[.relation` <- function(x, i) {
  indices <- stats::setNames(seq_along(x), names(x))
  taken <- try(indices[[i]], silent = TRUE)
  if (class(taken)[[1]] == "try-error")
    stop(attr(taken, "condition")$message)
  x[taken]
}

#' @export
`[[<-.relation` <- function(x, i, value) {
  indices <- stats::setNames(seq_along(x), names(x))
  taken <- try(indices[[i]], silent = TRUE)
  if (class(taken)[[1]] == "try-error")
    stop(attr(taken, "condition")$message)
  x[taken] <- value
  x
}

#' @export
`$.relation` <- function(x, name) {
  x[[name]]
}

#' @export
`$<-.relation` <- function(x, name, value) {
  pos <- match(name, names(x))
  if (is.na(pos))
    c(x, stats::setNames(value, name))
  else
    c(
      head(x, pos - 1),
      stats::setNames(value, name),
      tail(x, -pos)
    )
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

  dfs <- records(x)
  as <- attrs(x)
  ks <- keys(x)
  for (n in seq_len(min(n_relations, max))) {
    cat(paste0(
      "relation ",
      names(x)[[n]],
      ": ",
      toString(as[[n]]),
      "; ",
      with_number(nrow(dfs[[n]]), "record", "", "s"),
      "\n"
    ))
    keyset <- ks[[n]]
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
