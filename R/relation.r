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
#' @seealso \code{\link{records}}, \code{\link{attrs}}, \code{\link{keys}}, and
#'   \code{\link{attrs_order}} for extracting parts of the information in a
#'   \code{relation_schema}; \code{\link{gv}} for converting the schema into
#'   Graphviz code; \code{\link{rename_attrs}} for renaming the attributes in
#'   \code{attrs_order}.
#' @return A \code{relation} object, containing the list given in
#'   \code{relations}, with \code{attrs_order} stored in an attribute of the
#'   same name. Relation schemas are returned with their keys' attributes sorted
#'   according to the attribute order in \code{attrs_order}, and the keys then
#'   sorted by priority order. Attributes in the data frame are also sorted,
#'   first by order of appearance in the sorted keys, then by order in
#'   \code{attrs_order} for non-prime attributes.
#' @export
#' @examples
#' rels <- relation(
#'   list(
#'     a = list(
#'       df = data.frame(a = logical(), b = logical()),
#'       keys = list("a")
#'     ),
#'     b = list(
#'       df = data.frame(b = logical(), c = logical()),
#'       keys = list("b", "c")
#'     )
#'   ),
#'   attrs_order = c("a", "b", "c", "d")
#' )
#' print(rels)
#' records(rels)
#' attrs(rels)
#' stopifnot(identical(
#'   attrs(rels),
#'   lapply(records(rels), names)
#' ))
#' keys(rels)
#' attrs_order(rels)
#' names(rels)
#'
#' # inserting data
#' insert(rels, data.frame(a = 1L, b = 2L, c = 3L, d = 4L))
#' # data is only inserted into relations where all columns are given...
#' insert(rels, data.frame(a = 1L, b = 2L, c = 3L))
#' # and that are listed in relations argument
#' insert(
#'   rels,
#'   data.frame(a = 1L, b = 2L, c = 3L, d = 4L),
#'   relations = "a"
#' )
#'
#' # vector operations
#' rels2 <- relation(
#'   list(
#'     e = list(
#'       df = data.frame(a = logical(), e = logical()),
#'       keys = list("e")
#'     )
#'   ),
#'   attrs_order = c("a", "e")
#' )
#' c(rels, rels2) # attrs_order attributes are merged
#' unique(c(rels, rels))
#'
#' # subsetting
#' rels[1]
#' rels[c(1, 2, 1)]
#' stopifnot(identical(rels[[1]], rels[1]))
#'
#' # reassignment
#' rels3 <- rels
#' rels3[2] <- relation(
#'   list(
#'     d = list(
#'       df = data.frame(d = logical(), c = logical()),
#'       keys = list("d")
#'     )
#'   ),
#'   attrs_order(rels3)
#' )
#' print(rels3) # note the relation's name doesn't change
#' # names(rels3)[2] <- "d" # this would change the name
#' keys(rels3)[[2]] <- list(character()) # removing keys first...
#' # for a relation_schema, we could then change the attrs for
#' # the second relation. For a created relation, this is not
#' # allowed.
#' \dontrun{
#'   attrs(rels3)[[2]] <- c("b", "c")
#'   names(records(rels3)[[2]]) <- c("b", "c")
#' }
#'
#' # changing appearance priority for attributes
#' rels4 <- rels
#' attrs_order(rels4) <- c("d", "c", "b", "a")
#' print(rels4)
#'
#' # reconstructing from components
#' rels_recon <- relation(
#'   Map(list, df = records(rels), keys = keys(rels)),
#'   attrs_order(rels)
#' )
#' stopifnot(identical(rels_recon, rels))
#'
#' # can be a data frame column
#' data.frame(id = 1:2, relation = rels)
relation <- function(relations, attrs_order) {
  stopifnot(is.list(relations))
  stopifnot(is.character(attrs_order))

  stop_with_values_if(
    attrs_order,
    duplicated(attrs_order),
    "attrs_order must be unique",
    "duplicated",
    suffix_else = "",
    unique = TRUE
  )
  check_relation_names(names(relations))
  stop_with_elements_if(
    !vapply(relations, \(r) !anyDuplicated(names(r$df)), logical(1)),
    "relation attributes must be unique"
  )
  duplicate_relation_key_attributes <- lapply(
    seq_along(relations),
    \(n) {
      r <- relations[[n]]
      lapply(
        seq_along(r$keys),
        \(m) {
          k <- r$keys[[m]]
          dups <- sort(unique(k[duplicated(k)]))
          if (length(dups) == 0)
            NULL
          else
            paste0(n, ".", m, ".{", toString(dups), "}")
        }
      ) |>
        Reduce(f = c, init = character())
    }
  ) |>
    Reduce(f = c, init = character())
  stop_with_values_if(
    duplicate_relation_key_attributes,
    rep(TRUE, length(duplicate_relation_key_attributes)),
    "relation key attributes must be unique",
    prefix = "element"
  )
  stop_with_elements_if(
    lengths(relations) != 2,
    "relation elements must have length two"
  )
  correct_element_names <- vapply(
    relations,
    \(rel) setequal(names(rel), c("df", "keys")),
    logical(1L)
  )
  stop_with_elements_if(
    !correct_element_names,
    "relations must contain 'df' and 'keys' elements"
  )
  stop_with_elements_if(
    !vapply(relations, \(rel) is.data.frame(rel$df), logical(1)),
    "relation 'df' elements must be data frames"
  )
  stop_with_elements_if(
    !vapply(relations, \(rel) inherits(rel$keys, "list"), logical(1)),
    "relation 'keys' elements must be lists"
  )

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
  attrs_missing <- Reduce(
    c,
    lapply(relations, \(rel) names(rel$df)),
    init = character()
  ) |>
    setdiff(attrs_order)
  stop_with_values_if(
    attrs_missing,
    rep(TRUE, length(attrs_missing)),
    "relation attributes not in attrs_order",
    prefix = "missing",
    suffix_else = ""
  )
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

  missing_key_attributes <- lapply(
    seq_along(relations),
    \(n) {
      r <- relations[[n]]
      lapply(
        seq_along(r$keys),
        \(m) {
          k <- r$keys[[m]]
          missed <- setdiff(k, names(r$df))
          if (length(missed) == 0)
            NULL
          else
            paste0(
              n,
              ".",
              m,
              ".{",
              toString(missed),
              "}"
            )
        }
      ) |>
        Reduce(f = c, init = character())
    }
  ) |>
    Reduce(f = c, init = character())
  stop_with_values_if(
    missing_key_attributes,
    rep(TRUE, length(missing_key_attributes)),
    "relation keys must be within relation attributes",
    prefix = "element"
  )
  unsatisfied_keys <- lapply(
    seq_along(relations),
    \(n) {
      r <- relations[[n]]
      failed <- which(vapply(
        r$keys,
        \(key) df_anyDuplicated(r$df[, key, drop = FALSE]) > 0,
        logical(1)
      ))
      paste0(
        n,
        ".{",
        vapply(r$keys[failed], toString, character(1)),
        "}",
        recycle0 = TRUE
      )
    }
  ) |>
    Reduce(f = c, init = character())
  stop_with_values_if(
    unsatisfied_keys,
    rep(TRUE, length(unsatisfied_keys)),
    "relations must satisfy their keys",
    prefix = "element"
  )

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
  stop_with_values_if(
    nms,
    duplicated(nms),
    "relation names must be unique",
    prefix = "duplicated",
    suffix_else = "",
    unique = TRUE
  )
  stop_with_elements_if(
    nms == "",
    "relation names must be non-empty"
  )
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
  relation(new, attrs_order(x))
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
insert.relation <- function(x, vals, relations = names(x), all = FALSE, ...) {
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
  insertable <- vapply(
    attrs(x)[relations],
    \(as) all(is.element(as, names(vals))),
    logical(1)
  )
  if (all && any(!insertable)) {
    missed <- intersect(
      attrs_order(x),
      Reduce(
        c,
        lapply(attrs(x)[relations[!insertable]], setdiff, names(vals)),
        init = character()
      )
    )
    stop(paste("vals missing required attributes:", toString(missed)))
  }
  new_records[relations[insertable]] <- lapply(
    new_records[relations[insertable]],
    \(df) {
      if (!all(is.element(names(df), names(vals))))
        return(df)
      df <- if (nrow(df) == 0L)
        df_unique(vals[, names(df), drop = FALSE])
      else
        df_unique(df_rbind(
          df,
          vals[, names(df), drop = FALSE]
        ))
      df
    }
  )
  keydups <- mapply(
    \(df, ks) {
      any(vapply(
        ks,
        \(key) df_duplicated(df[, key, drop = FALSE]),
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
  if (anyNA(taken))
    stop("subset names that don't exist: ", toString(i[is.na(taken)]))
  res <- unclass(x)[taken]
  attrs$names <- make.unique(unname(stats::setNames(nm = attrs$names)[taken]))
  attributes(res) <- attrs
  res

}

#' @export
`[<-.relation` <- function(x, i, value) {
  check_reassignment_same_class(value, x)
  rcs <- records(x)
  ks <- keys(x)
  rcs[i] <- records(value)
  ks[i] <- keys(value)
  relation(
    stats::setNames(
      Map(
        \(recs, ks) list(df = recs, keys = ks),
        rcs,
        ks
      ),
      names(x)
    ),
    attrs_order = merge_attribute_orderings(attrs_order(x), attrs_order(value))
  )
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
  check_reassignment_same_class(value, x)
  pos <- match(name, names(x))
  if (is.na(pos))
    c(x, stats::setNames(value, name))
  else
    append(x[-pos], stats::setNames(value, name), pos)
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

#' @exportS3Method
format.relation <- function(x, ...) {
  paste0(
    "schema ",
    names(x),
    " (",
    vapply(
      records(x),
      \(df) with_number(nrow(df), "record", "", "s"),
      character(1)
    ),
    ")"
  )
}

#' @exportS3Method
as.data.frame.relation <- function(
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
