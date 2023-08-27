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
  x[] <- lapply(
    x,
    \(rel) {
      rel$df <- rbind(
        rel$df,
        vals[, names(rel$df), drop = FALSE]
      )
      rel
    }
  )
  x
}
