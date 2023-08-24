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
