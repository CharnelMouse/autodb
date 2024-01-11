# (foreign key) references are a commonly recurring component: while not
# currently a class in their own right, they have common manipulation patterns,
# that are collected here.

self_reference <- function(references) {
  vapply(references, \(r) r[[1]] == r[[3]], logical(1))
}

reference_names_element <- function(references, relation_names) {
  vapply(references, \(r) all(c(r[[1]], r[[3]]) %in% relation_names), logical(1))
}

reference_valid_attrs <- function(references, relation_schemas) {
  vapply(
    references,
    \(r) {
      all(r[[2]] %in% attrs(relation_schemas)[[r[[1]]]]) &&
        all(r[[4]] %in% unlist(keys(relation_schemas)[[r[[3]]]]))
    },
    logical(1)
  )
}

rename_reference_referands <- function(references, old_names, new_names) {
  stopifnot(length(new_names) == length(old_names))
  lapply(
    references,
    \(rl) list(
      new_names[match(rl[[1]], old_names)],
      rl[[2]],
      new_names[match(rl[[3]], old_names)],
      rl[[4]]
    )
  )
}

merge_reference_referands <- function(
  references,
  to_remove,
  merge_into,
  old_names,
  new_names
) {
  ind_map <- seq_along(old_names)
  remaining_inds <- setdiff(ind_map, to_remove)
  ind_map[to_remove] <- merge_into
  ind_map <- seq_along(remaining_inds)[match(ind_map, remaining_inds)]

  new_rels <- unique(rename_reference_referands(
    references,
    old_names,
    new_names[ind_map]
  ))
  new_rels[!self_reference(new_rels)]
}

print_references <- function(x, max) {
  if (length(x) == 0) {
    cat("no relationships\n")
  }else{
    cat(paste("relationships:\n"))
    n_relationships <- length(x)
    for (r in seq_len(n_relationships)) {
      rel <- x[[r]]
      cat(paste0(
        rel[[1]], ".{", toString(rel[[2]]),
        "} -> ",
        rel[[3]], ".{", toString(rel[[4]]), "}\n"
      ))
    }
    if (max < n_relationships)
      cat("... and", n_relationships - max, "other relationships\n")
  }
}
