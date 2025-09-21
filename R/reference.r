# (foreign key) references are a commonly recurring component: while not
# currently a class in their own right, they have common manipulation patterns,
# that are collected here.

check_valid_reference <- function(
  references,
  relation_schemas,
  type = c("relation schema", "relation")
) {
  type <- match.arg(type)
  if (!is.list(references))
    stop("references must be a list")
  stop_with_elements_if(
    lengths(references) != 4L |
      !vapply(references, is.list, logical(1)),
    "reference elements must be length-four lists"
  )
  ref_names_not_in_names <- setdiff(
    unlist(lapply(references, \(x) x[c(1, 3)])),
    names(relation_schemas)
  )
  stop_with_values_if(
    ref_names_not_in_names,
    rep(TRUE, length(ref_names_not_in_names)),
    paste(
      "reference relation names must be within",
      type,
      "names"
    ),
    prefix = "absent",
    suffix_else = ""
  )
  # sort so that keys are matched properly, and refs are assigned properly
  references <- lapply(
    references,
    \(ref) {
      ord <- order(match(ref[[4]], attrs_order(relation_schemas)))
      ref[[2]] <- ref[[2]][ord]
      ref[[4]] <- ref[[4]][ord]
      ref
    }
  )
  stop_with_elements_if(
    !reference_valid_attrs(references, relation_schemas),
    "reference attributes must be within referrer's attributes and referee's keys",
    prefix = "reference"
  )
  if (any(self_reference(references)))
    stop("reference cannot be from a relation's attribute to itself")
  references
}

self_reference <- function(references) {
  vapply(references, \(r) r[[1]] == r[[3]], logical(1))
}

reference_valid_attrs <- function(references, relation_schemas) {
  vapply(
    references,
    \(r) {
      all(r[[2]] %in% attrs(relation_schemas)[[r[[1]]]]) &&
        list(r[[4]]) %in% keys(relation_schemas)[[r[[3]]]]
    },
    logical(1)
  )
}

rename_reference_attrs <- function(x, old_names, names) {
  lapply(
    x,
    \(ref) {
      ref[c(2, 4)] <- lapply(
        ref[c(2, 4)],
        \(as) names[match(as, old_names)]
      )
      ref
    }
  )
}

rename_reference_relations <- function(references, old_names, new_names) {
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

  new_rels <- unique(rename_reference_relations(
    references,
    old_names,
    new_names[ind_map]
  ))
  new_rels[
    !self_reference(new_rels) &
      !is.na(vapply(new_rels, `[[`, character(1), 1))
  ]
}

print_references <- function(x, max) {
  if (length(x) == 0) {
    cat("no references\n")
  }else{
    cat(paste("references:\n"))
    n_references <- length(x)
    for (r in seq_len(min(n_references, max))) {
      rel <- x[[r]]
      cat(paste0(
        rel[[1]], ".{", toString(rel[[2]]),
        "} -> ",
        rel[[3]], ".{", toString(rel[[4]]), "}\n"
      ))
    }
    if (max < n_references)
      cat("... and", n_references - max, "other references\n")
  }
}

subset_refs <- function(refs, number_indices, names, new_names) {
  res <- unlist(
    lapply(
      refs,
      \(ref) {
        child_inds <- which(match(ref[[1]], names) == number_indices)
        parent_inds <- which(match(ref[[3]], names) == number_indices)
        if (length(child_inds) == 0 || length(parent_inds) == 0)
          return(list())
        child <- new_names[child_inds]
        parent <- new_names[parent_inds]
        unlist(
          lapply(
            child,
            \(c) lapply(parent, \(p) list(c, ref[[2]], p, ref[[4]]))
          ),
          recursive = FALSE
        )
      }
    ),
    recursive = FALSE
  )
  if (is.null(res)) list() else res
}
