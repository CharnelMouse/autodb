#' Databases
#'
#' Enhances a \code{\link{relation}} object with foreign key reference
#' information.
#'
#' Unlike \code{\link{relation_schema}} and \code{link{relation}}, and like
#' \code{\link{database_schema}}, \code{database} is not designed to be
#' vector-like: it only holds a single database. This adheres to the usual
#' package use case, where a single data frame is being analysed at a time.
#' However, it inherits from \code{\link{relation}}, so is vectorised with
#' respect to its relations.
#'
#' As with \code{\link{relation}}, duplicate relations, after ordering by
#' attribute, are allowed, and can be removed with \code{\link{unique}}.
#'
#' References, i.e. foreign key references, are allowed to have different
#' attribute names in the child and parent relations; this can't occur in the
#' output for \code{\link{autoref}} and \code{\link{normalise}}.
#'
#' Subsetting removes any references that involve removed relations.
#' Removing duplicates with \code{\link{unique}} changes references involving
#' duplicates to involve the kept equivalent relations instead. Renaming
#' relations with \code{\link[base:names]{`names<-`}} also changes their names
#' in the references.
#'
#' @inheritParams database_schema
#' @inheritParams autodb
#' @param relations a \code{\link{relation}} object.
#'
#' @return A \code{database} object, containing \code{relations} with
#'   \code{references} stored in an attribute of the same name. References
#'   are stored with their attributes in the order they appear in their
#'   respective relations.
#' @export
database <- function(relations, references, name = NA_character_) {
  if (!inherits(relations, "relation"))
    stop("relations must be a relation")
  if (!is.character(name) || length(name) != 1L)
    stop("name must be a scalar character")
  check_valid_reference(references, relations, "relation")

  relat_errors <- reference_errors(records(relations), references)
  if (length(relat_errors) > 0)
    stop(paste0(
      "relations must satisfy references in schema:\n",
      paste(
        vapply(
          relat_errors,
          \(relat) paste0(
            relat[[1]], ".{", toString(relat[[2]]),
            "} -> ",
            relat[[3]], ".{", toString(relat[[4]]), "}"
          ),
          character(1)
        ),
        collapse = "\n"
      )
    ))

  database_nocheck(relations, references, name)
}

database_nocheck <- function(relations, references, name = NA_character_) {
  structure(
    relations,
    name = name,
    references = references,
    class = c("database", "relation", "list")
  )
}

reference_errors <- function(records, references) {
  references[vapply(
    references,
    \(relat) {
      referrer <- unique(records[[relat[[1]]]][, relat[[2]], drop = FALSE])
      referee <- unique(records[[relat[[3]]]][, relat[[4]], drop = FALSE])
      !identical(
        nrow(merge(referrer, referee, by.x = relat[[2]], by.y = relat[[4]])),
        nrow(referrer)
      )
    },
    logical(1)
  )]
}

#' @export
`attrs<-.database` <- function(x, ..., value) {
  rels <- subrelations(x)
  attrs(rels) <- value
  if (any(!reference_valid_attrs(references(x), rels)))
    stop("attrs reassignments must keep attributes used in references")
  database(rels, references(x))
}

#' @export
`attrs_order<-.database` <- function(x, ..., value) {
  rels <- subrelations(x)
  attrs_order(rels) <- value
  database(
    rels,
    references = references(x),
    name = name(x)
  )
}

#' @export
rename_attrs.database <- function(x, names, ...) {
  new_subrels <- rename_attrs(subrelations(x), names)
  new_refs <- lapply(
    references(x),
    \(ref) {
      ref[c(2, 4)] <- lapply(
        ref[c(2, 4)],
        \(as) names[match(as, attrs_order(x))]
      )
      ref
    }
  )
  database(
    new_subrels,
    new_refs
  )
}

#' @export
`keys<-.database` <- function(x, ..., value) {
  rs <- subrelations(x)
  keys(rs, ...) <- value
  database(rs, references(x))
}

#' @export
`records<-.database` <- function(x, ..., value) {
  relations <- subrelations(x)
  records(relations) <- value
  database(relations, references(x))
}

#' @export
`names<-.database` <- function(x, value) {
  check_relation_names(value)
  new_refs <- lapply(
    references(x),
    \(ref) {
      ref[[1]] <- value[[match(ref[[1]], names(x))]]
      ref[[3]] <- value[[match(ref[[3]], names(x))]]
      ref
    }
  )
  attr(x, "names") <- value
  database_nocheck(x, new_refs, name(x))
}

#' @exportS3Method
name.database <- function(x, ...) {
  attr(x, "name")
}

#' @exportS3Method
references.database <- function(x, ...) {
  attr(x, "references")
}

#' @export
`references<-.database` <- function(x, value) {
  database(subrelations(x), value)
}

#' @exportS3Method
subrelations.database <- function(x, ...) {
  y <- unclass(x)
  attributes(y) <- NULL
  relation_nocheck(stats::setNames(y, names(x)), attrs_order(x))
}

#' @exportS3Method
unique.database <- function(x, ...) {
  relations <- subrelations(x)
  dups <- which(duplicated(relations))
  if (length(dups) == 0L)
    return(x)
  y <- sort_records(relations)
  dups_prec <- vapply(
    dups,
    \(n) Position(
      \(rs) {
        identical(unname(records(rs)), unname(records(y[[n]]))) &&
          identical(unname(keys(rs)), unname(keys(y[[n]])))
      },
      y
    ),
    integer(1)
  )
  merge_database_relations(x, dups, dups_prec)
}

merge_database_relations <- function(x, to_remove, merge_into, ...) {
  stopifnot(length(to_remove) == length(merge_into))
  relations <- merge_relations(subrelations(x), to_remove, merge_into)
  rels <- merge_reference_referands(
    references(x),
    to_remove,
    merge_into,
    names(x),
    names(relations)
  )
  database(relations, rels)
}

merge_relations <- function(x, to_remove, merge_into, ...) {
  stopifnot(length(to_remove) == length(merge_into))

  for (n in seq_along(to_remove)[to_remove != merge_into]) {
    stopifnot(identical(
      unname(keys(x[[to_remove[[n]]]])),
      unname(keys(x[[merge_into[[n]]]]))
    ))
    stopifnot(identical(
      unname(attrs(x[[to_remove[[n]]]])),
      unname(attrs(x[[merge_into[[n]]]]))
    ))
  }

  x[-to_remove]
}

#' @exportS3Method
c.database <- function(...) {
  lst <- list(...)
  joined_rels <- do.call(c, lapply(lst, subrelations))
  names(joined_rels) <- if (is.null(names(joined_rels)))
    character(length(joined_rels))
  else
    make.unique(names(joined_rels))

  references_list <- lapply(lst, references)
  new_references <- Map(
    rename_reference_referands,
    references_list,
    lapply(lst, names),
    unname(split(
      names(joined_rels),
      rep(factor(seq_along(lst)), lengths(lst))
    ))
  )
  joined_references <- do.call(c, new_references)

  result_lst <- list(joined_rels, joined_references)
  do.call(database, result_lst)
}

#' @exportS3Method
insert.database <- function(x, vals, relations = names(x), ...) {
  new_subrelations <- insert(subrelations(x), vals, relations, ...)
  dfs <- records(new_subrelations)
  reference_checks <- reference_errors(dfs, references(x))
  if (length(reference_checks)) {
    error_strings <- vapply(
      reference_checks,
      \(relat) paste0(
        relat[[1]], ".{", toString(relat[[2]]),
        "} -> ",
        relat[[3]], ".{", toString(relat[[4]]), "}"
      ),
      character(1)
    )
    stop(
      "insertion violates ",
      with_number(length(error_strings), "reference", "", "s"),
      ":\n",
      paste(error_strings, collapse = "\n")
    )
  }
  database(new_subrelations, references(x))
}

#' @export
`[.database` <- function(x, i) {
  new_relations <- subrelations(x)[i]
  kept_rels <- subset_refs(
    references(x),
    stats::setNames(seq_along(x), names(x))[i],
    names(x),
    names(new_relations)
  )
  database(new_relations, kept_rels)
}

#' @export
`[<-.database` <- function(x, i, value) {
  check_reassignment_same_class(value, x)
  full_ind <- stats::setNames(seq_along(x), names(x))[i]
  uniq <- !duplicated(full_ind, fromLast = TRUE)

  uniq_ind <- full_ind[uniq]
  uniq_value <- stats::setNames(value[uniq], names(x)[uniq_ind])
  rel <- subrelations(x)
  rel[uniq_ind] <- subrelations(uniq_value)
  refs <- references(x[setdiff(seq_along(x), uniq_ind)])
  refs <- c(refs, references(uniq_value))
  database(rel, refs)
}

#' @export
`$<-.database` <- function(x, name, value) {
  check_reassignment_same_class(value, x)
  pos <- match(name, names(x))
  if (is.na(pos))
    c(x, stats::setNames(value, name))
  else {
    renamed_value <- stats::setNames(value, name)
    subs <- subrelations(x)
    database(
      append(subs[-pos], subrelations(renamed_value), pos),
      c(
        references(x[-pos]),
        references(renamed_value)
      )
    )
  }
}

#' @exportS3Method
print.database <- function(x, max = 10, ...) {
  cat(paste0("database ", name(x), " with "))
  print(subrelations(x), max = max, ...)
  print_references(references(x), max)
}
