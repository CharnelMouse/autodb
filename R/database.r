#' Databases
#'
#' Enhances a \code{\link{relation}} object with foreign key reference
#' information.
#'
#' Unlike \code{\link{relation_schema}} and \code{\link{relation}}, and like
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
#' db <- database(
#'   rels,
#'   list(list("a", "b", "b", "b"))
#' )
#' print(db)
#' attrs(db)
#' stopifnot(identical(
#'   attrs(db),
#'   lapply(records(db), names)
#' ))
#' keys(db)
#' attrs_order(db)
#' names(db)
#' references(db)
#'
#' # relations can't reference themselves
#' \dontrun{
#'   database(
#'     relation(
#'       list(a = list(df = data.frame(a = 1:5), keys = list("a"))),
#'       c("a", "b")
#'     ),
#'     list(list("a", "a", "a", "a"))
#'   )
#'   database(
#'     relation(
#'       list(a = list(df = data.frame(a = 1:5, b = 6:10), keys = list("a"))),
#'       c("a", "b")
#'     ),
#'     list(list("a", "b", "a", "a"))
#'   )
#' }
#'
#' # an example with references between differently-named attributes
#' print(database(
#'   relation(
#'     list(
#'       citation = list(df = data.frame(citer = 1:5, citee = 6:10), keys = list(c("citer", "citee"))),
#'       article = list(df = data.frame(article = 1:10), keys = list("article"))
#'     ),
#'     c("citer", "citee", "article")
#'   ),
#'   list(
#'     list("citation", "citer", "article", "article"),
#'     list("citation", "citee", "article", "article")
#'   )
#' ))
#'
#' # inserting data
#' insert(db, data.frame(a = 1L, b = 2L, c = 3L, d = 4L))
#' # data is only inserted into relations where all columns are given...
#' insert(db, data.frame(a = 1L, b = 2L, c = 3L))
#' # and that are listed in relations argument
#' insert(
#'   db,
#'   data.frame(a = 1L, b = 2L, c = 3L, d = 4L),
#'   relations = "b"
#' )
#' # inserted data can't violate keys
#' \dontrun{
#'   insert(
#'     db,
#'     data.frame(a = 1L, b = 1:2)
#'   )
#' }
#' # inserted data can't violate foreign key references
#' \dontrun{
#'   insert(
#'     db,
#'     data.frame(a = 1L, b = 2L, c = 3L, d = 4L),
#'     relations = "a"
#'   )
#' }
#'
#' # vector operations
#' db2 <- database(
#'   relation(
#'     list(
#'       e = list(df = data.frame(a = 1:5, e = 6:10), keys = list("e"))
#'     ),
#'     attrs_order = c("a", "e")
#'   ),
#'   list()
#' )
#' c(db, db2) # attrs_order attributes are merged
#' unique(c(db, db))
#'
#' # subsetting
#' db[1]
#' stopifnot(identical(db[[1]], db[1]))
#' db[c(1, 2, 1, 2)] # replicates the foreign key references
#' c(db[c(1, 2)], db[c(1, 2)]) # doesn't reference between separate copies of db
#' unique(db[c(1, 2, 1, 2)]) # unique() also merges references
#'
#' # another example of unique() merging references
#' db_merge <- database(
#'   relation(
#'     list(
#'       a = list(
#'         df = data.frame(a = logical(), b = logical()),
#'         keys = list("a")
#'       ),
#'       b = list(
#'         df = data.frame(b = logical(), c = logical(), d = logical()),
#'         keys = list("b")
#'       ),
#'       c_d = list(
#'         df = data.frame(c = logical(), d = logical(), e = logical()),
#'         keys = list(c("c", "d"))
#'       ),
#'       a.1 = list(
#'         df = data.frame(a = logical(), b = logical()),
#'         keys = list("a")
#'       ),
#'       b.1 = list(
#'         df = data.frame(b = logical(), c = logical(), d = logical()),
#'         keys = list("b")
#'       )
#'     ),
#'     c("a", "b", "c", "d", "e")
#'   ),
#'   list(
#'     list("a", "b", "b", "b"),
#'     list("b.1", c("c", "d"), "c_d", c("c", "d"))
#'   )
#' )
#' print(db_merge)
#' unique(db_merge)
#'
#' # reassignment
#' # can't change keys included in references
#' \dontrun{keys(db)[[2]] <- list("c")}
#' # can't remove attributes included in keys
#' \dontrun{attrs(db)[[2]] <- list("c", "d")}
#' # can't remove attributes included in references
#' \dontrun{attrs(db)[[1]] <- c("a", "d")}
#' db3 <- db
#' # can change subset of schema, but loses references between altered and
#' # non-altered subsets
#' db3[2] <- database(
#'   relation(
#'     list(d = list(df = data.frame(d = logical(), c = logical()), keys = list("d"))),
#'     attrs_order(db3)
#'   ),
#'   list()
#' )
#' print(db3) # note the schema's name doesn't change
#' # names(db3)[2] <- "d" # this would change the name
#' keys(db3)[[2]] <- list(character()) # removing keys first...
#' # for a database_schema, we could then change the attrs for
#' # the second database. For a created relation, this is not
#' # allowed.
#' \dontrun{
#'   attrs(db3)[[2]] <- c("b", "c")
#'   names(records(db3)[[2]]) <- c("b", "c")
#' }
#'
#' # changing appearance priority for attributes
#' attrs_order(db3) <- c("d", "c", "b", "a")
#' print(db3)
#'
#' # changing relation schema names changes them in references
#' names(db3) <- paste0(names(db3), "_long")
#' print(db3)
#'
#' # reconstructing from components
#' db_recon <- database(
#'   relation(
#'     Map(list, df = records(db), keys = keys(db)),
#'     attrs_order(db)
#'   ),
#'   references(db)
#' )
#' stopifnot(identical(db_recon, db))
#' db_recon2 <- database(
#'   subrelations(db),
#'   references(db)
#' )
#' stopifnot(identical(db_recon2, db))
#'
#' # can be a data frame column
#' data.frame(id = 1:2, relation = db)
database <- function(relations, references) {
  if (!inherits(relations, "relation"))
    stop("relations must be a relation")
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

  database_nocheck(relations, references)
}

database_nocheck <- function(relations, references) {
  structure(
    relations,
    references = references,
    class = c("database", "relation", "list")
  )
}

reference_errors <- function(records, references) {
  references[vapply(
    references,
    \(relat) {
      referrer <- df_unique(records[[relat[[1]]]][, relat[[2]], drop = FALSE])
      referee <- df_unique(records[[relat[[3]]]][, relat[[4]], drop = FALSE])
      !identical(
        # unique() needed here in case floating-point values cause duplicates in
        # merge
        nrow(unique(df_join(referrer, referee, by.x = relat[[2]], by.y = relat[[4]]))),
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
    references = references(x)
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
  database_nocheck(x, new_refs)
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
  cat("database with ")
  print(subrelations(x), max = max, ...)
  print_references(references(x), max)
}
