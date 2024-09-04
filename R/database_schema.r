#' Database schemas
#'
#' Enhances a \code{\link{relation_schema}} object with foreign key reference
#' information.
#'
#' Unlike \code{\link{functional_dependency}} and \code{\link{relation_schema}},
#' \code{database_schema} is not designed to be vector-like: it only holds a
#' single database schema. This adheres to the usual package use case, where a
#' single data frame is being analysed at a time. However, it inherits from
#' \code{\link{relation_schema}}, so is vectorised with respect to its relation
#' schemas.
#'
#' As with \code{\link{relation_schema}}, duplicate relation schemas, after
#' ordering by attribute, are allowed, and can be removed with
#' \code{\link{unique}}.
#'
#' References, i.e. foreign key references, are allowed to have different
#' attribute names in the child and parent relations; this can't occur in the
#' output for \code{\link{autoref}} and \code{\link{normalise}}.
#'
#' Subsetting removes any references that involve removed relation schemas.
#' Removing duplicates with \code{\link{unique}} changes references involving
#' duplicates to involve the kept equivalent schemas instead. Renaming relation
#' schemas with \code{\link[base:names]{`names<-`}} also changes their names in
#' the references.
#'
#' @param relation_schemas a \code{\link{relation_schema}} object, as returned
#'   by \code{\link{synthesise}} or \code{\link{relation_schema}}.
#' @param references a list of references, each
#'  represented by a list containing four character elements. In order, the
#'  elements are a scalar giving the name of the child schema, a vector giving
#'  the child attribute names, a scalar giving the name of the parent schema,
#'  and a vector giving the parent attribute names. The vectors must be of the
#'  same length and contain names for attributes present in their respective
#'  schemas, and the parent attributes must form a key, in order.
#'
#' @return A \code{database_schema} object, containing \code{relation_schemas}
#'   with \code{references} stored in an attribute of the same name.
#'   References are stored with their attributes in the order they appear in
#'   their respective relation schemas.
#' @seealso \code{\link{attrs}}, \code{\link{keys}}, \code{\link{attrs_order}},
#'   and \code{\link{references}} for extracting parts of the information in a
#'   \code{database_schema}; \code{\link{create}} for creating a
#'   \code{\link{database}} object that uses the given schema; \code{\link{gv}}
#'   for converting the schema into Graphviz code; \code{\link{rename_attrs}}
#'   for renaming the attributes in \code{attrs_order}; \code{\link{reduce}} for
#'   filtering a schema's relations to those connected to a given relation by
#'   foreign key references; \code{\link{subschemas}} to return the
#'   \code{\link{relation_schema}} that the given schema contains.
#' @export
#' @examples
#' rs <- relation_schema(
#'   list(
#'     a = list(c("a", "b"), list("a")),
#'     b = list(c("b", "c"), list("b", "c"))
#'   ),
#'   attrs_order = c("a", "b", "c", "d")
#' )
#' ds <- database_schema(
#'   rs,
#'   list(list("a", "b", "b", "b"))
#' )
#' print(ds)
#' attrs(ds)
#' keys(ds)
#' attrs_order(ds)
#' names(ds)
#' references(ds)
#'
#' # relations can't reference themselves
#' \dontrun{
#'   database_schema(
#'     relation_schema(
#'       list(a = list("a", list("a"))),
#'       c("a", "b")
#'     ),
#'     list(list("a", "a", "a", "a"))
#'   )
#'   database_schema(
#'     relation_schema(
#'       list(a = list(c("a", "b"), list("a"))),
#'       c("a", "b")
#'     ),
#'     list(list("a", "b", "a", "a"))
#'   )
#' }
#'
#' # an example with references between differently-named attributes
#' print(database_schema(
#'   relation_schema(
#'     list(
#'       citation = list(c("citer", "citee"), list(c("citer", "citee"))),
#'       article = list("article", list("article"))
#'     ),
#'     c("citer", "citee", "article")
#'   ),
#'   list(
#'     list("citation", "citer", "article", "article"),
#'     list("citation", "citee", "article", "article")
#'   )
#' ))
#'
#' # vector operations
#' ds2 <- database_schema(
#'   relation_schema(
#'     list(
#'       e = list(c("a", "e"), list("e"))
#'     ),
#'     attrs_order = c("a", "e")
#'   ),
#'   list()
#' )
#' c(ds, ds2) # attrs_order attributes are merged
#' unique(c(ds, ds))
#'
#' # subsetting
#' ds[1]
#' ds[[1]] # same result as ds[1]
#' ds[c(1, 2, 1, 2)] # replicates the foreign key references
#' c(ds[c(1, 2)], ds[c(1, 2)]) # doesn't reference between separate copies of ds
#' unique(ds[c(1, 2, 1, 2)]) # unique() also merges references
#'
#' # another example of unique() merging references
#' ds_merge <- database_schema(
#'   relation_schema(
#'     list(
#'       a = list(c("a", "b"), list("a")),
#'       b = list(c("b", "c", "d"), list("b")),
#'       c_d = list(c("c", "d", "e"), list(c("c", "d"))),
#'       a.1 = list(c("a", "b"), list("a")),
#'       b.1 = list(c("b", "c", "d"), list("b"))
#'     ),
#'     c("a", "b", "c", "d", "e")
#'   ),
#'   list(
#'     list("a", "b", "b", "b"),
#'     list("b.1", c("c", "d"), "c_d", c("c", "d"))
#'   )
#' )
#' print(ds_merge)
#' unique(ds_merge)
#'
#' # reassignment
#' # can't change keys included in references
#' \dontrun{keys(ds)[[2]] <- list("c")}
#' # can't remove attributes included in keys
#' \dontrun{attrs(ds)[[2]] <- list("c", "d")}
#' # can't remove attributes included in references
#' \dontrun{attrs(ds)[[1]] <- c("a", "d")}
#' ds3 <- ds
#' # can change subset of schema, but loses references between altered and
#' # non-altered subsets
#' ds3[2] <- database_schema(
#'   relation_schema(
#'     list(d = list(c("d", "c"), list("d"))),
#'     attrs_order(ds3)
#'   ),
#'   list()
#' )
#' print(ds3) # note the schema's name doesn't change
#' # names(ds3)[2] <- "d" # this would change the name
#' keys(ds3)[[2]] <- list(character()) # removing keys first...
#' attrs(ds3)[[2]] <- c("b", "c") # so we can change the attrs legally
#' keys(ds3)[[2]] <- list("b", "c") # add the new keys
#' # add the reference lost during subset replacement
#' references(ds3) <- c(references(ds3), list(list("a", "b", "b", "b")))
#' stopifnot(identical(ds3, ds))
#'
#' # changing appearance priority for attributes
#' attrs_order(ds3) <- c("d", "c", "b", "a")
#' print(ds3)
#'
#' # changing relation schema names changes them in references
#' names(ds3) <- paste0(names(ds3), "_long")
#' print(ds3)
#'
#' # reconstructing from components
#' ds_recon <- database_schema(
#'   relation_schema(
#'     Map(list, attrs(ds), keys(ds)),
#'     attrs_order(ds)
#'   ),
#'   references(ds)
#' )
#' stopifnot(identical(ds_recon, ds))
#' ds_recon2 <- database_schema(
#'   subschemas(ds),
#'   references(ds)
#' )
#' stopifnot(identical(ds_recon2, ds))
database_schema <- function(relation_schemas, references) {
  if (!inherits(relation_schemas, "relation_schema"))
    stop("relations must be a relation_schema")
  check_valid_reference(references, relation_schemas, "relation schema")

  structure(
    relation_schemas,
    references = references,
    class = c("database_schema", "relation_schema")
  )
}

#' @exportS3Method
references.database_schema <- function(x, ...) {
  attr(x, "references")
}

#' @export
`references<-.database_schema` <- function(x, value) {
  database_schema(subschemas(x), value)
}

#' @export
`attrs<-.database_schema` <- function(x, ..., value) {
  if (any(mapply(\(ks, val) any(!is.element(unlist(ks), val)), keys(x), value)))
    stop("attrs reassignments must keep attributes used in keys")

  referrers <- split(
    lapply(references(x), `[[`, 2),
    vapply(references(x), `[[`, character(1), 1)
  )
  referees <- split(
    lapply(references(x), `[[`, 4),
    vapply(references(x), `[[`, character(1), 3)
  )
  refs <- lapply(
    names(x),
    \(nm) unlist(c(referrers[nm], referees[nm]))
  )
  if (any(mapply(\(ref_attrs, val) any(!is.element(ref_attrs, val)), refs, value)))
    stop("attrs reassignments must keep attributes used in references")

  rs <- subschemas(x)
  attrs(rs, ...) <- value
  database_schema(rs, references(x))
}

#' @export
`attrs_order<-.database_schema` <- function(x, ..., value) {
  rels <- subschemas(x)
  attrs_order(rels) <- value
  database_schema(rels, references(x))
}

#' @export
rename_attrs.database_schema <- function(x, names, ...) {
  new_subschemas <- rename_attrs(subschemas(x), names)
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
  database_schema(
    new_subschemas,
    new_refs
  )
}

#' @export
`keys<-.database_schema` <- function(x, ..., value) {
  rs <- subschemas(x)
  keys(rs, ...) <- value
  database_schema(rs, references(x))
}

#' @export
`names<-.database_schema` <- function(x, value) {
  check_schema_names(value)
  new_refs <- lapply(
    references(x),
    \(ref) {
      ref[[1]] <- value[[match(ref[[1]], names(x))]]
      ref[[3]] <- value[[match(ref[[3]], names(x))]]
      ref
    }
  )
  attr(x, "names") <- value
  database_schema(x, new_refs)
}

#' @exportS3Method
subschemas.database_schema <- function(x, ...) {
  y <- unclass(x)
  attributes(y) <- NULL
  relation_schema_nocheck(stats::setNames(y, names(x)), attrs_order(x))
}

#' @exportS3Method
create.database_schema <- function(x, ...) {
  database(
    create(subschemas(x)),
    references(x)
  )
}

#' @exportS3Method
unique.database_schema <- function(x, ...) {
  schemas <- subschemas(x)
  dups <- which(duplicated(schemas))
  if (length(dups) == 0L)
    return(x)
  dups_prec <- vapply(
    dups,
    \(n) Position(
      \(rs) {
        identical(unname(attrs(rs)), unname(attrs(schemas[[n]]))) &&
          identical(unname(keys(rs)), unname(keys(schemas[[n]])))
      },
      schemas
    ),
    integer(1)
  )
  merge_schemas(x, dups, dups_prec)
}

#' @exportS3Method
c.database_schema <- function(...) {
  lst <- list(...)
  joined_schemas <- do.call(c, lapply(lst, subschemas))
  names(joined_schemas) <- if (is.null(names(joined_schemas)))
    character(length(joined_schemas))
  else
    make.unique(names(joined_schemas))

  references_list <- lapply(lst, references)
  new_references <- Map(
    rename_reference_referands,
    references_list,
    lapply(lst, names),
    unname(split(
      names(joined_schemas),
      rep(factor(seq_along(lst)), lengths(lst))
    ))
  )
  joined_references <- do.call(c, new_references)

  result_lst <- list(joined_schemas, joined_references)
  do.call(database_schema, result_lst)
}

#' @exportS3Method
merge_schemas.database_schema <- function(x, to_remove, merge_into, ...) {
  stopifnot(length(to_remove) == length(merge_into))
  schemas <- merge_schemas.relation_schema(subschemas(x), to_remove, merge_into)
  rels <- merge_reference_referands(
    references(x),
    to_remove,
    merge_into,
    names(x),
    names(schemas)
  )
  database_schema(schemas, rels)
}

#' @export
`[.database_schema` <- function(x, i) {
  new_schemas <- subschemas(x)[i]
  kept_rels <- subset_refs(
    references(x),
    stats::setNames(seq_along(x), names(x))[i],
    names(x),
    names(new_schemas)
  )
  database_schema(new_schemas, kept_rels)
}

#' @export
`[<-.database_schema` <- function(x, i, value) {
  check_reassignment_same_class(value, x)
  full_ind <- stats::setNames(seq_along(x), names(x))[i]
  uniq <- !duplicated(full_ind, fromLast = TRUE)
  uniq_ind <- full_ind[uniq]
  uniq_value <- stats::setNames(value[uniq], names(x)[uniq_ind])
  rs <- subschemas(x)
  rs[uniq_ind] <- subschemas(uniq_value)
  refs <- references(x[setdiff(seq_along(x), uniq_ind)])
  refs <- c(refs, references(uniq_value))
  database_schema(rs, refs)
}

#' @export
`$<-.database_schema` <- function(x, name, value) {
  check_reassignment_same_class(value, x)
  pos <- match(name, names(x))
  if (is.na(pos))
    c(x, stats::setNames(value, name))
  else {
    renamed_value <- stats::setNames(value, name)
    subs <- subschemas(x)
    database_schema(
      append(subs[-pos], subschemas(renamed_value), pos),
      c(
        references(x[-pos]),
        references(renamed_value)
      )
    )
  }
}

#' @exportS3Method
print.database_schema <- function(x, max = 10, ...) {
  cat("database schema with ")
  print(subschemas(x), max = max, ...)
  print_references(references(x), max)
}
