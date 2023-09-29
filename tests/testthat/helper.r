is_valid_functional_dependency <- function(x) {
  expect_s3_class(x, "functional_dependency")
  attrs <- attrs_order(x)
  classes <- attrs_class(x)
  expect_true(all(lengths(unclass(x)) == 2L))
  expect_silent(dependent(x))
  expect_true(all(lengths(dependent(x)) == 1L))
  expect_true(all(vapply(detset(x), is.character, logical(1))))
  lhs <- detset(x)

  expect_true(all(is.element(unlist(x), attrs)))
  expect_true(all(
    mapply(\(dets, dep) !is.element(dep, dets), detset(x), dependent(x))
  ))
  expect_true(all(vapply(
    lhs,
    \(detset) !is.unsorted(match(detset, attrs)),
    logical(1)
  )))

  expect_identical(class(attrs_class(x)), "list")
  expect_true(identical(names(classes), attrs))
  expect_true(all(vapply(classes, is.character, logical(1))))
}

is_valid_minimal_functional_dependency <- function(x) {
  is_valid_functional_dependency(x)
  grouped <- split(detset(x), dependent(x))
  expect_true(!any(
    vapply(
      grouped,
      \(detsets) anyDuplicated(detsets) ||
        any(outer(
          detsets,
          detsets,
          Vectorize(\(d1, d2) {
            both <- intersect(d1, d2)
            !setequal(d1, d2) &&
              (setequal(both, d1) || setequal(both, d2))
          })
        )),
      logical(1)
    )
  ))
}

is_valid_relation_schema <- function(x, unique = FALSE, single_empty_key = FALSE) {
  expect_s3_class(x, "relation_schema")
  expect_true(is.character(names(x)))
  expect_true(!anyDuplicated(names(x)))
  expect_true(all(nchar(names(x)) > 0L))
  expect_true(all(lengths(unclass(x)) == 2))
  attrs <- attrs(x)
  keys <- keys(x)
  key_els <- lapply(keys, \(ks) unique(unlist(ks)))
  expect_identical(
    Map(\(as, n) as[seq_len(n)], attrs, lengths(key_els)),
    key_els
  )
  nonprime_attrs <- Map(
    \(as, n) as[setdiff(seq_along(as), seq_len(n))],
    attrs,
    lengths(key_els)
  )
  expect_true(all(vapply(
    keys,
    \(ks) all(vapply(ks, \(k) !is.unsorted(match(k, attrs_order(x))), logical(1))),
    logical(1)
  )))
  expect_true(all(vapply(
    nonprime_attrs,
    \(as) all(vapply(as, \(a) !is.unsorted(match(a, attrs_order(x))), logical(1))),
    logical(1)
  )))
  expect_true(all(vapply(keys, Negate(anyDuplicated), logical(1))))
  if (single_empty_key)
    expect_lte(sum(vapply(keys, identical, logical(1), list(character()))), 1L)
  expect_identical(length(attrs_order(x)), length(attrs_class(x)))
  expect_identical(class(attrs_class(x)), "list")
  expect_true(all(vapply(attrs_class(x), is.character, logical(1))))
  expect_identical(names(attrs_class(x)), attrs_order(x))

  if (unique) {
    expect_true(!anyDuplicated(x))

    implied_fds <- functional_dependency(
      unlist(
        Map(
          \(ks, as) {
            unlist(
              lapply(ks, \(k) lapply(setdiff(as, k), \(a) list(k, a))),
              recursive = FALSE
            )
          },
          keys,
          attrs
        ),
        recursive = FALSE
      ),
      attrs_order(x)
    )
    expect_true(!anyDuplicated(implied_fds))
  }
}

is_valid_relationships <- function(
  x,
  same_attr_name = FALSE,
  single_key_pairs = FALSE
) {
  act <- quasi_label(rlang::enquo(x), arg = "x")

  relationships <- relationships(x)
  attrs <- attrs(x)
  if (length(relationships) == 0L)
    return(invisible(act$val))

  # former condition is temporary until relationships are properly grouped
  if (single_key_pairs && anyDuplicated(relationships))
    fail(sprintf("%s has duplicate relationships", act$lab))
  for (fk in relationships) {
    if (!is(fk, "list"))
      fail(sprintf(
        "%s has non-list relationships",
        act$lab
      ))
    if (length(fk) != 4L)
      fail(sprintf(
        "%s has non-length-four relationships",
        act$lab
      ))
    if (!is.character(fk[[1]]))
      fail(sprintf(
        "%s has non-character relationship child names",
        act$lab
      ))
    if (!is.character(fk[[2]]))
      fail(sprintf(
        "%s has non-character relationship child attributes",
        act$lab
      ))
    if (!is.character(fk[[3]]))
      fail(sprintf(
        "%s has non-character relationship parent names",
        act$lab
      ))
    if (!is.character(fk[[4]]))
      fail(sprintf(
        "%s has non-character relationship parent attributes",
        act$lab
      ))
    if (!all(is.element(unlist(fk[c(1L, 3L)]), names(attrs))))
      fail(sprintf(
        "%s has relationships over non-present relation names",
        act$lab
      ))
    if (fk[[1]] == fk[[3]]) # no self-references, relax this?
      fail(sprintf(
        "%s has self-references in relationships",
        act$lab
      ))
    if (same_attr_name && !identical(fk[[2]], fk[[4]]))
      fail(sprintf(
        "%s has non-matching attribute names in relationships",
        act$lab
      ))
    if (anyDuplicated(fk[[2]]))
      fail(sprintf(
        "%s has relationships with non-unique child attribute names",
        act$lab
      ))
    if (anyDuplicated(fk[[4]]))
      fail(sprintf(
        "%s has relationships with non-unique parent attribute names",
        act$lab
      ))
    if (length(fk[[2]]) == 0L || length(fk[[4]]) == 0L)
      fail(sprintf(
        "%s has relationships with zero-length attribute sets",
        act$lab
      ))
    if (length(fk[[2]]) != length(fk[[4]]))
      fail(sprintf(
        "%s has relationships with different attribute set lengths",
        act$lab
      ))
    if (!all(is.element(fk[[2]], attrs[[fk[[1]]]])))
      fail(sprintf(
        "%s has invalid child attribute names in relationships",
        act$lab
      ))
    if (!all(is.element(fk[[4]], attrs[[fk[[3]]]])))
      fail(sprintf(
        "%s has invalid parent attribute names in relationships",
        act$lab
      ))
  }
  if (single_key_pairs) {
    relnames_df <- as.data.frame(do.call(
      rbind,
      lapply(relationships, \(r) unlist(r[c(1L, 3L)]))
    ))
    if (anyDuplicated(relnames_df))
      fail(sprintf(
        "%s has relationship pairs with multiple keys",
        act$lab
      ))
  }

  invisible(act$val)
}

is_valid_database_schema <- function(
  x,
  unique = FALSE,
  single_empty_key = FALSE,
  same_attr_name = FALSE,
  single_key_pairs = FALSE
) {
  is_valid_relation_schema(x, unique, single_empty_key)
  expect_s3_class(x, "database_schema")
  is_valid_relationships(x, same_attr_name, single_key_pairs)
}

is_valid_relation <- function(x) {
  expect_s3_class(x, "relation")

  expect_true(is.character(names(x)))
  expect_true(!anyDuplicated(names(x)))
  expect_true(all(nchar(names(x)) > 0L))

  rel_keys <- Map(\(r) r$keys, x)
  rel_key_els <- Map(\(ks) unique(unlist(ks)), rel_keys)
  rel_attrs <- Map(\(r) names(r$df), x)
  Map(
    \(ks, as) expect_identical(as[seq_along(ks)], ks),
    rel_key_els,
    rel_attrs
  )
  nonprime_attrs <- Map(
    \(ks, as) as[-seq_along(ks)],
    rel_key_els,
    rel_attrs
  )
  expect_true(all(vapply(
    rel_keys,
    \(ks) all(vapply(ks, \(k) !is.unsorted(match(k, attrs_order(x))), logical(1))),
    logical(1)
  )))
  expect_true(all(vapply(
    nonprime_attrs,
    \(as) all(vapply(as, \(a) !is.unsorted(match(a, attrs_order(x))), logical(1))),
    logical(1)
  )))
  expect_true(all(vapply(rel_keys, Negate(anyDuplicated), logical(1))))
  expect_lte(sum(vapply(rel_keys, identical, logical(1), list(character()))), 1L)
  expect_true(all(vapply(
    x,
    \(r) all(vapply(
      r$keys,
      \(k) !anyDuplicated(r$df[, k, drop = FALSE]),
      logical(1)
    )),
    logical(1)
  )))
}

is_valid_database <- function(
  x,
  same_attr_name = FALSE,
  single_key_pairs = FALSE
) {
  expect_s3_class(x, "database")
  is_valid_relation(x)

  fks <- relationships(x)
  is_valid_relationships(x, same_attr_name, single_key_pairs)
  for (fk in fks) {
    expect_true(identical(
      nrow(x[[fk[[1]]]]$df),
      nrow(merge(
        x[[fk[[1]]]]$df[, fk[[2]], drop = FALSE],
        x[[fk[[3]]]]$df[, fk[[4]], drop = FALSE],
        by.x = fk[[2]],
        by.y = fk[[4]]
      ))))
  }
  fk_children <- vapply(fks, "[[", character(1), 1L)
  fk_parents <- vapply(fks, "[[", character(1), 3L)
  fk_parent_sets <- split(fk_parents, fk_children)
  children <- names(fk_parent_sets)
  nonchildren <- setdiff(names(x), children)
}

expect_identical_unordered_table <- function(new, original) {
  expect_true(df_equiv(new, original))
}

gen_df <- function(
  nrow,
  ncol,
  minrow = 0L,
  mincol = 0L,
  remove_dup_rows = FALSE
) {
  asable_classes <- c("logical", "integer", "numeric", "character", "factor")
  list(
    gen.sample(seq.int(min(mincol, ncol), ncol), 1) |>
      gen.and_then(\(n) list(
        classes = gen.element(asable_classes) |> gen.c(of = n),
        nms = gen_attr_names(n, 9)
      )),
    n_records = gen.sample(seq.int(min(minrow, nrow), nrow), 1)
  ) |>
    gen.with(\(lst) c(lst[[1]], lst[2], list(remove_dup_rows = remove_dup_rows))) |>
    gen.and_then(uncurry(gen.df_fixed_ranges))
}

gen.df_fixed_ranges <- function(classes, nms, n_records, remove_dup_rows) {
  as_fns <- list(
    logical = as.logical,
    integer = as.integer,
    numeric = as.numeric,
    character = as.character,
    factor = with_args(factor, levels = c(FALSE, TRUE))
  )
  if (length(classes) == 0L)
    return(gen.pure(data.frame(a = NA)[, FALSE, drop = FALSE]))
  lapply(
    classes,
    \(cl) {
      as_fns[[cl]](c(FALSE, TRUE, NA)) |>
        gen.sample(size = n_records, replace = TRUE)
    }
  ) |>
    gen.with(
      with_args(setNames, nm = nms) %>>%
        as.data.frame %>>%
        (if (remove_dup_rows) unique else identity)
    )
}

gen_attr_name <- function(len) {
  gen.sample(c(letters, "_", " ", "."), gen.int(len)) |>
    gen.with(\(attr_name) paste(attr_name, collapse = ""))
}

gen_attr_names <- function(n, len) {
  gen_attr_name(len) |>
    gen.c(of = n) |>
    # as.character for length-0 NULL value
    gen.with(as.character %>>% make.unique)
}

gen_unique_dets <- function(n_attrs, n, max_dets) {
  # should also check no redundancy
  gen.subsequence(setdiff(seq_len(n_attrs), n)) |>
    gen.list(from = 0, to = min(max_dets, n_attrs - 1)) |>
    gen.with(unique)
}

gen_detset_lists <- function(n_attrs, max_dets) {
  md <- min(max_dets, n_attrs - 1)
  gen.structure(lapply(
    seq_len(n_attrs),
    function(n) {
      gen_unique_dets(n_attrs, n, md)
    }
  ))
}

gen_named_flat_deps_fixed_size <- function(attrs, n, max_detset_size, unique = TRUE) {
  list(
    gen.sample(attrs, n, replace = TRUE),
    gen.sample(attrs, gen.sample(0:max_detset_size)) |>
      gen.list(of = n)
  ) |>
    gen.with(\(lst) functional_dependency(
      Map(\(x, y) list(setdiff(x, y), y), lst[[2]], lst[[1]]),
      attrs,
      unique = unique
    ))
}

gen_named_flat_deps <- function(
  attrs,
  max_detset_size,
  from = 0L,
  to = NULL,
  of = NULL
) {
  max_detset_size <- min(max_detset_size, length(attrs) - 1L)
  (
    if (missing(of) || is.null(of))
      gen.sample(seq.int(from, to), 1)
    else
      gen.pure(of)
  ) |>
    gen.and_then(\(m) gen_named_flat_deps_fixed_size(attrs, m, max_detset_size))
}

gen_flat_deps_fixed_names <- function(
  n_attrs,
  max_detset_size,
  from = 0L,
  to = NULL,
  of = NULL
) {
  attrs <- LETTERS[seq.int(n_attrs)]
  gen_named_flat_deps(attrs, max_detset_size, from, to, of)
}

gen_flat_deps <- function(
  n_attrs,
  max_detset_size,
  max_attr_nchar = 9,
  from = 0L,
  to = NULL,
  of = NULL
) {
  gen_attr_names(n_attrs, max_attr_nchar) |>
    gen.and_then(\(attrs) gen_named_flat_deps(attrs, max_detset_size, from, to, of))
}

gen.keys <- function(attrs) {
  gen.subsequence(attrs) |>
    gen.list(to = 3) |>
    gen.with(\(keys) {
      uniq <- unique(keys)
      superset <- outer(
        uniq,
        uniq,
        Vectorize(\(sup, sub) {
          all(is.element(sub, sup)) && !all(is.element(sup, sub))
        })
      )
      rem <- uniq[!apply(superset, 1, any)]
      rem[keys_order(lapply(rem, match, attrs))]
    })
}
gen.relation_schema <- function(x, from, to) {
  gen.subsequence(x) |>
    gen.and_then(\(attrs) list(
      schemas = list(gen.pure(attrs), gen.keys(attrs)) |>
        gen.list(from = from, to = to),
      classes <- gen.element(list(
        "logical",
        "integer",
        "numeric",
        "character",
        c("nested1", "nested2")
      )) |>
        gen.list(of = length(x))
    )) |>
    gen.with(\(lst) {
      # only one schema can have an empty key
      rels_with_empty_keys <- which(vapply(
        lst[[1]],
        \(schema) any(lengths(schema[[2]]) == 0L),
        logical(1)
      ))
      if (length(rels_with_empty_keys) > 1L)
        lst[[1]] <- lst[[1]][-rels_with_empty_keys[-1]]

      nms <- make.names(
        vapply(lst[[1]], \(rel) name_dataframe(rel[[2]][[1]]), character(1)),
        unique = TRUE
      )
      list(setNames(lst[[1]], nms), x, setNames(lst[[2]], x))
    }) |>
    gen.with(\(lst) {
      do.call(relation_schema, lst)
    })
}

gen.attrs_class <- function(nm) {
  gen.element(list(
    "logical",
    "integer",
    "numeric",
    "character",
    c("nested1", "nested2")
  )) |>
    gen.list(of = length(nm)) |>
    gen.with(with_args(setNames, nm = nm))
}

gen.relation_schema_given_attrs_class <- function(attrs_class, from, to) {
  gen.subsequence(names(attrs_class)) |>
    gen.and_then(\(attrs) gen.relation_schema(attrs, from, to)) |>
    gen.with(\(rs) {attrs_class(rs) <- attrs_class[attrs_order(rs)]; rs})
}

gen.empty_relation_schema_given_attrs_class <- function(attrs_class) {
  gen.subsequence(names(attrs_class)) |>
    gen.with(\(attrs) relation_schema(
      setNames(list(), character()),
      attrs_order = attrs,
      attrs_class = attrs_class[attrs]
    ))
}

gen.relation <- function(x, from, to) {
  gen.relation_schema(x, from, to) |>
    gen.and_then(gen.relation_from_schema)
}

gen.relation_from_schema <- function(rs) {
  create(rs) |>
    gen.and_then(\(empty_rel) {
      lapply(
        empty_rel,
        \(r) {
          gen.sample(0:10, 1L) |>
            gen.and_then(with_args(
              gen.df_fixed_ranges,
              classes = rep("logical", ncol(r$df)),
              nms = names(r$df),
              remove_dup_rows = TRUE
            )) |>
            gen.with(\(df) list(
              df = remove_key_violations(df, r$keys),
              keys = r$keys
            ))
        }
      ) |>
        gen.with(with_args(relation, attrs_order = attrs_order(empty_rel)))
    })
}

remove_key_violations <- function(df, keys) {
  Reduce(
    \(df, key) df[!duplicated(df[, key, drop = FALSE]), , drop  = FALSE],
    keys,
    init = df
  )
}

remove_insertion_key_violations <- function(df, relation) {
  Reduce(
    \(df, rel) {
      Reduce(
        \(df, key) {
          remove <- if (length(key) == 0L)
            rep(nrow(rel$df) + nrow(df) > 1L, nrow(df))
          else
            duplicated(rbind(
              rel$df[, key, drop = FALSE],
              df[, key, drop = FALSE]
            ))[-seq_len(nrow(rel$df))]
          df[!remove, , drop = FALSE]
        },
        rel$keys,
        init = df
      )
    },
    relation,
    init = df
  )
}

gen.relationships_same_attrs <- function(rs, single_key_pairs) {
  gen.relationships_for_index_and_key <- function(rs, n, k) {
    contains_key <- setdiff(
      which(vapply(
        attrs(rs),
        \(as) all(is.element(k, as)),
        logical(1)
      )),
      n
    )
    gen.subsequence(contains_key) |>
      gen.with(\(citers) {
        lapply(
          citers,
          \(citer) list(
            names(rs)[[citer]],
            k,
            names(rs)[[n]],
            k
          )
        )
      })
  }
  gen.relationships_for_index <- function(rs, n) {
    ks <- keys(rs)[[n]]
    lapply(
      ks[lengths(ks) > 0L],
      gen.relationships_for_index_and_key,
      rs = rs,
      n = n
    ) |>
      gen.with(\(lst) {
        if (length(lst) == 0L) list() else unlist(lst, recursive = FALSE)
      }) |>
      gen.with(\(rels) {
        if (single_key_pairs)
          rels[!duplicated(lapply(rels, \(r) c(r[[1]], r[[3]])))]
        else
          rels
      })
  }
  lapply(seq_along(rs), gen.relationships_for_index, rs = rs) |>
    gen.with(\(lst) if (length(lst) == 0L) list() else unlist(lst, recursive = FALSE))
}

gen.relationships_different_attrs <- function(rs, single_key_pairs) {
  gen.relationships_for_index_and_key <- function(rs, n, k) {
    contains_key_length <- setdiff(
      which(vapply(
        attrs(rs),
        \(as) length(as) >= length(k),
        logical(1)
      )),
      n
    )
    gen.subsequence(contains_key_length) |>
      gen.and_then(\(citers) {
        lapply(
          citers,
          \(citer) {
            gen.sample(attrs(rs)[[citer]], length(k)) |>
              gen.with(\(attrs) {
                list(
                  names(rs)[[citer]],
                  attrs,
                  names(rs)[[n]],
                  k
                )
              })
          }
        )
      })
  }
  gen.relationships_for_index <- function(rs, n) {
    ks <- keys(rs)[[n]]
    lapply(
      ks[lengths(ks) > 0L],
      gen.relationships_for_index_and_key,
      rs = rs,
      n = n
    ) |>
      gen.with(\(lst) {
        if (length(lst) == 0L) list() else unlist(lst, recursive = FALSE)
      }) |>
      gen.with(\(rels) {
        if (single_key_pairs)
          rels[!duplicated(lapply(rels, \(r) c(r[[1]], r[[3]])))]
        else
          rels
      })
  }
  lapply(seq_along(rs), gen.relationships_for_index, rs = rs) |>
    gen.with(\(lst) if (length(lst) == 0L) list() else unlist(lst, recursive = FALSE))
}

gen.relationships <- function(rs, single_key_pairs) {
  gen.choice(
    gen.relationships_same_attrs(rs, single_key_pairs),
    gen.relationships_different_attrs(rs, single_key_pairs)
  )
}

gen.database_schema <- function(
  x,
  from,
  to,
  same_attr_name = FALSE,
  single_key_pairs = FALSE
) {
  gen.relation_schema(x, from, to) |>
    gen.and_then(\(rs) {
      list(
        gen.pure(rs),
        if (same_attr_name)
          gen.relationships_same_attrs(rs, single_key_pairs)
        else
          gen.relationships(rs, single_key_pairs))
    }) |>
    gen.with(\(lst) do.call(database_schema, lst))
}

gen.database_schema_given_attrs_class <- function(
  attrs_class,
  from,
  to,
  same_attr_name = FALSE,
  single_key_pairs = FALSE
) {
  gen.subsequence(names(attrs_class)) |>
    gen.and_then(\(attrs) gen.database_schema(
      attrs,
      from,
      to,
      same_attr_name,
      single_key_pairs
    )) |>
    gen.with(\(ds) {attrs_class(ds) <- attrs_class[attrs_order(ds)]; ds})
}

gen.database <- function(
  x,
  from,
  to,
  same_attr_name = TRUE,
  single_key_pairs = TRUE
) {
  gen.database_schema(
    x,
    from,
    to,
    same_attr_name = same_attr_name,
    single_key_pairs = single_key_pairs
  ) |>
    gen.and_then(\(ds) {
      gen.relation_from_schema(ds) |>
        gen.with(
          with_args(
            remove_relationship_violations,
            relationships = relationships(ds)
          ) %>>%
            with_args(database, relationships = relationships(ds))
        )
    })
}

remove_relationship_violations <- function(relation, relationships) {
  if (length(relationships) == 0L)
    return(relation)
  change <- TRUE
  while (change) {
    change <- FALSE
    for (ref in relationships) {
      child_name <- ref[[1]]
      child <- relation[[child_name]]$df[, ref[[2]], drop = FALSE]
      if (nrow(child) > 0L) {
        parent_name <- ref[[3]]
        parent <- relation[[parent_name]]$df[, ref[[4]], drop = FALSE]
        parent_keys <- relation[[parent_name]]$keys
        stopifnot(is.element(list(ref[[4]]), parent_keys))
        valid <- vapply(
          seq_len(nrow(child)),
          \(n) nrow(merge(
            child[n, , drop = FALSE],
            parent,
            by.x = ref[[2]],
            by.y = ref[[4]]
          )) > 0L,
          logical(1)
        )
        relation[[child_name]]$df <- relation[[child_name]]$df[valid, , drop = FALSE]
        if (!all(valid))
          change <- TRUE
      }
    }
  }
  relation
}

remove_insertion_relationship_violations <- function(df, database) {
  if (length(relationships(database)) == 0L)
    return(df)
  change <- TRUE
  while (change) {
    change <- FALSE
    for (ref in relationships(database)) {
      child_name <- ref[[1]]
      child <- rbind(
        database[[child_name]]$df[, ref[[2]], drop = FALSE],
        df[, ref[[2]], drop = FALSE]
      )
      if (nrow(child) > 0L) {
        parent_name <- ref[[3]]
        parent <- rbind(
          database[[parent_name]]$df[, ref[[4]], drop = FALSE],
          df[, ref[[4]], drop = FALSE]
        )
        valid <- vapply(
          seq_len(nrow(child)),
          \(n) nrow(merge(
            child[n, , drop = FALSE],
            parent,
            by.x = ref[[2]],
            by.y = ref[[4]]
          )) > 0L,
          logical(1)
        )
        df <- df[valid[-seq_len(nrow(database[[child_name]]$df))], , drop = FALSE]
        if (!all(valid))
          change <- TRUE
      }
    }
  }
  df
}

# generating key / determinant set lists
gen.nonempty_list <- function(generator, to)
  gen.list(generator, from = 1, to = to)
gen.emptyable_list <- function(generator, to)
  gen.list(generator, from = 0, to = to)
gen.list_with_dups <- function(generator, n_unique)
  gen.nonempty_list(generator, n_unique) |>
  gen.and_then(\(lst) gen.sample(lst, ceiling(1.5*length(lst)), replace = TRUE))

# functional utility functions for tests
`%>>%` <- function(fn1, fn2) function(...) fn2(fn1(...))
expect_biequal <- function(fn1, fn2) function(x) expect_equal(fn1(x), fn2(x))
expect_biidentical <- function(fn1, fn2)
  function(x) expect_identical(fn1(x), fn2(x))
split_by <- function(fn, ...) function(x) split(x, fn(x), ...)
subset_by <- function(fn) function(x) x[fn(x)]
sort_by <- function(fn) function(x) x[order(fn(x))]
if_discard_else <- function(cond, fn)
  function(x) if (cond(x)) discard() else fn(x)
uncurry <- function(fn) function(x) do.call(fn, x)
with_args <- function(fn, ...) {
  lst <- list(...)
  function(...) do.call(fn, c(list(...), lst))
}
apply_both <- function(fn1, fn2) function(x) {fn1(x); fn2(x)}
dup <- function(x) list(x, x)
onLeft <- function(f) function(x) list(f(x[[1]]), x[[2]])
onRight <- function(f) function(x) list(x[[1]], f(x[[2]]))
