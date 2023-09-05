is_valid_functional_dependency <- function(x) {
  expect_s3_class(x, "functional_dependency")
  attrs <- attrs_order(x)
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
      attr(x, "attrs_order")
    )
    expect_true(!anyDuplicated(implied_fds))
  }
}

is_valid_database_schema <- function(
    x,
    unique = FALSE,
    single_empty_key = FALSE,
    same_attr_name = FALSE
) {
  is_valid_relation_schema(x, unique, single_empty_key)

  expect_s3_class(x, "database_schema")
  fks <- relationships(x)

  for (fk in fks) {
    expect_true(is.character(fk))
    expect_length(fk, 4L)
    expect_false(fk[[1]] == fk[[3]])
    if (same_attr_name)
      expect_identical(fk[[2]], fk[[4]])
    expect_true(all(is.element(fk[c(1L, 3L)], names(x))))
    expect_true(is.element(fk[[2]], attrs(x)[[fk[[1]]]]))
    expect_true(is.element(fk[[4]], attrs(x)[[fk[[3]]]]))
  }
  if (unique) expect_true(!anyDuplicated(fks))
}

is_valid_relation <- function(x) {
  expect_s3_class(x, "relation")

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

is_valid_database <- function(x) {
  expect_s3_class(x, "database")
  is_valid_relation(x)

  fks <- relationships(x)
  for (fk in fks) {
    expect_is(fk, "character")
    expect_length(fk, 4L)
    expect_false(fk[1] == fk[3])
    expect_true(is.element(fk[2], names(x[[fk[1]]]$df)))
    expect_true(is.element(fk[4], names(x[[fk[3]]]$df)))
    expect_true(all(is.element(
      x[[fk[1]]]$df[[fk[2]]],
      x[[fk[3]]]$df[[fk[4]]]
    )))
  }
  expect_true(!anyDuplicated(fks))
  fk_children <- vapply(fks, "[", character(1), 1L)
  fk_parents <- vapply(fks, "[", character(1), 3L)
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
  asable_classes <- c("logical", "integer", "numeric", "character")
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
  lapply(
    classes,
    with_args(as, object = c(FALSE, TRUE, NA)) %>>%
      with_args(gen.sample, size = n_records, replace = TRUE)
  ) |>
    gen.with(with_args(setNames, nm = nms)) |>
    gen.with(as.data.frame %>>% (if (remove_dup_rows) unique else identity))
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
    gen.and_then(\(attrs) list(gen.pure(attrs), gen.keys(attrs))) |>
    gen.list(from = from, to = to) |>
    gen.with(\(lst) {
      # only one schema can have an empty key
      rels_with_empty_keys <- which(vapply(
        lst,
        \(schema) any(lengths(schema[[2]]) == 0L),
        logical(1)
      ))
      if (length(rels_with_empty_keys) > 1L)
        lst <- lst[-rels_with_empty_keys[-1]]

      nms <- make.names(
        vapply(lst, \(rel) name_dataframe(rel[[2]][[1]]), character(1)),
        unique = TRUE
      )
      list(setNames(lst, nms), x)
    }) |>
    gen.with(\(lst) {
      do.call(relation_schema, lst)
    })
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
            gen.with(\(df) list(df = df, keys = r$keys)) |>
            gen.with(\(rel) {
              for (k in rel$keys) {
                rel$df <- rel$df[
                  !duplicated(rel$df[, k, drop = FALSE]),
                  ,
                  drop  = FALSE
                ]
              }
              rel
            })
        }
      ) |>
        gen.with(with_args(relation, attrs_order = attrs_order(empty_rel)))
    })
}

gen.relationships_same_attrs <- function(rs) {
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
        lst <- lapply(
          citers,
          \(citer) lapply(
            k,
            \(a) c(names(rs)[[citer]], a, names(rs)[[n]], a)
          )
        )
        if (length(lst) == 0) list() else unlist(lst, recursive = FALSE)
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
        if (length(lst) == 0L) list() else unique(unlist(lst, recursive = FALSE))
      })
  }
  lapply(seq_along(rs), gen.relationships_for_index, rs = rs) |>
    gen.with(\(lst) if (length(lst) == 0L) list() else unlist(lst, recursive = FALSE))
}

gen.relationships_different_attrs <- function(rs) {
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
                Map(
                  \(key_attr, citing_attr) c(
                    names(rs)[[citer]],
                    citing_attr,
                    names(rs)[[n]],
                    key_attr
                  ),
                  k,
                  attrs
                )
              })
          }
        )
      }) |>
      gen.with(\(lst) {
        if (length(lst) == 0) list() else unlist(lst, recursive = FALSE)
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
        if (length(lst) == 0L) list() else unique(unlist(lst, recursive = FALSE))
      })
  }
  lapply(seq_along(rs), gen.relationships_for_index, rs = rs) |>
    gen.with(\(lst) if (length(lst) == 0L) list() else unlist(lst, recursive = FALSE))
}

gen.relationships <- function(rs) {
  gen.choice(
    gen.relationships_same_attrs(rs),
    gen.relationships_different_attrs(rs)
  )
}

gen.database_schema <- function(x, from, to, same_attr_name = FALSE) {
  gen.relation_schema(x, from, to) |>
    gen.and_then(\(rs) {
      list(
        gen.pure(rs),
        if (same_attr_name)
          gen.relationships_same_attrs(rs)
        else
          gen.relationships(rs))
    }) |>
    gen.with(\(lst) do.call(database_schema, lst))
}

gen.database <- function(nrow_to, ncol_to, nrow_from = 0L, unique = FALSE) {
  # placeholder, need to generate more directly
  gen_df(nrow_to, ncol_to, minrow = nrow_from, remove_dup_rows = unique) |>
    gen.with(autodb)
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
