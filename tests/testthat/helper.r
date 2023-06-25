is_valid_functional_dependency <- function(x) {
  expect_s3_class(x, "functional_dependency")
  attrs <- attr(x, "attrs")
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

is_valid_relation_schema <- function(x) {
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
    \(ks) all(vapply(ks, \(k) !is.unsorted(match(k, all_attrs(x))), logical(1))),
    logical(1)
  )))
  expect_true(all(vapply(
    nonprime_attrs,
    \(as) all(vapply(as, \(a) !is.unsorted(match(a, all_attrs(x))), logical(1))),
    logical(1)
  )))
  expect_true(all(vapply(keys, Negate(anyDuplicated), logical(1))))
  expect_lte(sum(vapply(keys, identical, logical(1), list(character()))), 1L)

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
    attr(x, "all_attrs")
  )
  expect_true(!anyDuplicated(implied_fds))
}

is_valid_database_schema <- function(x) {
  expect_s3_class(x, "database_schema")
  expect_true(!anyDuplicated(x$relation_names))
  expect_true(all(nchar(x$relation_names) > 0L))
  expect_identical(length(x$attrs), length(x$keys))
  key_els <- lapply(x$keys, \(ks) unique(unlist(ks)))
  expect_identical(
    Map(\(as, n) as[seq_len(n)], x$attrs, lengths(key_els)),
    key_els
  )
  nonprime_attrs <- Map(
    \(as, n) as[setdiff(seq_along(as), seq_len(n))],
    x$attrs,
    lengths(key_els)
  )
  expect_true(all(vapply(
    x$keys,
    \(ks) all(vapply(ks, \(k) !is.unsorted(match(k, x$all_attrs)), logical(1))),
    logical(1)
  )))
  expect_true(all(vapply(
    nonprime_attrs,
    \(as) all(vapply(as, \(a) !is.unsorted(match(a, x$all_attrs)), logical(1))),
    logical(1)
  )))
  expect_true(all(vapply(x$keys, Negate(anyDuplicated), logical(1))))
  expect_lte(sum(vapply(x$keys, identical, logical(1), list(character()))), 1L)

  implied_fds <- functional_dependency(
    unlist(
      Map(
        \(ks, as) {
          unlist(
            lapply(ks, \(k) lapply(setdiff(as, k), \(a) list(k, a))),
            recursive = FALSE
          )
        },
        x$keys,
        x$attrs
      ),
      recursive = FALSE
    ),
    x$all_attrs
  )
  expect_true(!anyDuplicated(implied_fds))

  if (!is.null(x$relationships)) {
    fks <- x$relationships
    for (fk in fks) {
      expect_length(fk, 2L)
      expect_identical(lengths(fk), 2:1)
      expect_true(is.integer(fk[[1]]))
      expect_true(is.character(fk[[2]]))
      expect_false(fk[[1]][1] == fk[[1]][2])
      expect_true(is.element(fk[[2]], x$attrs[[fk[[1]][1]]]))
      expect_true(is.element(fk[[2]], x$attrs[[fk[[1]][2]]]))
    }
    expect_true(!anyDuplicated(fks))
    fk_relations <- lapply(fks, "[[", 1L)
    fk_children <- vapply(fk_relations, "[", integer(1), 1L)
    fk_parents <- vapply(fk_relations, "[", integer(1), 2L)
    fk_parent_sets <- split(fk_parents, fk_children)
    children <- strtoi(names(fk_parent_sets))
    nonchildren <- setdiff(seq_along(x$relation_names), children)
    Map(
      expect_setequal,
      fk_parent_sets[as.character(children)],
      x$parents[children]
    )
    lapply(x$parents[nonchildren], expect_identical, integer())
  }
}

is_valid_database <- function(x) {
  expect_s3_class(x, "database")
  expect_setequal(names(x), c("name", "relations", "relationships", "attributes"))

  expect_is(x$name, "character")

  expect_true(!anyDuplicated(names(x$relations)))
  expect_true(all(nchar(names(x$relations)) > 0L))

  rel_keys <- Map(\(r) r$keys, x$relations)
  rel_key_els <- Map(\(ks) unique(unlist(ks)), rel_keys)
  rel_attrs <- Map(\(r) names(r$df), x$relations)
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
    \(ks) all(vapply(ks, \(k) !is.unsorted(match(k, x$attributes)), logical(1))),
    logical(1)
  )))
  expect_true(all(vapply(
    nonprime_attrs,
    \(as) all(vapply(as, \(a) !is.unsorted(match(a, x$attributes)), logical(1))),
    logical(1)
  )))
  expect_true(all(vapply(rel_keys, Negate(anyDuplicated), logical(1))))
  expect_lte(sum(vapply(rel_keys, identical, logical(1), list(character()))), 1L)
  expect_true(all(vapply(
    x$relations,
    \(r) all(vapply(
      r$keys,
      \(k) !anyDuplicated(r$df[, k, drop = FALSE]),
      logical(1)
    )),
    logical(1)
  )))

  fks <- x$relationships
  for (fk in fks) {
    expect_is(fk, "character")
    expect_length(fk, 4L)
    expect_false(fk[1] == fk[3])
    expect_true(is.element(fk[2], names(x$relations[[fk[1]]]$df)))
    expect_true(is.element(fk[4], names(x$relations[[fk[3]]]$df)))
    expect_true(all(is.element(
      x$relations[[fk[1]]]$df[[fk[2]]],
      x$relations[[fk[3]]]$df[[fk[4]]]
    )))
  }
  expect_true(!anyDuplicated(fks))
  fk_children <- vapply(fks, "[", character(1), 1L)
  fk_parents <- vapply(fks, "[", character(1), 3L)
  fk_parent_sets <- split(fk_parents, fk_children)
  children <- names(fk_parent_sets)
  nonchildren <- setdiff(names(x$relations), children)
  Map(
    expect_setequal,
    fk_parent_sets[children],
    lapply(x$relations[children], \(r) r$parents)
  )
  lapply(
    lapply(x$relations[nonchildren], \(r) r$parents),
    expect_identical,
    character()
  )
}

expect_identical_unordered_table <- function(new, original) {
  expect_true(df_equiv(new, original))
}

gen_df <- function(nrow, ncol, minrow = 0L, remove_dup_rows = FALSE) {
  gen_ncol <- gen.sample(seq.int(minrow, ncol), 1)
  gen_len <- gen.sample(seq.int(minrow, nrow), 1)
  gen_classes <- generate(for (ncol in gen_ncol) {
    classes <- c("logical", "integer", "numeric", "character")
    gen.sample(classes, ncol, replace = TRUE)
  })
  gen_lst <- generate(
    for (classes in gen_classes) {
      generate(
        for (len_inc in gen_len) {
          generate(
            for (nms in gen_attr_names(length(classes), 9)) {
              lapply(
                classes,
                \(class) gen.sample(
                  as(c(FALSE, TRUE, NA), class),
                  len_inc,
                  replace = TRUE
                )
              ) |>
                setNames(nms)
            }
          )
        }
      )
    }
  )
  generate(for (lst in gen_lst) {
    if (remove_dup_rows)
      unique(as.data.frame(lst))
    else
      as.data.frame(lst)
  })
}

gen_attr_name <- function(len) {
  gen.sample(c(letters, "_", " ", "."), gen.int(len)) |>
    gen.with(\(attr_name) paste(attr_name, collapse = ""))
}

gen_attr_names <- function(n, len) {
  gen_attr_name(len) |>
    gen.c(of = n) |>
    # as.character for length-0 NULL value
    gen.with(\(attr_names) make.unique(as.character(attr_names)))
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

# generating key / determinant set lists
gen.nonempty_list <- function(generator, to)
  gen.list(generator, from = 1, to = to)
gen.emptyable_list <- function(generator, to)
  gen.list(generator, from = 0, to = to)
gen.list_with_dups <- function(generator, n_unique)
  gen.nonempty_list(generator, n_unique) |>
  gen.and_then(\(lst) gen.sample(lst, ceiling(1.5*length(lst)), replace = TRUE))

# functional utility functions for tests
`%>>%` <- function(fn1, fn2) function(x) fn2(fn1(x))
expect_biequal <- function(fn1, fn2) function(x) expect_equal(fn1(x), fn2(x))
expect_biidentical <- function(fn1, fn2)
  function(x) expect_identical(fn1(x), fn2(x))
split_by <- function(fn, ...) function(x) split(x, fn(x), ...)
subset_by <- function(fn) function(x) x[fn(x)]
sort_by <- function(fn) function(x) x[order(fn(x))]
if_discard_else <- function(cond, fn)
  function(x) if (cond(x)) discard() else fn(x)
uncurry <- function(fn) function(x) fn(x[[1]], x[[2]])
with_args <- function(fn, ...) function(x) fn(x, ...)
apply_both <- function(fn1, fn2) function(x) {fn1(x); fn2(x)}
dup <- function(x) list(x, x)
onRight <- function(f) function(x) list(x[[1]], f(x[[2]]))
