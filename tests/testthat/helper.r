# re-implementation of expect_no_error that hedgehog doesn't complain about
# (it doesn't register usage of expect_no_ as an expectation)
expect_errorless <- function(object) {
  expect(
    class(try(object))[1] != "try-error",
    failure_message = "object returned an error"
  )
}

is_valid_functional_dependency <- function(x) {
  expect_s3_class(x, "functional_dependency")
  attrs <- attrs_order(x)
  expect_true(all(lengths(unclass(x)) == 2L))
  expect_silent(dependant(x))
  expect_true(all(lengths(dependant(x)) == 1L))
  expect_true(all(vapply(detset(x), is.character, logical(1))))
  lhs <- detset(x)

  expect_true(all(is.element(unlist(x), attrs)))
  expect_true(all(
    mapply(\(dets, dep) !is.element(dep, dets), detset(x), dependant(x))
  ))
  expect_true(all(vapply(
    lhs,
    \(detset) !is.unsorted(match(detset, attrs)),
    logical(1)
  )))
}

is_valid_minimal_functional_dependency <- function(x) {
  is_valid_functional_dependency(x)
  expect_true(all(outer(x, x, "<=") == diag(length(x))))
}

is_valid_key_set <- function(x) {
  expect_identical(class(x), "list")
  expect_true(all(vapply(x, \(y) class(y)[[1]], character(1)) == "character"))
  expect_gt(length(x), 0) # every relation has at least one key
  expect_true(all(outer(x, x, Vectorize(\(y, z) all(is.element(y, z)))) == diag(length(x))))
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

is_valid_references <- function(
  x,
  same_attr_name = FALSE,
  single_key_pairs = FALSE
) {
  act <- quasi_label(rlang::enquo(x), arg = "x")

  references <- references(x)
  attrs <- attrs(x)
  if (length(references) == 0L)
    return(invisible(act$val))

  # former condition is temporary until references are properly grouped
  if (single_key_pairs && anyDuplicated(references))
    fail(sprintf("%s has duplicate references", act$lab))
  for (fk in references) {
    if (!is(fk, "list"))
      fail(sprintf(
        "%s has non-list references",
        act$lab
      ))
    if (length(fk) != 4L)
      fail(sprintf(
        "%s has non-length-four references",
        act$lab
      ))
    if (!is.character(fk[[1]]))
      fail(sprintf(
        "%s has non-character reference child names",
        act$lab
      ))
    if (!is.character(fk[[2]]))
      fail(sprintf(
        "%s has non-character reference child attributes",
        act$lab
      ))
    if (!is.character(fk[[3]]))
      fail(sprintf(
        "%s has non-character reference parent names",
        act$lab
      ))
    if (!is.character(fk[[4]]))
      fail(sprintf(
        "%s has non-character reference parent attributes",
        act$lab
      ))
    if (!all(is.element(unlist(fk[c(1L, 3L)]), names(attrs))))
      fail(sprintf(
        "%s has references over non-present relation names",
        act$lab
      ))
    if (fk[[1]] == fk[[3]]) # no self-references, relax this?
      fail(sprintf(
        "%s has self-references in references",
        act$lab
      ))
    if (same_attr_name && !identical(fk[[2]], fk[[4]]))
      fail(sprintf(
        "%s has non-matching attribute names in references",
        act$lab
      ))
    if (anyDuplicated(fk[[2]]))
      fail(sprintf(
        "%s has references with non-unique child attribute names",
        act$lab
      ))
    if (anyDuplicated(fk[[4]]))
      fail(sprintf(
        "%s has references with non-unique parent attribute names",
        act$lab
      ))
    if (length(fk[[2]]) == 0L || length(fk[[4]]) == 0L)
      fail(sprintf(
        "%s has references with zero-length attribute sets",
        act$lab
      ))
    if (length(fk[[2]]) != length(fk[[4]]))
      fail(sprintf(
        "%s has references with different attribute set lengths",
        act$lab
      ))
    if (!all(is.element(fk[[2]], attrs[[fk[[1]]]])))
      fail(sprintf(
        "%s has invalid child attribute names in references",
        act$lab
      ))
    if (!all(is.element(fk[[4]], attrs[[fk[[3]]]])))
      fail(sprintf(
        "%s has invalid parent attribute names in references",
        act$lab
      ))
  }
  if (single_key_pairs) {
    relnames_df <- as.data.frame(do.call(
      rbind,
      lapply(references, \(r) unlist(r[c(1L, 3L)]))
    ))
    if (anyDuplicated(relnames_df))
      fail(sprintf(
        "%s has reference pairs with multiple keys",
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
  is_valid_references(x, same_attr_name, single_key_pairs)
}

is_valid_relation <- function(x, unique = FALSE, single_empty_key = FALSE) {
  expect_s3_class(x, "relation")

  expect_true(is.character(names(x)))
  expect_true(!anyDuplicated(names(x)))
  expect_true(all(nchar(names(x)) > 0L))

  rel_keys <- keys(x)
  rel_key_els <- lapply(rel_keys, \(ks) unique(unlist(ks)))
  rel_attrs <- attrs(x)
  key_attrs_first <- mapply(
    \(ks, as) identical(as[seq_along(ks)], ks),
    rel_key_els,
    rel_attrs
  )
  expect_true(all(key_attrs_first))
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
  if (single_empty_key)
    expect_lte(sum(vapply(rel_keys, identical, logical(1), list(character()))), 1L)
  expect_true(all(mapply(
    \(recs, ks) all(vapply(
      ks,
      \(k) !df_anyDuplicated(recs[, k, drop = FALSE]),
      logical(1)
    )),
    records(x),
    rel_keys
  )))
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
          rel_keys,
          rel_attrs
        ),
        recursive = FALSE
      ),
      attrs_order(x)
    )
    expect_true(!anyDuplicated(implied_fds))
  }
}

is_valid_database <- function(
  x,
  unique = FALSE,
  single_empty_key = FALSE,
  same_attr_name = FALSE,
  single_key_pairs = FALSE
) {
  is_valid_relation(x, unique, single_empty_key)
  expect_s3_class(x, "database")

  fks <- references(x)
  is_valid_references(x, same_attr_name, single_key_pairs)
  recs <- records(x)
  for (fk in fks) {
    expect_true(identical(
      nrow(records(x)[[fk[[1]]]]),
      nrow(df_join(
        recs[[fk[[1]]]][, fk[[2]], drop = FALSE],
        recs[[fk[[3]]]][, fk[[4]], drop = FALSE],
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
  expect_true(df_equiv(new, original, digits = NA))
}

gen_df <- function(
  nrow,
  ncol,
  minrow = 0L,
  mincol = 0L,
  remove_dup_rows = FALSE,
  digits = NA,
  variant = c("data.frame", "tibble"),
  atomic = FALSE
) {
  asable_classes <- c(
    "logical",
    "integer",
    "numeric",
    "character",
    "factor",
    if (!atomic) "list",
    if (!atomic) "matrix"
  )
  list(
    gen.element(seq.int(min(mincol, ncol), ncol)) |>
      gen.and_then(\(n) list(
        classes = gen.element(asable_classes) |> gen.c(of = n),
        nms = gen_attr_names(n, 9)
      )),
    n_records = gen.element(seq.int(min(minrow, nrow), nrow)),
    variant = gen.element(variant)
  ) |>
    gen.with(\(lst) c(lst[[1]], lst[2], list(remove_dup_rows = remove_dup_rows), lst[3])) |>
    gen.and_then(uncurry(with_args(gen.df_fixed_ranges, digits = digits)))
}

gen.df_fixed_ranges <- function(
  classes,
  nms,
  n_records,
  remove_dup_rows,
  digits = getOption("digits"),
  variant = c("data.frame", "tibble")
) {
  variant <- match.arg(variant)
  variant <- switch(
    variant,
    data.frame = identity,
    tibble = with_args(tibble::as_tibble, .name_repair = "minimal")
  )
  as_fns <- list(
    logical = gen.element(c(FALSE, TRUE, NA)) |>
      gen.c(of = n_records) |>
      gen.with(as.logical),
    integer = gen.choice(
      gen.shrink(
        function(x) as.integer(shrink.towards(0L)(x)),
        gen.element(-5:5)
      ),
      gen.pure(NA_integer_),
      prob = c(10, 1)
    ) |>
      gen.c(of = n_records) |>
      gen.with(as.integer) |>
      gen.with(as.data.frame.vector),
    numeric = gen.numeric() |>
      gen.c(of = n_records) |>
      gen.with(as.numeric) |>
      gen.and_then(with_args(gen.float_coincide, digits = digits)) |>
      gen.with(as.data.frame.vector),
    character = gen.element(c("FALSE", "TRUE", NA_character_)) |>
      gen.c(of = n_records) |>
      gen.with(as.character) |>
      gen.with(as.data.frame.vector),
    factor = gen.element(c("FALSE", "TRUE", NA_character_)) |>
      gen.c(of = n_records) |>
      gen.with(with_args(factor, levels = c("FALSE", "TRUE"))) |>
      gen.with(as.data.frame.vector),
    list = gen.list_element() |>
      gen.list(of = n_records) |>
      gen.with(as.data.frame.vector),
    matrix = gen.element(0:2) |>
      gen.and_then(\(n) {
        gen.choice(
          gen.element(c(FALSE, TRUE, NA)) |>
            gen.c(of = n*n_records) |>
            gen.with(as.logical),
          gen.element(c(-5:5, NA_integer_)) |>
            gen.c(of = n*n_records) |>
            gen.with(as.integer),
          gen.numeric() |>
            gen.c(of = n*n_records) |>
            gen.with(as.numeric),
          gen.element(c("FALSE", "TRUE", NA_character_)) |>
            gen.c(of = n*n_records) |>
            gen.with(as.character),
          gen.element(c("FALSE", "TRUE", NA_character_)) |>
            gen.c(of = n*n_records) |>
            gen.with(with_args(factor, levels = c("FALSE", "TRUE")))
        )
      }) |>
      gen.with(with_args(matrix, nrow = n_records)) |>
      gen.with(\(x) {
        dat <- data.frame(seq_len(n_records))[, FALSE, drop = FALSE]
        dat[[1]] <- x
        dat
      })
  )
  if (length(classes) == 0L)
    return(
      if (remove_dup_rows)
        gen.pure(data.frame(a = NA)[rep(1L, min(n_records, 1L)), FALSE, drop = FALSE])
      else
        gen.pure(data.frame(a = NA)[rep(1L, n_records), FALSE, drop = FALSE])
    )
  lapply(
    classes,
    \(cl) {
      # gen.sample only shrinks by reordering,
      # and gen.c incorrectly returns NULL when size = 0,
      # so we need to unlist "manually"
      as_fns[[cl]]
    }
  ) |>
    gen.with(
      with_args(as.data.frame, check.names = FALSE) %>>%
        with_args(setNames, nm = nms) %>>%
        (if (remove_dup_rows) df_unique else identity)
    ) |>
    gen.with(variant)
}


gen.list_element <- function() {
  gen.choice( # list where each element is NULL or one of the other types
    gen.pure(NULL),
    gen.element(c(FALSE, TRUE, NA)) |>
      gen.c(from = 0, to = 2),
    gen.element(c(-5:5, NA_integer_)) |>
      gen.c(from = 0, to = 2),
    gen.numeric() |>
      gen.c(from = 0, to = 2),
    gen.element(c("FALSE", "TRUE", NA_character_)) |>
      gen.c(from = 0, to = 2),
    gen.element(c("FALSE", "TRUE", NA_character_)) |>
      gen.c(from = 0, to = 2) |>
      gen.with(with_args(factor, levels = c("FALSE", "TRUE"))),
    gen.list_element() |>
      gen.list(from = 0, to = 2)
  )
}

gen.float_coincide <- function(x, digits) {
  x <- coarsen_if_float(x, digits)
  len <- length(x)
  given <- which(!is.na(x))
  plen <- length(given)
  mag <- ifelse(!is.na(x) & x == 0, 0, floor(log(abs(x[given]), 10)))
  if (len == 0 || is.na(digits))
    return(gen.sample_resampleable(x, of = len))
  gen.sample_resampleable(seq_len(len), of = len) |>
    gen.and_then(\(inds) {
      mag_inds <- match(inds, given)
      mag_inds <- mag_inds[!is.na(mag_inds)]
      list(
        gen.pure(x),
        gen.pure(inds),
        gen.unif(-0.5, 0.5) |>
          gen.c(of = length(mag_inds)) |>
          gen.with(with_args(`*`, 10^(mag[mag_inds] + 1 - digits)))
      )
    }) |>
    gen.with(uncurry(\(x, inds, y) {
      res <- x[inds]
      given <- which(!is.na(res))
      if (length(given) != length(y)) {
        stop(
          "x: ", toString(x),
          "\ninds: ", toString(inds),
          "\ngiven: ", toString(given),
          "\nmag_inds: ", toString(match(inds, which(!is.na(x)))),
          "\nmag: ", toString(mag),
          "\ndigits: ", digits,
          "\nexp: ", toString(mag[match(inds, which(!is.na(x)))] + 1 - digits),
          "\nmult: ", toString(10^(mag[match(inds, which(!is.na(x)))] + 1 - digits)),
          "\ny: ", toString(y)
        )
      }
      res[given] <- res[given] + y
      res
    }))
}

gen.numeric <- function(weights = c(0.8, 0.2))
  list(
    gen.choice(gen.unif(-5, 5), gen.pure(NA_real_), prob = weights),
    gen.element(-5:5)
  ) |>
  gen.and_then(uncurry(\(x, y) x*10^y))

gen_attr_name <- function(len) {
  gen.sample_resampleable(c(letters, LETTERS, "_", " ", "."), to = len) |>
    gen.and_then(\(chars) {
      if (all(chars == " ")) {
        gen.element(c(letters, "_", "."))
      }else{
        gen.pure(chars)
      }
    }) |>
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
    gen.sample(attrs, gen.element(0:max_detset_size)) |>
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
      gen.element(seq.int(from, to))
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
gen.relation_schema <- function(x, from, to, single_empty_key = FALSE) {
  gen.subsequence(x) |>
    gen.and_then(\(attrs) {
      list(gen.pure(attrs), gen.keys(attrs)) |>
        gen.list(from = from, to = to)
    }) |>
    gen.with(\(schemas) {
      if (single_empty_key) {
        # only one schema can have an empty key
        rels_with_empty_keys <- which(vapply(
          schemas,
          \(schema) any(lengths(schema[[2]]) == 0L),
          logical(1)
        ))
        if (length(rels_with_empty_keys) > 1L)
          schemas <- schemas[-rels_with_empty_keys[-1]]
      }

      nms <- make.names(
        vapply(schemas, \(rel) name_dataframe(rel[[2]][[1]]), character(1)),
        unique = TRUE
      )
      list(setNames(schemas, nms), x)
    }) |>
    gen.with(\(lst) {
      do.call(relation_schema_nocheck, lst)
    })
}
gen.relation_schema_empty_keys <- function(x, from, to, min_empty) {
  if (min_empty > from)
    stop("can't guarantee more empty keys than minimum schema count")
  gen.relation_schema(x, from, to) |>
    gen.and_then(\(rs) {
      gen.element(floor(max(0L, min_empty)):length(rs)) |>
        gen.and_then(\(n_empty) {
          if (n_empty == 0)
            return(gen.pure(rs))
          gen.sample(seq_along(rs), n_empty, replace = FALSE) |>
            gen.with(\(empty) {
              keys(rs)[empty] <- rep(list(list(character())), n_empty)
              rs
            })
        })
    })
}

# references are included to ensure attributes that reference each other have
# the same class
gen.attrs_class <- function(nm, references = list()) {
  groups <- seq_along(nm)
  for (rel in references) {
    child_attrs <- match(rel[[2]], nm)
    parent_attrs <- match(rel[[4]], nm)
    stopifnot(!anyNA(c(child_attrs, parent_attrs)))
    for (n in seq_along(child_attrs)) {
      grp <- groups[c(child_attrs[[n]], parent_attrs[[n]])]
      groups[is.element(groups, grp)] <- min(grp)
    }
  }
  gen.element(list(
    "logical",
    "integer",
    "numeric",
    "character",
    "factor"
  )) |>
    gen.list(of = length(unique(groups))) |>
    gen.with(\(group_classes) group_classes[match(groups, sort(unique(groups)))]) |>
    gen.with(with_args(setNames, nm = nm))
}

gen.relation <- function(
  x,
  from,
  to,
  rows_from = 0L,
  rows_to = 10L,
  single_empty_key = FALSE,
  variant = c("data.frame", "tibble")
) {
  list(
    gen.relation_schema(x, from, to, single_empty_key = single_empty_key),
    gen.element(variant)
  ) |>
    gen.and_then(uncurry(
      \(rs, var) gen.relation_from_schema(rs, rows_from, rows_to, var)
    ))
}

gen.relation_from_schema <- function(
  rs,
  rows_from = 0L,
  rows_to = 10L,
  digits = getOption("digits"),
  variant = c("data.frame", "tibble")
) {
  variant <- match.arg(variant)
  gen.pure(create(rs)) |>
    gen.and_then(\(empty_rel) {
      r_attrs <- attrs(empty_rel)
      r_ncols <- lengths(r_attrs)
      r_keys <- keys(empty_rel)
      lapply(
        setNames(seq_along(empty_rel), names(empty_rel)),
        \(n) {
          ks <- r_keys[[n]]
          gen.element(rows_from:rows_to) |>
            gen.and_then(with_args(
              gen.df_fixed_ranges,
              classes = rep("logical", r_ncols[[n]]),
              nms = r_attrs[[n]],
              remove_dup_rows = TRUE,
              digits = digits,
              variant = variant
            )) |>
            gen.with(\(df) list(
              df = remove_key_violations(df, ks),
              keys = ks
            ))
        }
      ) |>
        gen.with(with_args(relation_nocheck, attrs_order = attrs_order(empty_rel)))
    })
}

remove_key_violations <- function(df, keys) {
  Reduce(
    \(df, key) df[!df_duplicated(df[, key, drop = FALSE]), , drop  = FALSE],
    keys,
    init = df
  )
}

remove_insertion_key_violations <- function(df, relation) {
  Reduce(
    \(df, n) {
      recs <- records(relation)
      Reduce(
        \(df, key) {
          r_df <- recs[[n]]
          r_attrs <- names(r_df)
          remove <- if (length(key) == 0L) {
            negind <- if (nrow(r_df) == 0)
              TRUE
            else
              -seq_len(nrow(r_df))
            if (length(r_attrs) == 0L)
              rep(FALSE, nrow(df))
            else{
              single_adds <- lapply(
                seq_len(nrow(df)),
                \(n) df_rbind(r_df, df[n, r_attrs, drop = FALSE])
              )
              record_new <- vapply(
                single_adds,
                \(sa) {
                  nondups <- !duplicated(sa)
                  if (length(nondups) != nrow(r_df) + 1L)
                    stop(paste(print(1), print(relation[[n]]), print(df)))
                  res <- nondups[negind]
                  if (length(res) != 1)
                    stop(paste(print(2), print(relation[[n]]), print(df)))
                  res
                },
                logical(1)
              )
              record_new
            }
          }else{
            negind <- if (nrow(r_df) == 0)
              TRUE
            else
              -seq_len(nrow(r_df))
            comb <- df_rbind(r_df, df[, r_attrs, drop = FALSE])
            key_dups <- df_duplicated(comb[, key, drop = FALSE])[negind]
            single_adds <- lapply(
              seq_len(nrow(df)),
              \(n) df_rbind(r_df, df[n, r_attrs, drop = FALSE])
            )
            record_new <- vapply(
              single_adds,
              \(sa) {
                nondups <- !duplicated(sa)
                if (length(nondups) != nrow(r_df) + 1L)
                  stop(paste(print(1), print(relation[[n]]), print(df)))
                res <- nondups[negind]
                if (length(res) != 1)
                  stop(paste(print(2), print(relation[[n]]), print(df)))
                res
              },
              logical(1)
            )
            key_dups & record_new
          }
          df[!remove, , drop = FALSE]
        },
        keys(relation)[[n]],
        init = df
      )
    },
    seq_along(relation),
    init = df
  )
}

remove_violated_references <- function(references, relation) {
  recs <- records(relation)
  references[vapply(
    references,
    \(rel) {
      child <- recs[[rel[[1]]]][, rel[[2]], drop = FALSE]
      parent <- recs[[rel[[3]]]][, rel[[4]], drop = FALSE]
      identical(
        nrow(child),
        nrow(df_join(
          child,
          parent,
          by.x = rel[[2]],
          by.y = rel[[4]]
        ))
      )
    },
    logical(1)
  )]
}

gen.references_same_attrs <- function(rs, single_key_pairs) {
  gen.references_for_index_and_key <- function(rs, n, k) {
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
  gen.references_for_index <- function(rs, n) {
    ks <- keys(rs)[[n]]
    lapply(
      ks[lengths(ks) > 0L],
      gen.references_for_index_and_key,
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
  lapply(seq_along(rs), gen.references_for_index, rs = rs) |>
    gen.with(\(lst) if (length(lst) == 0L) list() else unlist(lst, recursive = FALSE))
}

gen.references_different_attrs <- function(rs, single_key_pairs) {
  gen.references_for_index_and_key <- function(rs, n, k) {
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
  gen.references_for_index <- function(rs, n) {
    ks <- keys(rs)[[n]]
    lapply(
      ks[lengths(ks) > 0L],
      gen.references_for_index_and_key,
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
  lapply(seq_along(rs), gen.references_for_index, rs = rs) |>
    gen.with(\(lst) if (length(lst) == 0L) list() else unlist(lst, recursive = FALSE))
}

gen.references <- function(rs, single_key_pairs) {
  gen.choice(
    gen.references_same_attrs(rs, single_key_pairs),
    gen.references_different_attrs(rs, single_key_pairs)
  )
}

gen.database_schema <- function(
  x,
  from,
  to,
  single_empty_key = FALSE,
  same_attr_name = FALSE,
  single_key_pairs = FALSE
) {
  gen.relation_schema(x, from, to, single_empty_key = single_empty_key) |>
    gen.and_then(\(rs) {
      list(
        gen.pure(rs),
        if (same_attr_name)
          gen.references_same_attrs(rs, single_key_pairs)
        else
          gen.references(rs, single_key_pairs))
    }) |>
    gen.with(\(lst) do.call(database_schema_nocheck, lst))
}
gen.database_schema_empty_keys <- function(
  x,
  from,
  to,
  min_empty,
  same_attr_name = FALSE,
  single_key_pairs = FALSE
) {
  gen.relation_schema_empty_keys(x, from, to, min_empty) |>
    gen.and_then(\(rs) {
      list(
        gen.pure(rs),
        if (same_attr_name)
          gen.references_same_attrs(rs, single_key_pairs)
        else
          gen.references(rs, single_key_pairs))
    }) |>
    gen.with(\(lst) do.call(database_schema_nocheck, lst))
}

gen.database <- function(
  x,
  from,
  to,
  single_empty_key = FALSE,
  same_attr_name = TRUE,
  single_key_pairs = TRUE,
  rows_from = 0L,
  rows_to = 10L,
  variant = c("data.frame", "tibble")
) {
  list(
    gen.database_schema(
      x,
      from,
      to,
      single_empty_key = single_empty_key,
      same_attr_name = same_attr_name,
      single_key_pairs = single_key_pairs
    ),
    gen.element(variant)
  ) |>
    gen.and_then(uncurry(\(ds, var) {
      gen.relation_from_schema(ds, rows_from, rows_to, var) |>
        gen.with(
          with_args(
            remove_reference_violations,
            references = references(ds)
          ) %>>%
            with_args(database_nocheck, references = references(ds))
        )
    }))
}

remove_reference_violations <- function(relation, references) {
  if (length(references) == 0L)
    return(relation)
  change <- TRUE
  recs <- records(relation)
  while (change) {
    change <- FALSE
    for (ref in references) {
      child_name <- ref[[1]]
      child <- recs[[child_name]][, ref[[2]], drop = FALSE]
      if (nrow(child) > 0L) {
        child_records <- df_records(child)
        parent_name <- ref[[3]]
        parent <- recs[[parent_name]][, ref[[4]], drop = FALSE]
        parent_keys <- keys(relation)[[parent_name]]
        stopifnot(is.element(list(ref[[4]]), parent_keys))
        parent_records <- df_records(parent)
        valid <- is.element(child_records, parent_records)
        recs[[child_name]] <- recs[[child_name]][valid, , drop = FALSE]
        if (!all(valid))
          change <- TRUE
      }
    }
  }
  records(relation) <- recs
  relation
}

remove_insertion_reference_violations <- function(df, database) {
  if (length(references(database)) == 0L)
    return(df)
  recs <- records(database)
  change <- TRUE
  while (change) {
    change <- FALSE
    for (ref in references(database)) {
      child_name <- ref[[1]]
      child <- df_rbind(
        recs[[child_name]][, ref[[2]], drop = FALSE],
        df[, ref[[2]], drop = FALSE]
      )
      if (nrow(child) > 0L) {
        parent_name <- ref[[3]]
        parent <- df_rbind(
          recs[[parent_name]][, ref[[4]], drop = FALSE],
          df[, ref[[4]], drop = FALSE]
        )
        valid <- vapply(
          seq_len(nrow(child)),
          \(n) nrow(df_join(
            child[n, , drop = FALSE],
            parent,
            by.x = ref[[2]],
            by.y = ref[[4]]
          )) > 0L,
          logical(1)
        )
        df <- df[valid[-seq_len(nrow(recs[[child_name]]))], , drop = FALSE]
        if (!all(valid))
          change <- TRUE
      }
    }
  }
  df
}

gen_fk_reduction_for_df <- function(df) {
  true_dbs <- normalise(discover(df))
  true_fks <- references(true_dbs)
  true_fk_key_switch <- lapply(
    true_fks,
    \(fk) {
      len <- length(fk[[2]])
      new_tbs <- vapply(
        keys(true_dbs),
        \(ks) match(len, lengths(ks)),
        integer(1)
      )
      valid_new_tbs <- new_tbs[
        !is.na(new_tbs) &
          names(new_tbs) != fk[[1]] &
          names(new_tbs) != fk[[3]]
      ]
      new_fks <- Map(
        \(new_key_index, new_parent) {
          new_fk <- c(
            fk[1:2],
            list(new_parent, keys(true_dbs)[[new_parent]][[new_key_index]])
          )
          stopifnot(length(new_fk) == 4)
          if (!is.element(list(new_fk[[4]]), keys(true_dbs)[[new_fk[[3]]]]))
            stop("argh")
          new_fk
        },
        valid_new_tbs,
        names(valid_new_tbs)
      ) |>
        Filter(f = \(fk) !is.element(list(fk), true_fks)) |>
        Filter(f = \(fk) {
          length(remove_violated_references(
            list(fk),
            decompose(df, true_dbs)
          )) == 0
        })
      new_fks
    }
  )
  if (all(lengths(true_fk_key_switch) == 0))
    return(gen.pure(list(df, NULL)))
  gen.element(which(lengths(true_fk_key_switch) > 0)) |>
    gen.and_then(\(index) list(
      gen.pure(df),
      gen.element(true_fk_key_switch[[index]]) |>
        gen.with(\(new_fk) {
          dbs <- true_dbs
          references(dbs)[[index]] <- new_fk
          dbs
        })
    ))
}

fac2char <- function(df) {
  facs <- vapply(df, is.factor, logical(1))
  df[, facs] <- lapply(df[, facs, drop = FALSE], as.character)
  df
}

# naively inserting data into a database can give reference errors,
# which sets of relations are legal to insert into?
minimal_legal_insertion_sets <- function(db, df) {
  refs <- references(db)
  ref_mat <- matrix(FALSE, nrow = length(db), ncol = length(db))
  dimnames(ref_mat) <- list(child = names(db), parent = names(db))
  for (ref in refs) {
    ref_mat[ref[[1]], ref[[3]]] <- TRUE
  }

  # 1. If df doesn't have all attributes for a relation, inserting does nothing:
  # it's a legal insertion set, that relation can't be inserted into to nake
  # insertion into a parent legal.
  have_attrs <- vapply(
    attrs(db),
    \(x) all(x %in% names(df)),
    logical(1)
  )
  # 2. Otherwise, if a relation already has the given data, it's a legal
  # insertion set.
  already_present <- rep(FALSE, length(db))
  already_present[have_attrs] <- vapply(
    records(db[have_attrs]),
    \(r) {
      # assumes df rows are already unique
      nrow(df_join(r, df[, names(r), drop = FALSE])) == nrow(df)
    },
    logical(1)
  )
  # take out rels with data already, since sets by themselves and can't affect
  # legality of children
  legal_sets <- as.list(c(
    rownames(ref_mat)[!have_attrs],
    rownames(ref_mat)[already_present]
  ))
  ref_mat <- ref_mat[!already_present, !already_present, drop = FALSE]

  # 3. Otherwise, if inserting into that relation would cause a key violation,
  # the relation can't be inserted into.
  violates_key <- rep(NA, length(db))
  violates_key[have_attrs][!already_present] <- vapply(
    names(db)[have_attrs][!already_present],
    \(nm) {
      nr <- df_rbind(records(db)[[nm]], df[, attrs(db)[[nm]], drop = FALSE])
      any(vapply(
        keys(db)[[nm]],
        \(key) as.logical(df_anyDuplicated(nr[, key, drop = FALSE])),
        logical(1)
      ))
    },
    logical(1)
  )

  # 4. Otherwise, check whether all the relation's parents have legal insertion
  # sets. If they do, the relation has a legal insertion set that includes
  # itself, plus the insertion sets of the parents that don't already contain
  # the data themselves.

  # First, we determine whether a relation can be inserted into when ignoring
  # foreign keys.
  legal <- (have_attrs & !violates_key)[!already_present]

  # Then, we find all the relations it refers to, directly and indirectly. This
  # includes itself.
  family_mat <- ref_mat
  old_val <- NA & family_mat
  while (!identical(old_val, family_mat)) {
    old_val <- family_mat
    family_mat <- family_mat | (family_mat %*% ref_mat)
  }
  diag(family_mat) <- TRUE

  # Check whether it depends on anything illegal.
  eventually_illegal <- apply(family_mat[, !legal, drop = FALSE], 1, any)

  # Keep dependency set otherwise.
  legal_sets <- c(
    legal_sets,
    apply(
      family_mat[!eventually_illegal, , drop = FALSE],
      1,
      \(lgs) rownames(ref_mat)[lgs],
      simplify = FALSE
    )
  )

  unique(legal_sets)
}

# error arising from x[[indices]], assuming there is one
single_subset_failure_type <- function(x, indices) {
  if (length(indices) > 1)
    "attempt to select more than one element in vectorIndex"
  else {
    if (
      length(indices) == 1 &&
      all(indices < 0) &&
      length(indices) + 1 < length(x)
    ) {
      if (is.logical(indices))
        "attempt to select more than one element in get1Index"
      else
        "attempt to select more than one element in integerOneIndex"
    }else{
      if (length(indices) == 1)
        "attempt to select less than one element in integerOneIndex"
      else
        "attempt to select less than one element in get1index"
    }
  }
}

# generating key / determinant set lists
gen.nonempty_list <- function(generator, to)
  gen.list(generator, from = 1, to = to)
gen.emptyable_list <- function(generator, to)
  gen.list(generator, from = 0, to = to)
gen.list_with_dups <- function(generator, n_unique)
  gen.nonempty_list(generator, n_unique) |>
  gen.and_then(\(lst) gen.sample(lst, ceiling(1.5*length(lst)), replace = TRUE))

# gen.sample with replace=TRUE, but allowing changing the sample
# when shrinking, not just re-ordering
gen.sample_resampleable <- function(x, from = 1, to = NULL, of = NULL) {
  if ((!missing(from) || !missing(to)) && !missing(of))
    stop("Specify `to` and `from`, or `of`")
  if (!missing(of)) {
    if (of == 0) {
      gen.pure(x[FALSE])
    }else{
      gen.element(x) |>
        gen.list(of = of) |>
        gen.and_then(function(xs) do.call(c, xs))
    }
  }else {
    gen.element(from:to) |>
      gen.and_then(\(of) {
        if (of == 0) {
          gen.pure(x[FALSE])
        }else{
          gen.element(x) |>
            gen.list(of = of) |>
            gen.and_then(function(xs) do.call(c, xs))
        }
      })
  }
}

rel2df <- function(rel, relations) {
  records(rel)[relations] <- lapply(records(rel)[relations], as.data.frame)
  rel
}

# functional utility functions for tests
`%>>%` <- function(fn1, fn2) function(...) fn2(fn1(...))
biapply <- function(fn1, fn2) function(x) list(fn1(x), fn2(x))
bi <- function(fn, fn1, fn2) {
  function(x) fn(fn1(x), fn2(x))
}
expect_bi <- function(logical_fn, fn1, fn2) {
  function(x) expect_true(logical_fn(fn1(x), fn2(x)))
}
expect_biequal <- function(fn1, fn2) function(x) expect_equal(fn1(x), fn2(x))
expect_biidentical <- function(fn1, fn2)
  function(...) expect_identical(fn1(...), fn2(...))
split_by <- function(fn, ...) function(x) split(x, fn(x), ...)
subset_by <- function(fn) function(x) x[fn(x)]
if_discard_else <- function(cond, fn)
  function(...) if (cond(...)) discard() else fn(...)
uncurry <- function(fn) function(x) do.call(fn, x)
with_args <- function(fn, ...) {
  lst <- list(...)
  function(...) do.call(fn, c(list(...), lst))
}
apply_both <- function(fn1, fn2) function(x) {fn1(x); fn2(x)}
dup <- function(x) list(x, x)
onLeft <- function(f) function(x) list(f(x[[1]]), x[[2]])
onRight <- function(f) function(x) list(x[[1]], f(x[[2]]))
fand <- function(f, g) function(...) f(...) & g(...)

concatenate_keeps_attribute_order <- function(...) {
  lst <- list(...)

  # expect c(...) silent
  res <- tryCatch(
    c(...),
    error = identity,
    warning = identity,
    message = identity
  )
  if (inherits(res, "condition"))
    return(fail("concatenation was not silent"))
  expect_true(all(mapply(
    identical,
    lapply(lst, attrs_order),
    lapply(lst, \(x) intersect(attrs_order(res), attrs_order(x)))
  )))
}

make.unique_after <- function(x, pre) {
  if (length(pre) == 0)
    return(x)
  stopifnot(!anyDuplicated(pre))
  make.unique(c(pre, x))[-seq_along(pre)]
}
with_timeout <- function(expr, timeout = 5) {
  R.utils::withTimeout(
    expr,
    timeout = timeout,
    onTimeout = "silent"
  )
}
