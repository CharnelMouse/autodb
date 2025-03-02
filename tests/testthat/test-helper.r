test_that("gen_df can give a table of exact height", {
  forall(
    gen.element(0:6) |>
      gen.and_then(\(n) list(
        gen.pure(n),
        gen_df(n, 7, minrow = n, remove_dup_rows = FALSE)
      )),
    onRight(nrow) %>>% (uncurry(expect_identical))
  )
})

test_that("gen_df can give a table of exact width", {
  forall(
    gen.element(0:7) |>
      gen.and_then(\(n) list(
        gen.pure(n),
        gen_df(6, n, mincol = n, remove_dup_rows = FALSE)
      )),
    onRight(ncol) %>>% (uncurry(expect_identical))
  )
})

test_that("gen_df can generate a tibble without name failure", {
  forall(
    gen.df_fixed_ranges("logical", "..1", 2, TRUE, "tibble"),
    expect_silent
  )
})

test_that("gen_flat_deps_fixed_names generates valid", {
  forall(gen_flat_deps_fixed_names(7, 20, to = 20L), is_valid_functional_dependency)
})

test_that("gen_flat_deps generates valid", {
  forall(gen_flat_deps(7, 20, to = 20L), is_valid_functional_dependency)
})

test_that("gen.relation_schema generates valid relation schemas", {
  forall(
    gen.element(c(FALSE, TRUE)) |>
      gen.and_then(\(sek) list(
        sek,
        gen.relation_schema(letters[1:6], 0, 8, single_empty_key = sek)
      )),
    \(sek, rs) is_valid_relation_schema(rs, single_empty_key = sek),
    curry = TRUE
  )
})

test_that("gen.relation_schema_empty_keys generates valid relation schemas, enough empty keys", {
  forall(
    gen.element(0:4) |>
      gen.and_then(\(from) {
        gen.element(0:from) |>
          gen.and_then(\(me) {
            list(
              gen.pure(from),
              gen.pure(me),
              gen.relation_schema_empty_keys(
                letters[1:6],
                from = from,
                to = 8,
                min_empty = me
              )
            )
          })
      }),
    \(from, me, rs) {
      is_valid_relation_schema(rs)
      expect_gte(
        sum(vapply(keys(rs), identical, logical(1), list(character()))),
        me
      )
    },
    curry = TRUE
  )
})

test_that("gen.database_schema generates valid database schemas", {
  forall(
    gen.element(c(FALSE, TRUE)) |>
      gen.list(of = 3) |>
      gen.and_then(uncurry(\(sek, san, skp) list(
        gen.pure(sek),
        gen.pure(san),
        gen.pure(skp),
        gen.database_schema(
          letters[1:6],
          0,
          8,
          single_empty_key = sek,
          same_attr_name = san,
          single_key_pairs = skp
        )
      ))),
    \(sek, san, skp, ds) is_valid_database_schema(
      ds,
      single_empty_key = sek,
      same_attr_name = san,
      single_key_pairs = skp
    ),
    curry = TRUE
  )
})

test_that("gen.database_schema_empty_keys generates valid relation schemas, enough empty keys", {
  forall(
    list(
      gen.element(c(FALSE, TRUE)),
      gen.element(c(FALSE, TRUE)),
      gen.element(0:4)
    ) |>
      gen.and_then(uncurry(\(san, skp, from) list(
        gen.pure(san),
        gen.pure(skp),
        gen.element(0:from) |>
          gen.and_then(\(me) {
            list(
              gen.pure(me),
              gen.database_schema_empty_keys(
                letters[1:6],
                from = from,
                to = 8,
                min_empty = me,
                same_attr_name = san,
                single_key_pairs = skp
              )
            )
          })
      ))) |>
      gen.with(uncurry(\(san, skp, lst) {
        c(list(san, skp), lst)
      })),
    \(san, skp, me, ds) {
      is_valid_database_schema(
        ds,
        same_attr_name = san,
        single_key_pairs = skp
      )
      expect_gte(
        sum(vapply(keys(ds), identical, logical(1), list(character()))),
        me
      )
    },
    curry = TRUE
  )
})

test_that("gen.relation generates valid relations", {
  forall(
    gen.element(c(FALSE, TRUE)) |>
      gen.and_then(\(sek) list(
        gen.pure(sek),
        gen.relation(letters[1:4], 6, 7, single_empty_key = sek)
      )),
    \(sek, r) is_valid_relation(r, single_empty_key = sek),
    curry = TRUE
  )
})

test_that("gen.database generates valid databases", {
  forall(
    gen.element(c(FALSE, TRUE)) |>
      gen.list(of = 3) |>
      gen.and_then(uncurry(\(sek, san, skp) list(
        gen.pure(sek),
        gen.pure(san),
        gen.pure(skp),
        gen.database(
          letters[1:7],
          from = 0,
          to = 6,
          single_empty_key = sek,
          same_attr_name = san,
          single_key_pairs = skp
        )
      ))),
    \(sek, san, skp, ds) is_valid_database(
      ds,
      single_empty_key = sek,
      same_attr_name = san,
      single_key_pairs = skp
    ),
    curry = TRUE
  )
})

test_that("remove_insertion_key_violations removes violations", {
  forall(
    gen.relation(letters[1:4], 0, 6) |>
      gen.and_then(\(r) {
        list(
          gen.pure(r),
          gen.int(10) |>
            gen.and_then(with_args(
              gen.df_fixed_ranges,
              classes = rep("logical", length(attrs_order(r))),
              nms = attrs_order(r),
              remove_dup_rows = TRUE
            ))
        )
      }) |>
      gen.with(uncurry(\(r, df) {
        list(
          r,
          unclass(r),
          df,
          remove_insertion_key_violations(df, r)
        )
      })),
    \(r, r_unclassed, df, df_trimmed) {
      recs <- records(r)
      ks <- keys(r)
      expect_true(all(vapply(
        seq_along(r),
        \(n) {
          r_df <- recs[[n]]
          new_rel <- df_unique(rbind(r_df, df_trimmed[, names(r_df), drop = FALSE]))
          all(vapply(
            ks[[n]],
            \(key) {!df_anyDuplicated(new_rel[, key, drop = FALSE])},
            logical(1)
          ))
        },
        logical(1)
      )))
    },
    curry = TRUE
  )
})
