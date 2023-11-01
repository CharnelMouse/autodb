library(hedgehog)

describe("create", {
  it("creates a valid structure", {
    forall(
      gen.relation_schema(letters[1:6], 0, 10),
      create %>>% is_valid_relation
    )
    forall(
      gen.database_schema(letters[1:6], 0, 10),
      create %>>% is_valid_database
    )
  })
  it("is commutative with adding foreign key constraints", {
    # need the same for create_insert and create %>>% insert once generating data
    forall(
      list(
        gen.relation_schema(letters[1:6], 0, 10),
        gen.element(c(FALSE, TRUE))
      ) |>
        gen.and_then(uncurry(\(rs, skp) {
          list(
            gen.pure(rs),
            gen.relationships(rs, skp)
          )
        })),
      \(rs, fks) {
        expect_biidentical(
          with_args(database_schema, relationships = fks) %>>% create,
          create %>>% with_args(database, relationships = fks)
        )(rs)
      },
      curry = TRUE
    )
  })
})

describe("insert", {
  it("does nothing when inserting nothing into nonempty relations, replaces into empty", {
    forall(
      gen.relation(letters[1:4], 0L, 6L, rows_from = 1L),
      expect_biidentical(
        identity,
        \(r) insert(
          r,
          data.frame(setNames(
            lapply(attrs_order(r), \(x) logical()),
            attrs_order(r)
          ))
        )
      )
    )
    forall(
      gen.relation_schema(letters[1:4], 0L, 6L) |>
        gen.and_then(\(schema) {
          list(
            gen.pure(create(schema)),
            gen.attrs_class(attrs_order(schema)) |>
              gen.and_then(\(classes) {
                gen.df_fixed_ranges(
                  classes,
                  attrs_order(schema),
                  0L,
                  FALSE
                )
              })
          )
        }),
      \(rel, df) expect_identical(
        relation(
          lapply(
            rel,
            \(r) {
              r$df <- df[, names(r$df), drop = FALSE]
              r
            }
          ),
          attrs_order(rel)
        ),
        insert(rel, df)
      ),
      curry = TRUE
    )
    forall(
      gen.database(letters[1:6], 0L, 6L, rows_from = 1L),
      expect_biidentical(
        identity,
        \(db) insert(
          db,
          data.frame(setNames(
            lapply(attrs_order(db), \(x) logical()),
            attrs_order(db)
          ))
        )
      )
    )
    forall(
      gen.database_schema(letters[1:4], 0L, 6L) |>
        gen.and_then(\(schema) {
          list(
            gen.pure(create(schema)),
            gen.attrs_class(attrs_order(schema)) |>
              gen.and_then(\(classes) {
                gen.df_fixed_ranges(
                  classes,
                  attrs_order(schema),
                  0L,
                  FALSE
                )
              })
          )
        }),
      \(db, df) expect_identical(
        database(
          relation(
            lapply(
              db,
              \(r) {
                r$df <- df[, names(r$df), drop = FALSE]
                r
              }
            ),
            attrs_order(db)
          ),
          relationships(db)
        ),
        insert(db, df)
      ),
      curry = TRUE
    )
  })
  it("returns an error when inserting key violations (i.e. same key, different record)", {
    df <- data.frame(a = 1:3, b = c(1:2, 1L))
    expect_error(
      insert(
        decompose(df, normalise(discover(df, 1))),
        data.frame(a = 1:2, b = 2:1)
      ),
      "^insertion violates key constraints in 1 relation: a$"
    )
  })
  it("returns a valid object when given data that can be legally inserted", {
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
              )) |>
              gen.with(with_args(remove_insertion_key_violations, relation = r))
          )
        }),
      insert %>>% is_valid_relation,
      curry = TRUE
    )
    forall(
      # same_attr_name = TRUE very low high chance of FK violations
      # to be removed, but = FALSE is invalid for common table insertion
      gen.database(letters[1:4], 0, 6, same_attr_name = FALSE) |>
        gen.and_then(\(d) {
          list(
            gen.pure(d),
            gen.int(10) |>
              gen.and_then(with_args(
                gen.df_fixed_ranges,
                classes = rep("logical", length(attrs_order(d))),
                nms = attrs_order(d),
                remove_dup_rows = TRUE
              )) |>
              gen.with(with_args(remove_insertion_key_violations, relation = d)) |>
              gen.with(with_args(remove_insertion_relationship_violations, database = d))
          )
        }),
      insert %>>% is_valid_database,
      curry = TRUE
    )
  })
  it("is commutative with adding foreign key constraints", {
    gen.ex_from_table <- list(
      # mincol to give good chance of non-zero count for relationships
      gen_df(6, 7, minrow = 2, mincol = 3, remove_dup_rows = FALSE),
      gen.element(c(FALSE, TRUE))
    ) |>
      gen.with(uncurry(\(df, skp) {
        if (nrow(df) < 2)
          stop(print(df))
        df1 <- df[seq_len(floor(nrow(df)/2)), , drop = FALSE]
        db_schema <- normalise(discover(df, 1))
        rel_schema <- subschemas(db_schema)
        df2 <- df[-seq_len(floor(nrow(df)/2)), , drop = FALSE]
        stopifnot(
          nrow(df) >= 2,
          nrow(df1) >= 1,
          nrow(df2) >= 1
        )
        relats <- relationships(db_schema)
        rel <- create(rel_schema) |> insert(df1)
        list(rel, df1, df2, relats)
      }))
    gen.ex <- list(
      gen_df(6, 7, minrow = 2),
      gen.relation(letters[1:4], 0, 6, rows_from = 1L, rows_to = 1L),
      gen.element(c(FALSE, TRUE))
    ) |>
      gen.and_then(uncurry(\(r, skp) {
        list(
          gen.pure(r),
          gen.sample(40:50, 1) |>
            gen.and_then(with_args(
              gen.df_fixed_ranges,
              classes = rep("logical", length(attrs_order(r))),
              nms = attrs_order(r),
              remove_dup_rows = TRUE
            )) |>
            gen.with(with_args(remove_insertion_key_violations, relation = r)),
          gen.relationships(r, skp) |>
            gen.with(with_args(remove_violated_relationships, relation = r))
        ) |>
          gen.with(uncurry(\(r, df, rels) {
            changed <- TRUE
            n <- 0L
            while (changed) {
              new_df <- remove_insertion_relationship_violations(
                df,
                database(r, rels)
              )
              new_rels <- remove_violated_relationships(
                rels,
                insert(r, new_df)
              )
              changed <- !identical(new_df, df) || !identical(new_rels, rels)
              df <- new_df
              rels <- new_rels
            }
            list(r, df, rels)
          }))
      }))
    expect_both_valid_db_then <- function(fn) {
      function(x, y) {
        is_valid_database(x)
        is_valid_database(y)
        fn(x, y)
      }
    }
    forall(
      gen.ex_from_table,
      \(r, old_df, df, rels) {
        if (nrow(df) == 0L || length(rels) == 0L)
          discard()
        (
          biapply(
            with_args(database, relationships = rels) %>>%
              with_args(insert, vals = df),
            with_args(insert, vals = df) %>>%
              with_args(database, relationships = rels)
          ) %>>%
            (uncurry(expect_both_valid_db_then(expect_identical)))
        )(r)
      },
      curry = TRUE,
      tests = 300,
      discard.limit = 210
    )
  })
})

describe("subrelations", {
  it("returns a valid relation for database", {
    forall(
      gen.database(letters[1:6], 0, 6),
      subrelations %>>% is_valid_relation
    )
  })
})

describe("subrelations", {
  it("returns a valid relation_schema for database_schema", {
    forall(
      gen.element(c(FALSE, TRUE)) |>
        gen.and_then(with_args(
          gen.database_schema,
          x = letters[1:6],
          from = 0,
          to = 6
        )),
      subschemas %>>% is_valid_relation_schema
    )
  })
})
