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
  it("does nothing when inserting nothing", {
    forall(
      gen.relation(letters[1:4], 0L, 6L),
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
      gen.database(letters[1:6], 0L, 6L),
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
  })
  it("returns an error when inserting key violations (e.g. already-present data)", {
    forall(
      gen_df(6, 7, minrow = 1, remove_dup_rows = TRUE) |>
        gen.with(\(df) {
          list(
            x = discover(df, 1) |>
              synthesise() |>
              cross_reference(ensure_lossless = TRUE) |>
              subschemas() |>
              create_insert(df = df),
            vals = df
          )
        }),
      \(x, vals) {
        expect_error(
          insert(x, vals),
          regexp = "^insertion violates key constraints in (1 relation|(\\d)+ relations): [[:alnum:]\\._]+(, [[:alnum:]\\._]+)*$"
        )
      },
      curry = TRUE
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
    forall(
      list(
        gen.relation(letters[1:4], 0, 6),
        gen.element(c(FALSE, TRUE))
      ) |>
        gen.and_then(uncurry(\(r, skp) {
          list(
            gen.pure(r),
            gen.int(10) |>
              gen.and_then(with_args(
                gen.df_fixed_ranges,
                classes = rep("logical", length(attrs_order(r))),
                nms = attrs_order(r),
                remove_dup_rows = TRUE
              )) |>
              gen.with(with_args(remove_insertion_key_violations, relation = r)),
            gen.relationships(r, skp)
          )
        })),
      \(r, df, rels) {
        expect_biidentical(
          with_args(database, relationships = rels) %>>%
            with_args(insert, vals = df),
          with_args(insert, vals = df) %>>%
            with_args(database, relationships = rels)
        )(r)
      },
      curry = TRUE
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
