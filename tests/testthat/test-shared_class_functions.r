library(hedgehog)

describe("create", {
  it("is commutative with adding foreign key constraints", {
    # need the same for create_insert and create %>>% insert once generating data
    forall(
      gen.relation_schema(letters[1:6], 0, 10) |>
        gen.and_then(\(rs) {
          list(
            gen.pure(rs),
            gen.relationships(rs)
          )
        }),
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
