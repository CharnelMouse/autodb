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
      gen.relation(6L, 6L),
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
      gen.database(6L, 6L),
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
