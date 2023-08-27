library(hedgehog)

describe("create", {
  it("equates database_schema %>>% create and create %>>% database", {
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
