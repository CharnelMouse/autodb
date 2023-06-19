library(hedgehog)

describe("normalise", {
  it("gives valid schemas", {
    # table_dum and table_dee
    empty_fds <- functional_dependency(list(), attrs = character())
    empty_schema <- normalise(empty_fds)
    is_valid_database_schema(empty_schema)

    forall(
      gen_flat_deps(7, 6),
      apply_both(
        normalise %>>% is_valid_database_schema,
        with_args(normalise, ensure_lossless = FALSE) %>>%
          is_valid_database_schema
      )
    )
  })
  it("is the same as synthesise >> cross_reference", {
    forall(
      gen_flat_deps(7, 6),
      expect_biidentical(
        normalise,
        synthesise %>>% cross_reference
      )
    )
  })
})
