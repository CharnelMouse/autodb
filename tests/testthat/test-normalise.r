library(hedgehog)

describe("normalise", {
  it("gives valid schemas with same-attribute-names foreign key references", {
    # table_dum and table_dee
    empty_fds <- functional_dependency(list(), attrs_order = character())
    empty_schema <- normalise(empty_fds)
    is_valid_database_schema(empty_schema, same_attr_name = TRUE)

    forall(
      gen_flat_deps(7, 6, to = 6L),
      apply_both(
        normalise %>>% with_args(is_valid_database_schema, same_attr_name = TRUE),
        with_args(normalise, ensure_lossless = FALSE) %>>%
          with_args(is_valid_database_schema, same_attr_name = TRUE)
      )
    )
  })
  it("is the same as synthesise >> cross_reference", {
    forall(
      gen_flat_deps(7, 6, to = 6L),
      expect_biidentical(
        normalise,
        synthesise %>>% cross_reference
      )
    )
  })
})
