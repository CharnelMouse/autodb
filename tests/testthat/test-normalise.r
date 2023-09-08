library(hedgehog)

describe("normalise", {
  it("gives valid schemas with singly-linked-pairs, same-attributes, reference pairs", {
    # table_dum and table_dee
    empty_fds <- functional_dependency(list(), attrs_order = character())
    empty_schema <- normalise(empty_fds)
    is_valid_database_schema(
      empty_schema,
      same_attr_name = TRUE,
      single_key_pairs = TRUE
    )

    forall(
      list(
        gen_flat_deps(7, 20, to = 20L),
        gen.element(c(FALSE, TRUE)),
        gen.element(c(FALSE, TRUE))
      ),
      \(fds, el, ra) {
        normalise(fds, ensure_lossless = el, remove_avoidable = ra) |>
          is_valid_database_schema(
            same_attr_name = TRUE,
            single_key_pairs = TRUE
          )
      },
      curry = TRUE
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
