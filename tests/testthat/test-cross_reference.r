describe("cross_reference", {
  it("returns relations", {
    df <- data.frame(a = integer(), b = integer(), c = integer())
    deps <- list(
      dependencies = list(
        list("a", "b"),
        list("a", "c"),
        list("b", "c")
      ),
      attrs = c("a", "b", "c")
    )
    norm_deps <- normalise(deps)
    tables <- decompose(df, norm_deps)
    es <- cross_reference(tables)
    expected_relations <- list(
      c("a", "b", "b", "b")
    )
    expect_identical(es$relationships, expected_relations)
  })
})
