describe("cross_reference", {
  it("returns relationships", {
    df <- data.frame(a = integer(), b = integer(), c = integer())
    norm_deps <- list(
      attrs = list(
        c("a", "b"),
        c("b", "c")
      ),
      keys = list(
        list("a"),
        list("b")
      )
    )
    es <- cross_reference(norm_deps)
    expected_parents = list(2L, integer())
    expected_relations <- list(
      list(c(1L, 2L), "b")
    )
    expect_identical(es$parents, expected_parents)
    expect_identical(es$relationships, expected_relations)
  })
})
