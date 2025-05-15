describe("generate_next_seeds", {
  it("generates all single attributes if min_deps and max_non_deps are empty", {
    forall(
      gen.int(10),
      \(n) {
        powerset <- nonempty_powerset(n, use_visited = FALSE)
        lhs_attr_nodes <- to_nodes(seq_len(n), powerset)
        expect_setequal(
          generate_next_seeds(integer(), integer(), lhs_attr_nodes, powerset, detset_limit = n),
          lhs_attr_nodes
        )
      }
    )
  })
})
