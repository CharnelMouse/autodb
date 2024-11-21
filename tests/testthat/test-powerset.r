describe("powerset", {
  it("reducing to n elements is the same as constructing with that many", {
    forall(
      gen.element(0:10) |>
        gen.and_then(\(n) list(
          gen.pure(n),
          gen.element(0:n),
          gen.element(0:n)
        )),
      \(n, size, m) {
        expect_identical(
          powerset_nodes(n, F, size) |> reduce_powerset(m),
          powerset_nodes(m, F, size)
        )
      },
      curry = TRUE
    )
  })
})
