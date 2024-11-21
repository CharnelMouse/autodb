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
          nonempty_powerset(n, F, size) |> reduce_powerset(m),
          nonempty_powerset(m, F, size)
        )
      },
      curry = TRUE
    )
  })
})
