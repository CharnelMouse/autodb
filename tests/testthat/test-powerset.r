library(hedgehog)

describe("nonempty_powerset", {
  it("maps bitsets to their integer representation if max_size = cardinality", {
    forall(
      gen.element(0:10),
      \(n) {
        p <- nonempty_powerset(n, use_visited = FALSE)
        expect_identical(
          vapply(
            p$bits,
            \(bits) as.integer(sum(bits*2^(seq_len(n) - 1))),
            integer(1)
          ),
          seq_len(2^n - 1)
        )
      }
    )
  })
  it("has bitset_index equal to seq_len(2^cardinality-1) if max_size = cardinality", {
    forall(
      gen.element(0:10),
      expect_biidentical(
        with_args(nonempty_powerset, use_visited = FALSE) %>>%
          with_args(getElement, name = "bitset_index"),
        (\(n) 2^n - 1) %>>%
          seq_len
      )
    )
  })
  it("maps full-set node index to limited index using bitset_index", {
    forall(
      gen.element(0:10) |>
        gen.c(of = 2) |>
        gen.with(sort %>>% as.list),
      \(n, m) {
        p <- nonempty_powerset(n, use_visited = FALSE, max_size = m)
        full <- nonempty_powerset(n, use_visited = FALSE)
        full_bit_matches <- match(full$bits, p$bits)
        expect_identical(full_bit_matches, p$bitset_index)
      },
      curry = TRUE
    )
  })
  it("initialises visited values to FALSE if used", {
    forall(
      gen.element(0:10),
      expect_biidentical(
        with_args(nonempty_powerset, use_visited = TRUE) %>>%
          with_args(getElement, name = "visited"),
        with_args(rep_len, x = FALSE)
      )
    )
  })
})

describe("reduce_powerset", {
  it("is like constructing with new cardinality, except for bitset mappings", {
    forall(
      gen.element(0:10) |>
        gen.and_then(\(n) list(
          gen.pure(n),
          gen.element(0:n),
          gen.element(0:n)
        )),
      \(n, size, m) {
        expect_identical(
          nonempty_powerset(n, F, size) |>
            reduce_powerset(m) |>
            (\(x) x[setdiff(names(x), "bitset_index")])(),
          nonempty_powerset(m, F, size) |>
            (\(x) x[setdiff(names(x), "bitset_index")])()
        )
      },
      curry = TRUE
    )
  })
})
