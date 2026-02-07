describe("df_duplicated", {
  it("works for zero-column data frames", {
    df <- data.frame(a = 1:5)[, -1, drop = FALSE]
    expect_identical(df_duplicated(df), rep(c(FALSE, TRUE), c(1, 4)))
    df2 <- df[FALSE, , drop = FALSE]
    expect_identical(df_duplicated(df2), logical())
  })
  it("is the same as duplicated for data frames with at least one column...", {
    forall(
      gen_df(6, 7, mincol = 1),
      if_discard_else(
        \(x) any(vapply(x, \(y) inherits(y, "array") && ncol(y) <= 1, logical(1))),
        expect_biidentical(df_duplicated, duplicated)
      )
    )
  })
  it("except for zero-or-one-column matrix columns, then it checks for duplicate rows", {
    df <- data.frame(a = 1:6)[, FALSE, drop = FALSE]
    df$a <- matrix(integer(), nrow = 6, ncol = 0)
    expect_identical(df_duplicated(df), rep(c(FALSE, TRUE), c(1, 5)))

    df2 <- data.frame(a = 1:6)[, FALSE, drop = FALSE]
    df2$a <- matrix(1:3, nrow = 6, ncol = 1)
    expect_identical(df_duplicated(df2), rep(c(FALSE, TRUE), c(3, 3)))
  })
  it("is invariant to calling lookup_table first", {
    forall(
      gen_df(6, 7),
      expect_biidentical(
        df_duplicated,
        lookup_table %>>% df_duplicated
      )
    )
  })
})

describe("df_unique", {
  it("returns the first row, if any, of zero-column data frames", {
    df <- data.frame(a = 1:5)[, -1, drop = FALSE]
    expect_identical(df_unique(df), df[1, , drop = FALSE])
    df2 <- df[FALSE, , drop = FALSE]
    expect_identical(df_unique(df2), df2)
  })
  it("is the same as unique for data frames with at least one column...", {
    forall(
      gen_df(6, 7, mincol = 1),
      if_discard_else(
        \(x) any(vapply(x, \(y) inherits(y, "array") && ncol(y) <= 1, logical(1))),
        expect_biidentical(df_unique, unique)
      )
    )
  })
  it("except for zero-or-one-column matrix columns, then it checks for duplicate rows", {
    df <- data.frame(a = 1:6)[, FALSE, drop = FALSE]
    df$a <- matrix(integer(), nrow = 6, ncol = 0)
    expect_identical(df_unique(df), df[1, , drop = FALSE])

    df2 <- data.frame(a = 1:6)[, FALSE, drop = FALSE]
    df2$a <- matrix(1:3, nrow = 6, ncol = 1)
    expect_identical(df_unique(df2), df2[1:3, , drop = FALSE])
  })
  it("gives result with lookup table invariant to calling lookup_table first", {
    forall(
      gen_df(6, 7),
      expect_biidentical(
        df_unique %>>% lookup_table,
        lookup_table %>>% df_unique %>>% lookup_table
      )
    )
  })
})

describe("df_anyDuplicated", {
  it("works for zero-column data frames", {
    df <- data.frame(a = 1:5)[, -1, drop = FALSE]
    expect_identical(df_anyDuplicated(df), 2L)
    df2 <- df[1, , drop = FALSE]
    expect_identical(df_anyDuplicated(df2), 0L)
    df3 <- df[FALSE, , drop = FALSE]
    expect_identical(df_anyDuplicated(df3), 0L)
  })
  it("is the same as duplicated for data frames with at least one column...", {
    forall(
      gen_df(6, 7, mincol = 1),
      if_discard_else(
        \(x) any(vapply(x, \(y) inherits(y, "array") && ncol(y) <= 1, logical(1))),
        expect_biidentical(df_anyDuplicated, anyDuplicated)
      )
    )
  })
  it("except for zero-or-one-column matrix columns, then it checks for duplicate rows", {
    df <- data.frame(a = 1:6)[, FALSE, drop = FALSE]
    df$a <- matrix(integer(), nrow = 6, ncol = 0)
    expect_identical(df_anyDuplicated(df), 2L)

    df2 <- data.frame(a = 1:6)[, FALSE, drop = FALSE]
    df2$a <- matrix(1:3, nrow = 6, ncol = 1)
    expect_identical(df_anyDuplicated(df2), 4L)
  })
  it("is invariant to calling lookup_table first", {
    forall(
      gen_df(6, 7),
      expect_biidentical(
        df_anyDuplicated,
        lookup_table %>>% df_anyDuplicated
      )
    )
  })
})

describe("df_rbind", {
  it("works for data frames with no columns", {
    df <- data.frame(a = 1:5)[, -1, drop = FALSE]
    df2 <- df[1:3, , drop = FALSE]
    expect_identical(
      df_rbind(df, df2),
      data.frame(a = 1:8)[, -1, drop = FALSE]
    )
  })
  it("is the same as rbind >> as.data.frame for data frames with at least one column", {
    forall(
      gen.int(7) |>
        gen.and_then(with_args(gen_attr_names, len = 9)) |>
        gen.and_then(\(nms) {
          gen_df(6, length(nms), mincol = length(nms)) |>
            gen.with(with_args(setNames, nm = nms)) |>
            gen.list(of = 2)
        }),
      expect_biidentical(
        uncurry(df_rbind),
        uncurry(rbind %>>% as.data.frame)
      )
    )
  })
})
