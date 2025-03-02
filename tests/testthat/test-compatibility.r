describe("df_duplicated", {
  it("works for zero-column data frames", {
    df <- data.frame(a = 1:5)[, -1, drop = FALSE]
    expect_identical(df_duplicated(df), rep(c(FALSE, TRUE), c(1, 4)))
    df2 <- df[FALSE, , drop = FALSE]
    expect_identical(df_duplicated(df2), logical())
  })
  it("is the same as duplicated for data frames with at least one column", {
    forall(
      gen_df(6, 7, mincol = 1),
      expect_biidentical(df_duplicated, duplicated)
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
  it("is the same as unique for data frames with at least one column", {
    forall(
      gen_df(6, 7, mincol = 1),
      expect_biidentical(df_unique, unique)
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
  it("is the same as duplicated for data frames with at least one column", {
    forall(
      gen_df(6, 7, mincol = 1),
      expect_biidentical(df_anyDuplicated, anyDuplicated)
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
