describe("df_duplicated", {
  it("works for zero-column data frames", {
    df <- data.frame(a = 1:5)[, -1, drop = FALSE]
    expect_identical(df_duplicated(df), rep(c(FALSE, TRUE), c(1, 4)))
    df2 <- df[FALSE, , drop = FALSE]
    expect_identical(df_duplicated(df2), logical())
  })
  it("is the same as duplicated for data frames with at least one atomic-only column", {
    forall(
      gen_df(6, 7, mincol = 1, atomic = TRUE),
      expect_biidentical(df_duplicated, duplicated %>>% as.logical)
    )
  })
  it("checks for duplicate rows for lone zero-or-one-column matrix columns", {
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
  it("is the same as unique for data frames with at least one atomic-only column", {
    forall(
      gen_df(6, 7, mincol = 1, atomic = TRUE),
      expect_biidentical(df_unique, unique)
    )
  })
  it("checks for duplicate rows for lone zero-or-one-column matrix columns", {
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
  it("is the same as duplicated for data frames with at least one atomic-only column", {
    forall(
      gen_df(6, 7, mincol = 1, atomic = TRUE),
      expect_biidentical(df_anyDuplicated, anyDuplicated)
    )
  })
  it("checks for duplicate rows for lone zero-or-one-column matrix columns", {
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
  it("is the same as rbind >> as.data.frame for data frames with at least one row and column, if the latter works", {
    # minrow = 1 since rbind() ignores empty tables
    forall(
      gen.int(5) |>
        gen.and_then(with_args(gen_attr_names, len = 9)) |>
        gen.and_then(\(nms) {
          gen_df(4, length(nms), minrow = 1, mincol = length(nms), atomic = TRUE) |>
            gen.with(with_args(setNames, nm = nms)) |>
            gen.list(from = 0, to = 3)
        }),
      expect_biidentical(
        uncurry(df_rbind),
        uncurry(rbind %>>% as.data.frame)
      )
    )
  })
  it("doesn't rbind vectors with matrices if the latter have a single column", {
    x <- data.frame(a = 1:3, b = 1:3)
    y <- data.frame(a = 4:6)
    y$b <- matrix(4:9, ncol = 2)
    z <- x
    z$b <- as.matrix(x$b)

    expect_error(df_rbind(x, y))
    expect_error(df_rbind(x, z))

    res <- data.frame(a = rep(4:6, 2))
    res$b <- rbind(y$b, y$b)
    expect_identical(df_rbind(y, y), res)
  })
  it("can rbind matrices with each other if they have the same column count", {
    x <- data.frame(a = 1:3)
    x$b <- matrix(4:9, ncol = 2)
    res <- rbind(x, x)
    res$b <- rbind(x$b, x$b)
    expect_identical(df_rbind(x, x), res)

    y <- data.frame(a = 1:3)
    y$b <- matrix(4:6, ncol = 1)
    expect_error(df_rbind(x, y))
  })
  it("preserves row count", {
    forall(
      list(
        gen_df(4, 6),
        gen.int(3)
      ) |>
        gen.with(uncurry(\(x, n) rep(list(x), n))),
      expect_biidentical(
          with_args(do.call, what = df_rbind) %>>% nrow,
          with_args(vapply, nrow, integer(1)) %>>% sum
      )
    )
  })
})

describe("df_records", {
  it("returns as many records as are in the table", {
    forall(
      gen_df(4, 5),
      expect_biidentical(
        df_records %>>% length,
        nrow
      )
    )
  })
})
