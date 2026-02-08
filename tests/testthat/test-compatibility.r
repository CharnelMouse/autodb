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
        expect_biidentical(df_duplicated, duplicated %>>% as.logical)
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
  it("can rbind vectors with matrices (converted to lists if different ncols, or vectors if ncol = 1)", {
    x <- data.frame(a = 1:3, b = 1:3)
    y <- data.frame(a = 4:6)
    y$b <- matrix(4:9, ncol = 2)
    z <- x
    z$b <- as.matrix(x$b)

    res <- data.frame(a = 1:6)
    res$b <- list(1L, 2L, 3L, c(4L, 7L), c(5L, 8L), c(6L, 9L))
    expect_identical(df_rbind(x, y), res)

    res2 <- data.frame(a = rep(4:6, 2))
    res2$b <- rbind(y$b, y$b)
    expect_identical(df_rbind(y, y), res2)

    expect_identical(df_rbind(x, z), rbind(x, x))
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
