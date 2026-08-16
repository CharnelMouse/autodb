describe("df_join", {
  it("returns a table with unique rows if it joins it with itself (maybe re-ordered)", {
    forall(
      gen_df(4, 6, remove_dup_rows = TRUE),
      expect_bi(
        with_args(df_equiv, digits = NA),
        identity,
        dup %>>% uncurry(df_join)
      )
    )
  })
  it("merges tables with matching names/classes and 1D non-list columns in the same way as merge()", {
    forall(
      gen.element(0:5) |>
        gen.and_then(\(n) {
          list(
            gen.element(c("logical", "integer", "numeric", "character", "factor")) |>
              gen.c(of = n),
            gen_attr_names(n, 9)
          )
        }) |>
        gen.and_then(uncurry(\(classes, nms) {
          list(
            classes = gen.pure(classes),
            nms = gen.pure(nms),
            n_records = gen.element(0:6),
            remove_dup_rows = FALSE
          ) |>
            gen.and_then(uncurry(gen.df_fixed_ranges)) |>
            gen.list(of = 2)
        })) |>
        gen.with(\(x) {
          list(
            dfs = x,
            classes = lapply(x, tuple_classes)
          )
        }),
      \(dfs, classes) {
        x <- dfs[[1]]
        y <- dfs[[2]]

        msg <- character()

        join1 <- df_join(x, y)
        merge1 <- merge(x, y, sort = FALSE)
        if (!df_equiv(
          join1,
          merge1,
          digits = NA
        ))
          msg <- c(
            msg,
            "df_join/merge differ for all = FALSE",
            paste(
              "df_join:",
              toString(vapply(df_records(join1), tuple_string, character(1)))
            ),
            paste(
              "merge  :",
              toString(vapply(df_records(merge1), tuple_string, character(1)))
            )
          )

        join2 <- df_join(x, y, all = TRUE)
        merge2 <- merge(x, y, sort = FALSE, all = TRUE)
        if (!df_equiv(
          join2,
          merge2,
          digits = NA
        ))
          msg <- c(
            msg,
            "df_join/merge differ for all = TRUE:",
            paste(
              "df_join:",
              toString(vapply(df_records(join2), tuple_string, character(1)))
            ),
            paste(
              "merge  :",
              toString(vapply(df_records(merge2), tuple_string, character(1)))
            )
          )

        if (length(msg) == 0)
          succeed()
        else
          fail(paste(msg, collapse = "\n"))
      },
      curry = TRUE,
      tests = 100*getOption("hedgehog.tests", 100)
    )
  })
  it("merges correctly by compound keys containing matrix columns", {
    df <- data.frame(1:3)[, FALSE, drop = FALSE]
    df$a <- matrix(c(1:6, 1:3), nrow = 3, byrow = TRUE)

    # merging compound keys with matrix columns works
    df2 <- cbind(df, b = c(1L, 2L, 1L))
    expect_identical(nrow(df_join(df2, df2, by = c("a", "b"))), 5L)
    expect_identical(nrow(df_join(df2, df2, by = c("b", "a"))), 5L)
    expect_identical(nrow(df_join(df2, df2, by = c("a", "b"), all = TRUE)), 5L)

    # merging compound keys with only matrix columns works
    df3 <- df2
    df3$c <- df$a
    expect_identical(nrow(df_join(df3, df3, by = c("a", "c"))), 5L)
    expect_identical(nrow(df_join(df3, df3, by = c("c", "a"))), 5L)
    expect_identical(nrow(df_join(df3, df3, by = c("a", "c"), all = TRUE)), 5L)
  })
  it("gives no error if given a simple list key (since doesn't sort)", {
    x <- data.frame(a = 1:2)[, FALSE, drop = FALSE]
    x$a <- list(NA_integer_, NA_real_)
    expect_error(merge(x, x))
    expect_no_error(merge(x, x, sort = FALSE))
    expect_no_error(df_join(x, x))
  })
  it("merges correctly if given a simple matrix key", {
    df <- data.frame(1:3)[, FALSE, drop = FALSE]
    df$a <- matrix(c(1:6, 1:3), nrow = 3, byrow = TRUE)
    expect_identical(nrow(unique(df)), 2L)
    expect_identical(nrow(df_unique(df)), 2L)
    expect_identical(
      df_join(df, df),
      `rownames<-`(df[rep(1:2, c(4, 1)), , drop = FALSE], NULL)
    )
  })
  it("doesn't match NAs of different classes in list columns", {
    x <- data.frame(a = 1:2)[, FALSE, drop = FALSE]
    x$a <- list(NA_integer_, NA_real_)
    expect_true(!df_anyDuplicated(x))
    expect_true(!df_anyDuplicated(df_join(x, x)))
  })
  it("doesn't match FALSE with 'FALSE' in list columns", {
    x <- data.frame(a = 1:2)[, FALSE, drop = FALSE]
    x$a <- list(FALSE, "FALSE")
    expect_true(!df_anyDuplicated(x))
    expect_true(!df_anyDuplicated(df_join(x, x)))
  })
  it("doesn't match numbers of different classes in list columns", {
    x <- data.frame(a = 1:2)[, FALSE, drop = FALSE]
    x$a <- list(1.0, 1L)
    expect_true(!df_anyDuplicated(x))
    expect_true(!df_anyDuplicated(df_join(x, x)))
  })
  it("matches a matrix with a vector if it has a single column", {
    x <- data.frame(a = 1:3)
    x$b <- matrix(1:6, nrow = 3)
    expect_error(df_join(x, x, by.x = "b", by.y = "a"))
    expect_error(df_join(x, x, by.x = "a", by.y = "b"))

    y <- data.frame(a = 1:3)
    y$b <- matrix(1:3, nrow = 3)
    expect_error(df_join(y, y, by.x = "a", by.y = "b"))
  })
})
