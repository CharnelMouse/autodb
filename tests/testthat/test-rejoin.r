library(hedgehog)

describe("rejoin", {
  it("is left-inverse for lossless full-dep database creation, outside of row permutations, for tables with unique rows", {
    autodb_inverted_by_rejoin <- expect_bi(
      df_equiv,
      with_args(autodb, ensure_lossless = TRUE) %>>%
        rejoin,
      identity
    )
    table_dum <- data.frame()
    table_dee <- data.frame(a = 1)[, -1, drop = FALSE]
    autodb_inverted_by_rejoin(table_dum)
    autodb_inverted_by_rejoin(table_dee)
    # 6 columns allows for interesting cases, such as a table containing two
    # independent ones, or a reference involving several attributes
    forall(
      gen_df(6, 7, remove_dup_rows = TRUE),
      autodb_inverted_by_rejoin,
      tests = 1000
    )
    forall(
      gen_df(6, 7, remove_dup_rows = FALSE),
      expect_bi(
        df_equiv,
        with_args(autodb, ensure_lossless = TRUE) %>>%
          rejoin,
        function(df) {
          if (ncol(df) == 0)
            df[
              rep(
                c(TRUE, FALSE),
                (nrow(df) > 0) * c(1, nrow(df) - 1)
              ),
              ,
              drop = FALSE
            ]
          else
            unique(df)
        }
      ),
      tests = 1000
    )
  })
  it("is possible for any database constructed from a data frame", {
    forall(
      gen.choice(0, 10) |>
        gen.list(of = 2) |>
        gen.and_then(uncurry(\(rows, cols) {
          gen_df(rows, cols, remove_dup_rows = TRUE)
        })) |>
        gen.with(with_args(autodb, ensure_lossless = TRUE)),
      rejoin %>>%
        with_args(try, silent = TRUE) %>>%
        with_args(expect_s3_class, class = "data.frame")
    )
  })
  it("handles databases with zero-column relations properly", {
    db <- database(
      relation(
        list(
          constants = list(
            df = data.frame(a = 1L)[, FALSE, drop = FALSE],
            keys = list(character())
          ),
          a = list(df = data.frame(a = 1L), keys = list("a"))
        ),
        "a"
      ),
      list()
    )
    expect_identical(rejoin(db), data.frame(a = 1L))
  })
})
