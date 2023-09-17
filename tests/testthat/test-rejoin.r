library(hedgehog)

describe("rejoin", {
  it("is left-inverse for lossless full-dep database creation, outside of row permutations, for tables with unique rows", {
    autodb_inverted_by_rejoin <- function(df) {
      database <- autodb(df)
      df2 <- rejoin(database)
      expect_identical_unordered_table(df2, df)
    }
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
})
