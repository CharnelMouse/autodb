library(hedgehog)

describe("rejoin", {
  it("is left-inverse for lossless full-dep database creation, outside of row permutations, for tables with unique rows", {
    autodb_inverted_by_rejoin <- function(df) {
      database <- autodb(df, 1)
      df2 <- rejoin(database)
      expect_identical_unordered_table(df2, df)
    }
    # 6 columns allows for interesting cases, such as a table containing two
    # independent ones, or a reference involving several attributes
    forall(
      gen_df(6, 7, remove_dup_rows = TRUE),
      autodb_inverted_by_rejoin,
      tests = 1000
    )
  })
})
