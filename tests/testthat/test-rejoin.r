library(hedgehog)

describe("rejoin", {
  it("is left-inverse for lossless full-dep database creation, outside of row/column permutations, for tables with unique rows", {
    # 6 columns allows for interesting cases, such as a table containing two
    # independent ones, or a reference involving several attributes
    forall(
      gen_df(6, 7),
      function(df) {
        df <- unique(df)
        es <- autonorm(df, 1)
        df2 <- rejoin(es)
        expect_identical_unordered_table(df2, df)
      }
    )
  })
})
