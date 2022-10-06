library(hedgehog)

describe("rejoin", {
  it("is left-inverse for lossless full-dep database creation, outside of row/column permutations, for tables with unique rows", {
    autonorm_inverted_by_rejoin <- function(df) {
      es <- autonorm(df, 1)
      df2 <- rejoin(es)
      expect_identical_unordered_table(df2, df)
    }
    # 6 columns allows for interesting cases, such as a table containing two
    # independent ones, or a reference involving several attributes
    forall(gen_df(6, 7, remove_dup_rows = TRUE), autonorm_inverted_by_rejoin)
  })
})
