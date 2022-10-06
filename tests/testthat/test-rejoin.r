library(hedgehog)

describe("rejoin", {
  it("is left-inverse for lossless full-dep database creation, outside of row/column permutations, for tables with unique rows", {
    # 6 columns allows for interesting cases, such as a table containing two
    # independent ones, or a reference involving several attributes
    gen_ncol_inc <- gen.int(7)
    gen_len_inc <- gen.int(6)
    gen_lst <- generate(
      for (n_col_inc in gen_ncol_inc) {
        generate(
          for (len_inc in gen_len_inc) {
            rep(
              list(gen.sample(c(FALSE, TRUE), len_inc - 1, replace = TRUE)),
              n_col_inc - 1
            ) |>
              setNames(make.unique(rep_len(LETTERS, n_col_inc - 1)))
          }
        )
      }
    )
    gen_df <- generate(for (lst in gen_lst) as.data.frame(lst))
    forall(
      gen_df,
      function(df) {
        df <- unique(df)
        es <- autonorm(df, 1)
        df2 <- rejoin(es)
        expect_identical_unordered_table(df2, df)
      }
    )
  })
})
