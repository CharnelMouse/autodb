describe("df_equiv", {
  it("considers data frames equivalent to themselves", {
    forall(
      gen_df(4, 6, remove_dup_rows = FALSE),
      dup %>>% uncurry(with_args(df_equiv, digits = NA)) %>>% expect_true
    )
  })
  it("ignores row names", {
    forall(
      gen_df(4, 6, remove_dup_rows = TRUE) |>
        gen.and_then(\(df) {
          list(
            gen.pure(df),
            gen.sample(seq_len(nrow(df))) |>
              gen.with(\(rs) `rownames<-`(df[rs, , drop = FALSE], NULL))
          )
        }),
      uncurry(with_args(df_equiv, digits = NA)) %>>%
        expect_true
    )
  })
  it("compares floats to given number of significant digits, if not NA", {
    df1 <- data.frame(a = 1.23)
    df2 <- data.frame(a = 1.25)
    expect_false(df_equiv(df1, df2, digits = NA))
    expect_false(df_equiv(df1, df2, digits = 3))
    expect_true(df_equiv(df1, df2, digits = 2))
  })
})
