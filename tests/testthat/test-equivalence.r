describe("df_equiv", {
  it("considers data frames equivalent to themselves", {
    forall(
      gen_df(4, 6, remove_dup_rows = FALSE),
      dup %>>% uncurry(df_equiv) %>>% expect_true
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
      uncurry(df_equiv) %>>%
        expect_true
    )
  })
})
